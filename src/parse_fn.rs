use crate::parse_type::{
    consume_declaration_name, consume_field_type, consume_generic_params, consume_where_clause,
};
use crate::parse_utils::{
    consume_comma, consume_ident, consume_outer_attributes, consume_punct, consume_stuff_until,
    parse_any_ident, parse_punct,
};
use crate::punctuated::Punctuated;
use crate::types::{
    FnParam, FnQualifiers, FnReceiverParam, FnTypedParam, Function, GroupSpan, TyExpr,
};
use crate::{Attribute, VisMarker};
use proc_macro2::{Delimiter, Ident, Punct, TokenStream, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

/// If venial fails to parse the declaration as a function, it can detect that it
/// is either a constant (`const` ambiguity), an impl or a module (`unsafe` ambiguity).
pub(crate) enum NotFunction {
    Const,
    Impl,
    Mod,
}

pub(crate) fn consume_fn_qualifiers(tokens: &mut TokenIter) -> FnQualifiers {
    let tk_default = consume_ident(tokens, "default");
    let tk_const = consume_ident(tokens, "const");
    let tk_async = consume_ident(tokens, "async");
    let tk_unsafe = consume_ident(tokens, "unsafe");

    let tk_extern;
    let extern_abi;
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "extern" => {
            tk_extern = Some(ident.clone());
            tokens.next();

            match tokens.peek() {
                Some(TokenTree::Literal(literal)) => {
                    extern_abi = Some(literal.clone());
                    tokens.next();
                }
                _ => {
                    extern_abi = None;
                }
            }
        }
        _ => {
            tk_extern = None;
            extern_abi = None;
        }
    };

    FnQualifiers {
        tk_default,
        tk_const,
        tk_async,
        tk_unsafe,
        tk_extern,
        extern_abi,
    }
}

fn parse_fn_params(tokens: TokenStream) -> Punctuated<FnParam> {
    let mut fields = Punctuated::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }
        let attributes = consume_outer_attributes(&mut tokens);

        let tk_ref = consume_punct(&mut tokens, '&');
        let tk_mut = consume_ident(&mut tokens, "mut");
        let tk_self = consume_ident(&mut tokens, "self");

        let param = if let Some(tk_self) = tk_self {
            FnParam::Receiver(FnReceiverParam {
                attributes,
                tk_ref,
                tk_mut,
                tk_self,
            })
        } else {
            // TODO - handle non-ident argument names
            let param_name = parse_any_ident(&mut tokens, "fn param name");
            let tk_colon = parse_punct(&mut tokens, ':', "fn params");

            let ty_tokens = consume_field_type(&mut tokens);
            FnParam::Typed(FnTypedParam {
                attributes,
                tk_mut,
                name: param_name,
                tk_colon,
                ty: TyExpr { tokens: ty_tokens },
            })
        };

        let comma = consume_comma(&mut tokens);

        fields.push(param, comma);
    }

    fields
}

fn consume_fn_return(tokens: &mut TokenIter) -> Option<([Punct; 2], TyExpr)> {
    let dash = consume_punct(tokens, '-')?;

    let tip = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => punct,
        _ => panic!("cannot parse fn return: expected '>' after '-' token"),
    };

    Some((
        [dash, tip],
        TyExpr {
            tokens: (consume_stuff_until(
                tokens,
                |token| match token {
                    TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
                    TokenTree::Ident(i) if i == &Ident::new("where", i.span()) => true,
                    TokenTree::Punct(punct) if punct.as_char() == ';' => true,
                    _ => false,
                },
                true,
            )),
        },
    ))
}

/// Tries to parse a function definition.
///
/// Panics when the following tokens do not constitute a function definition, with one exception:
/// when `const` is followed by an identifier which is not `fn`, then `None` is returned. This is to
/// allow fallback to a constant declaration (both can begin with the `const` token).
pub(crate) fn consume_fn(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Result<Function, NotFunction> {
    // TODO consider multiple-lookahead instead of potentially cloning many tokens
    let before_start = tokens.clone();
    let qualifiers = consume_fn_qualifiers(tokens);

    // fn keyword, or const fallback
    let next_token = tokens.next();
    let tk_fn_keyword = if let Some(TokenTree::Ident(ident)) = &next_token {
        if ident == "fn" {
            ident.clone()
        } else if qualifiers.has_only_const_xor_unsafe() {
            // This is not a function, detect what else it is
            // Note: detection already done here, because then we only need the lookahead/rollback once
            let declaration_type = if qualifiers.tk_const.is_some() {
                NotFunction::Const
            } else if qualifiers.tk_unsafe.is_some() {
                match &next_token {
                    Some(TokenTree::Ident(ident)) if ident == "impl" => NotFunction::Impl,
                    Some(TokenTree::Ident(ident)) if ident == "mod" => NotFunction::Mod,
                    token => panic!(
                        "expected 'fn', 'impl' or 'mod' after 'unsafe', got {:?}",
                        token
                    ),
                }
            } else {
                unreachable!()
            };

            // rollback iterator, could be start of const declaration
            *tokens = before_start;
            return Err(declaration_type);
        } else {
            panic!("expected 'fn' keyword, got ident '{}'", ident)
        }
    } else {
        panic!("expected 'fn' keyword")
    };

    let fn_name = consume_declaration_name(tokens);
    let generic_params = consume_generic_params(tokens);

    let (params, tk_params_parens) = match tokens.next().unwrap() {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
            (parse_fn_params(group.stream()), GroupSpan::new(&group))
        }
        _ => panic!("cannot parse function"),
    };

    let (tk_return_arrow, return_ty) = if let Some((arrow, ty)) = consume_fn_return(tokens) {
        (Some(arrow), Some(ty))
    } else {
        (None, None)
    };

    let where_clause = consume_where_clause(tokens);

    let (function_body, tk_semicolon) = match &tokens.next().unwrap() {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
            (Some(group.clone()), None)
        }
        TokenTree::Punct(punct) if punct.as_char() == ';' => (None, Some(punct.clone())),
        _ => panic!("cannot parse function"),
    };

    Ok(Function {
        attributes,
        vis_marker,
        qualifiers,
        tk_fn_keyword,
        name: fn_name,
        generic_params,
        tk_params_parens,
        params,
        where_clause,
        tk_return_arrow,
        return_ty,
        tk_semicolon,
        body: function_body,
    })
}
