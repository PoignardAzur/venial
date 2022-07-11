use crate::parse_type::{
    consume_declaration_name, consume_field_type, consume_generic_params, consume_where_clause,
};
use crate::parse_utils::{consume_attributes, consume_comma, consume_stuff_until, parse_ident};
use crate::punctuated::Punctuated;
use crate::types::{
    Function, FunctionParameter, FunctionQualifiers, FunctionReceiverParameter,
    FunctionTypedParameter, GroupSpan, TyExpr,
};
use crate::{Attribute, VisMarker};
use proc_macro2::{Delimiter, Ident, Punct, TokenStream, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn consume_fn_qualifiers(tokens: &mut TokenIter) -> FunctionQualifiers {
    let tk_default = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "default" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    let tk_const = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "const" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    let tk_async = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "async" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    let tk_unsafe = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "unsafe" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };

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

    FunctionQualifiers {
        tk_default,
        tk_const,
        tk_async,
        tk_unsafe,
        tk_extern,
        extern_abi,
    }
}

pub(crate) fn parse_fn_params(tokens: TokenStream) -> Punctuated<FunctionParameter> {
    let mut fields = Punctuated::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }
        let attributes = consume_attributes(&mut tokens);

        let tk_ref = match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '&' => {
                let ref_symbol = punct.clone();
                tokens.next();
                Some(ref_symbol)
            }
            _ => None,
        };
        let tk_mut = match tokens.peek() {
            Some(TokenTree::Ident(ident)) if ident == "mut" => {
                let mut_ident = ident.clone();
                tokens.next();
                Some(mut_ident)
            }
            _ => None,
        };
        let tk_self = match tokens.peek() {
            Some(TokenTree::Ident(ident)) if ident == "self" => {
                let self_ident = ident.clone();
                tokens.next();
                Some(self_ident)
            }
            _ => None,
        };

        let param = if let Some(tk_self) = tk_self {
            FunctionParameter::Receiver(FunctionReceiverParameter {
                attributes,
                tk_ref,
                tk_mut,
                tk_self,
            })
        } else {
            // TODO - handle non-ident argument names
            let ident = parse_ident(tokens.next().unwrap()).unwrap();
            let tk_colon = match tokens.next() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct.clone(),
                _ => panic!("cannot parse fn params"),
            };
            let ty_tokens = consume_field_type(&mut tokens);
            FunctionParameter::Typed(FunctionTypedParameter {
                attributes,
                tk_mut,
                name: ident,
                tk_colon,
                ty: TyExpr { tokens: ty_tokens },
            })
        };

        let comma = consume_comma(&mut tokens);

        fields.push(param, comma);
    }

    fields
}

pub(crate) fn consume_fn_return(tokens: &mut TokenIter) -> Option<([Punct; 2], TyExpr)> {
    let dash = match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '-' => punct.clone(),
        _ => return None,
    };
    tokens.next().unwrap();

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

pub(crate) fn consume_fn(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Function {
    let qualifiers = consume_fn_qualifiers(tokens);

    // fn keyword
    let tk_fn_keyword = if let Some(TokenTree::Ident(fn_keyword)) = tokens.next() {
        fn_keyword
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

    Function {
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
    }
}
