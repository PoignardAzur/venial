use crate::parse_type::consume_field_type;
use crate::parse_utils::{consume_attributes, consume_comma, consume_stuff_until, parse_ident};
use crate::punctuated::Punctuated;
use crate::types::{FunctionParameter, FunctionQualifiers, TyExpr};
use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
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

pub(crate) fn consume_fn_return(tokens: &mut TokenIter) -> Option<TyExpr> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '-' => (),
        _ => return None,
    };
    let _dash = tokens.next().unwrap();

    match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => (),
        _ => panic!("cannot parse fn return: expected '>' after '-' token"),
    };

    Some(TyExpr {
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
    })
}

pub(crate) fn parse_fn_params(tokens: TokenStream) -> Punctuated<FunctionParameter> {
    let mut fields = Punctuated::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }
        let attributes = consume_attributes(&mut tokens);

        // TODO - handle non-ident argument names
        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        // TODO - Handle self parameter
        match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => (),
            _ => panic!("cannot parse fn params"),
        };

        let ty_tokens = consume_field_type(&mut tokens);
        let comma = consume_comma(&mut tokens);

        fields.push(
            FunctionParameter {
                attributes,
                name: ident,
                ty: TyExpr { tokens: ty_tokens },
            },
            comma,
        );
    }

    fields
}
