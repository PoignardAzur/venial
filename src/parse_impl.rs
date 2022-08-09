use crate::parse_fn::consume_fn;
use crate::parse_utils::{
    consume_attributes, consume_inner_attributes, consume_stuff_until, consume_vis_marker,
};
use crate::types::{Constant, ImplMember, TyDefinition, ValueExpr};
use crate::types_edition::GroupSpan;
use crate::{Attribute, TyExpr, VisMarker};
use proc_macro2::{Group, Ident, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn consume_for(tokens: &mut TokenIter) -> Option<Ident> {
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "for" => {
            let tk_for = ident.clone();
            tokens.next();
            Some(tk_for)
        }
        _ => None,
    }
}

pub(crate) fn consume_constant(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Constant {
    let tk_const = match tokens.next() {
        Some(TokenTree::Ident(ident)) if ident == "const" => ident,
        _ => panic!("cannot parse constant"),
    };

    let name = match tokens.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("cannot parse constant"),
    };

    let tk_colon = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct,
        _ => panic!("cannot parse constant"),
    };

    let ty_tokens = consume_stuff_until(
        tokens,
        |tt| matches!(tt, TokenTree::Punct(punct) if punct.as_char() == '=' || punct.as_char() == ';'),
        true,
    );

    let tk_equals = match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {
            let punct = punct.clone();
            tokens.next();
            Some(punct)
        }
        _ => None,
    };

    let value_tokens = consume_stuff_until(
        tokens,
        |tt| matches!(tt, TokenTree::Punct(punct) if punct.as_char() == ';'),
        true,
    );
    let initializer = if value_tokens.is_empty() {
        None
    } else {
        Some(ValueExpr {
            tokens: value_tokens,
        })
    };

    let tk_semicolon = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => punct,
        _ => panic!("cannot parse constant"),
    };

    Constant {
        attributes,
        vis_marker,
        tk_const,
        name,
        tk_colon,
        ty: TyExpr { tokens: ty_tokens },
        tk_equals,
        initializer,
        tk_semicolon,
    }
}

pub(crate) fn consume_ty_definition(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Option<TyDefinition> {
    let tk_type = match tokens.next() {
        Some(TokenTree::Ident(ident)) if ident == "type" => ident,
        _ => {
            return None;
        }
    };

    let name = match tokens.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("cannot parse associated type"),
    };

    let tk_equals = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => punct,
        _ => panic!("cannot parse associated type"),
    };

    let ty_tokens = consume_stuff_until(
        tokens,
        |tt| matches!(tt, TokenTree::Punct(punct) if punct.as_char() == ';'),
        true,
    );

    let tk_semicolon = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => punct,
        _ => panic!("cannot parse associated type"),
    };

    Some(TyDefinition {
        attributes,
        vis_marker,
        tk_type,
        name,
        tk_equals,
        initializer_ty: TyExpr { tokens: ty_tokens },
        tk_semicolon,
    })
}

pub(crate) fn consume_fn_const_or_type(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
    context: &str, // for panic
) -> ImplMember {
    if let Some(TokenTree::Ident(ident)) = tokens.peek() {
        match ident.to_string().as_str() {
            "type" => {
                let assoc_ty = consume_ty_definition(tokens, attributes, vis_marker);
                ImplMember::AssocTy(assoc_ty.unwrap())
            }
            "default" | "const" | "async" | "unsafe" | "extern" | "fn" => {
                if let Some(method) = consume_fn(tokens, attributes.clone(), vis_marker.clone()) {
                    ImplMember::Method(method)
                } else {
                    let constant = consume_constant(tokens, attributes, vis_marker);
                    ImplMember::Constant(constant)
                }
            }
            _ => panic!("unsupported {} item `{}`", context, ident),
        }
    } else {
        panic!("unsupported {} element: {:?}", context, tokens.peek())
    }
}

pub(crate) fn parse_impl_body(token_group: Group) -> (GroupSpan, Vec<Attribute>, Vec<ImplMember>) {
    let mut body_items = vec![];

    let mut tokens = token_group.stream().into_iter().peekable();
    let inner_attributes = consume_inner_attributes(&mut tokens);
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);
        let item = consume_fn_const_or_type(&mut tokens, attributes, vis_marker, "impl");

        body_items.push(item);
    }

    (GroupSpan::new(&token_group), inner_attributes, body_items)
}
