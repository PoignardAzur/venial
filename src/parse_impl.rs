use crate::parse_fn::consume_fn;
use crate::parse_type::{consume_generic_params, consume_where_clause};
use crate::parse_utils::{
    consume_ident, consume_inner_attributes, consume_outer_attributes, consume_stuff_until,
    consume_vis_marker, parse_any_ident, parse_ident,
};
use crate::types::{Constant, ImplMember, TyDefinition, ValueExpr};
use crate::types_edition::GroupSpan;
use crate::{Attribute, Declaration, Impl, TyExpr, VisMarker};
use proc_macro2::{Delimiter, Group, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn parse_const_or_static(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Constant {
    let (tk_const_or_static, is_const) = match tokens.next() {
        Some(TokenTree::Ident(ident)) if ident == "const" => (ident, true),
        Some(TokenTree::Ident(ident)) if ident == "static" => (ident, false),
        _ => panic!("cannot parse const/static"),
    };

    let tk_mut = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "mut" => {
            assert!(!is_const, "`const mut` is not a valid declaration");
            Some(ident.clone())
        }
        _ => None,
    };

    let name = parse_any_ident(tokens, "const/static");

    let tk_colon = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct,
        _ => panic!("cannot parse {}", tk_const_or_static),
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
        _ => panic!("cannot parse {}", tk_const_or_static),
    };

    Constant {
        attributes,
        vis_marker,
        tk_const_or_static,
        tk_mut,
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
    let tk_type = parse_ident(tokens, "type");
    let name = parse_any_ident(tokens, "associated type");

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
) -> Declaration {
    if let Some(TokenTree::Ident(ident)) = tokens.peek() {
        let keyword = ident.to_string();
        match keyword.as_str() {
            "type" => {
                let assoc_ty = consume_ty_definition(tokens, attributes, vis_marker);
                Declaration::TyDefinition(assoc_ty.unwrap())
            }
            "default" | "const" | "async" | "unsafe" | "extern" | "fn" => {
                if let Some(method) = consume_fn(tokens, attributes.clone(), vis_marker.clone()) {
                    Declaration::Function(method)
                } else if keyword == "const" {
                    let constant = parse_const_or_static(tokens, attributes, vis_marker);
                    Declaration::Constant(constant)
                } else if keyword == "unsafe" {
                    let impl_decl = parse_impl(tokens, attributes);
                    Declaration::Impl(impl_decl)
                } else {
                    unreachable!();
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

        let attributes = consume_outer_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);
        let item = match consume_fn_const_or_type(&mut tokens, attributes, vis_marker, "impl") {
            Declaration::Function(function) => ImplMember::Method(function),
            Declaration::Constant(constant) => ImplMember::Constant(constant),
            Declaration::TyDefinition(ty_def) => ImplMember::AssocTy(ty_def),
            _ => panic!("unsupported impl item `{:?}`", tokens.peek()),
        };

        body_items.push(item);
    }

    (GroupSpan::new(&token_group), inner_attributes, body_items)
}

pub(crate) fn parse_impl(tokens: &mut TokenIter, attributes: Vec<Attribute>) -> Impl {
    let tk_unsafe = consume_ident(tokens, "unsafe");
    let tk_impl = parse_ident(tokens, "impl");

    let impl_generic_params = consume_generic_params(tokens);
    let trait_or_self_ty = consume_stuff_until(
        tokens,
        |tk| match tk {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
            TokenTree::Ident(ident) if ident == "for" || ident == "where" => true,
            _ => false,
        },
        true,
    );

    let (tk_for, trait_ty, self_ty) = if let Some(tk_for) = consume_ident(tokens, "for") {
        let self_ty = consume_stuff_until(
            tokens,
            |tk| match tk {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
                TokenTree::Ident(ident) if ident == "where" => true,
                _ => false,
            },
            true,
        );

        (
            Some(tk_for),
            Some(TyExpr {
                tokens: trait_or_self_ty,
            }),
            TyExpr { tokens: self_ty },
        )
    } else {
        (
            None,
            None,
            TyExpr {
                tokens: trait_or_self_ty,
            },
        )
    };

    let where_clause = consume_where_clause(tokens);

    let (tk_braces, inner_attributes, body_items) = match tokens.next().unwrap() {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => parse_impl_body(group),
        token => panic!("cannot parse impl: unexpected token {:?}", token),
    };

    Impl {
        attributes,
        tk_unsafe,
        tk_impl,
        impl_generic_params,
        trait_ty,
        tk_for,
        self_ty,
        where_clause,
        body_items,
        inner_attributes,
        tk_braces,
    }
}
