use crate::parse_extern::{parse_extern_block, parse_extern_crate};
use crate::parse_fn::{consume_fn, consume_macro, NotFunction};
use crate::parse_mod::parse_mod;
use crate::parse_type::{consume_bound, consume_generic_params, consume_where_clause};
use crate::parse_utils::{
    consume_ident, consume_inner_attributes, consume_outer_attributes, consume_punct,
    consume_stuff_until, consume_vis_marker, parse_any_ident, parse_ident, parse_punct,
};
use crate::types::{Constant, ImplMember, TypeAlias, ValueExpr};
use crate::types_edition::GroupSpan;
use crate::{Attribute, Item, Impl, Trait, TraitMember, TyExpr, VisMarker};
use proc_macro2::{Delimiter, Group, TokenTree};
use quote::ToTokens;
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
    let tk_colon = parse_punct(tokens, ':', "const/static");

    let ty_tokens = consume_stuff_until(
        tokens,
        |tt| matches!(tt, TokenTree::Punct(punct) if punct.as_char() == '=' || punct.as_char() == ';'),
        true,
    );

    let tk_equals = consume_punct(tokens, '=');

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

    let tk_semicolon = parse_punct(tokens, ';', "const/static");

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
) -> Option<TypeAlias> {
    let context = "associated type";
    let tk_type = parse_ident(tokens, "type", context);
    let name = parse_any_ident(tokens, context);

    let bound = consume_bound(tokens, |token| {
        matches!(
            token,
            TokenTree::Punct(punct) if punct.as_char() == '=' || punct.as_char() == ';'
        )
    });

    let tk_equals = consume_punct(tokens, '=');

    let initializer_ty = if tk_equals.is_some() {
        let ty_tokens = consume_stuff_until(
            tokens,
            |tt| matches!(tt, TokenTree::Punct(punct) if punct.as_char() == ';'),
            true,
        );
        Some(TyExpr { tokens: ty_tokens })
    } else {
        None
    };

    let tk_semicolon = parse_punct(tokens, ';', context);

    Some(TypeAlias {
        attributes,
        vis_marker,
        tk_type,
        name,
        bound,
        tk_equals,
        initializer_ty,
        tk_semicolon,
    })
}

// TODO could accept a mask, allowing only certain sub-items. This could be made to reject e.g. extern crate in impl blocks.
pub(crate) fn consume_either_fn_type_const_static_impl(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
    context: &str, // for panic
) -> Item {
    if let Some(TokenTree::Ident(ident)) = tokens.peek() {
        let keyword = ident.to_string();
        match keyword.as_str() {
            "type" => {
                let assoc_ty = consume_ty_definition(tokens, attributes, vis_marker);
                Item::TypeAlias(assoc_ty.unwrap())
            }

            // Note: `static` is only used for extern "abi" {} blocks. Checked in call site.
            "default" | "const" | "static" | "async" | "unsafe" | "extern" | "fn" => {
                match consume_fn(tokens, attributes.clone(), vis_marker.clone()) {
                    Ok(method) => Item::Function(method),
                    Err(NotFunction::Const) => {
                        let constant = parse_const_or_static(tokens, attributes, vis_marker);
                        Item::Constant(constant)
                    }
                    Err(NotFunction::Static) => {
                        let static_decl = parse_const_or_static(tokens, attributes, vis_marker);
                        Item::Constant(static_decl)
                    }
                    Err(NotFunction::Trait) => {
                        let trait_decl = parse_trait(tokens, attributes, vis_marker);
                        Item::Trait(trait_decl)
                    }
                    Err(NotFunction::Impl) => {
                        let impl_decl = parse_impl(tokens, attributes);
                        Item::Impl(impl_decl)
                    }
                    Err(NotFunction::Mod) => {
                        let mod_decl = parse_mod(tokens, attributes, vis_marker);
                        Item::Module(mod_decl)
                    }
                    Err(NotFunction::ExternBlock) => {
                        let extern_decl = parse_extern_block(tokens, attributes, vis_marker);
                        Item::ExternBlock(extern_decl)
                    }
                    Err(NotFunction::ExternCrate) => {
                        let crate_decl = parse_extern_crate(tokens, attributes, vis_marker);
                        Item::ExternCrate(crate_decl)
                    }
                }
            }
            ident => {
                if let Some(vis_marker) = vis_marker {
                    panic!(
                        "unsupported visibility marker `{}`",
                        vis_marker.to_token_stream()
                    );
                }
                match consume_macro(tokens, attributes.clone()) {
                    Some(macro_) => Item::Macro(macro_),
                    None => panic!("unsupported {} item `{}`", context, ident),
                }
            }
        }
    } else {
        panic!("unsupported {} element: {:?}", context, tokens.peek())
    }
}

pub(crate) fn parse_impl_body(
    token_group: Group,
    allow_static: bool,
) -> (GroupSpan, Vec<Attribute>, Vec<ImplMember>) {
    let mut body_items = vec![];

    let mut tokens = token_group.stream().into_iter().peekable();
    let inner_attributes = consume_inner_attributes(&mut tokens);
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_outer_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);
        let item = match consume_either_fn_type_const_static_impl(
            &mut tokens,
            attributes,
            vis_marker,
            "impl",
        ) {
            Item::Function(function) => ImplMember::Method(function),
            Item::Constant(const_) if const_.tk_const_or_static == "const" => {
                // `const` can appear in impl/trait blocks.
                ImplMember::Constant(const_)
            }
            Item::Constant(static_) if allow_static => {
                // `static` can appear in `extern "abi" {}` blocks.
                ImplMember::Constant(static_)
            }
            Item::TypeAlias(ty_def) => ImplMember::AssocTy(ty_def),
            Item::Macro(macro_) => ImplMember::Macro(macro_),
            _ => panic!("unsupported impl item `{:?}`", tokens.peek()),
        };

        body_items.push(item);
    }

    (GroupSpan::new(&token_group), inner_attributes, body_items)
}

pub(crate) fn parse_impl(tokens: &mut TokenIter, attributes: Vec<Attribute>) -> Impl {
    let tk_unsafe = consume_ident(tokens, "unsafe");
    let tk_impl = parse_ident(tokens, "impl", "impl block");

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
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
            parse_impl_body(group, false)
        }
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

pub(crate) fn parse_trait(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Trait {
    let tk_unsafe = consume_ident(tokens, "unsafe");
    let tk_trait = parse_ident(tokens, "trait", "trait declaration");
    let name = parse_any_ident(tokens, "trait name");
    let generic_params = consume_generic_params(tokens);
    let bound = consume_bound(tokens, |token| match token {
        TokenTree::Ident(ident) if ident == "where" => true,
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
        _ => false,
    });
    let where_clause = consume_where_clause(tokens);

    // For trait body, at the moment reuse impl parsing
    let (tk_braces, inner_attributes, body_items) = match tokens.next().unwrap() {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
            parse_impl_body(group, false)
        }
        token => panic!("cannot parse trait: unexpected token {:?}", token),
    };

    let body_items = body_items
        .into_iter()
        .map(|item| match item {
            ImplMember::Method(function) => TraitMember::Method(function),
            ImplMember::Constant(constant) => TraitMember::Constant(constant),
            ImplMember::AssocTy(assoc_ty) => TraitMember::AssocTy(assoc_ty),
            ImplMember::Macro(macro_) => TraitMember::Macro(macro_),
        })
        .collect();

    Trait {
        attributes,
        vis_marker,
        tk_unsafe,
        tk_trait,
        name,
        generic_params,
        bound,
        where_clause,
        body_items,
        inner_attributes,
        tk_braces,
    }
}
