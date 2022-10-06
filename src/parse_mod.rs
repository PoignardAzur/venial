use crate::parse::parse_declaration_tokens;
use crate::parse_type::consume_declaration_name;
use crate::parse_utils::{
    consume_ident, consume_inner_attributes, consume_stuff_until, parse_ident, parse_punct,
    TokenIter,
};
use crate::{Attribute, GroupSpan, Module, TyExpr, UseDeclaration, VisMarker};
use proc_macro2::token_stream::IntoIter;
use proc_macro2::{Delimiter, TokenTree};
use std::iter::Peekable;

pub(crate) fn parse_mod(
    tokens: &mut Peekable<IntoIter>,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Module {
    // TODO some items currently unsupported: decl-macros, extern crate

    let tk_unsafe = consume_ident(tokens, "unsafe");
    let tk_mod = parse_ident(tokens, "mod", "module declaration");
    let module_name = consume_declaration_name(tokens);

    let (group, tk_semicolon) = match tokens.next().unwrap() {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => (Some(group), None),
        TokenTree::Punct(punct) if punct.as_char() == ';' => (None, Some(punct)),
        token => panic!(
            "cannot parse mod: expected `{{ }}` or `;`, but got token {:?}",
            token
        ),
    };

    let inner_attributes;
    let tk_braces;
    let members;
    if let Some(group) = group {
        // Parse mod block body
        let mut mod_members = vec![];
        let mut tokens = group.stream().into_iter().peekable();

        tk_braces = Some(GroupSpan::new(&group));
        inner_attributes = consume_inner_attributes(&mut tokens);
        loop {
            if tokens.peek().is_none() {
                break;
            }
            let item = parse_declaration_tokens(&mut tokens)
                .unwrap_or_else(|e| panic!("declaration in mod: {}", e));
            mod_members.push(item);
        }
        members = mod_members;
    } else {
        tk_braces = None;
        inner_attributes = vec![];
        members = vec![];
    }

    Module {
        attributes,
        vis_marker,
        tk_unsafe,
        tk_mod,
        name: module_name,
        tk_semicolon,
        tk_braces,
        inner_attributes,
        members,
    }
}

pub(crate) fn parse_use_declaration(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> UseDeclaration {
    let tk_use = parse_ident(tokens, "use", "use declaration");

    let import_tree = consume_stuff_until(
        tokens,
        |token| match token {
            TokenTree::Punct(punct) if punct.as_char() == ';' => true,
            _ => false,
        },
        true,
    );

    let tk_semicolon = parse_punct(tokens, ';', "use declaration");

    UseDeclaration {
        attributes,
        vis_marker,
        tk_use,
        import_tree: TyExpr {
            tokens: import_tree,
        },
        tk_semicolon,
    }
}
