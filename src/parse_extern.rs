use crate::parse_impl::parse_impl_body;
use crate::parse_utils::{consume_ident, parse_any_ident, parse_ident, parse_punct, TokenIter};
use crate::{Attribute, ExternBlock, ExternCrate, VisMarker};
use proc_macro2::{Delimiter, TokenTree};

pub(crate) fn parse_extern_crate(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> ExternCrate {
    consume_extern_crate(tokens, attributes, vis_marker).expect("cannot parse extern crate")
}

fn consume_extern_crate(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Option<ExternCrate> {
    let tk_extern = consume_ident(tokens, "extern")?;
    let tk_crate = consume_ident(tokens, "crate")?;

    let name = parse_any_ident(tokens, "extern crate");
    let tk_as = consume_ident(tokens, "as");

    let alias;
    let tk_underscore;
    if tk_as.is_some() {
        alias = Some(parse_any_ident(tokens, "extern crate: alias"));
        if alias.is_none() {
            tk_underscore = Some(parse_ident(tokens, "_", "extern crate"));
        } else {
            tk_underscore = None;
        }
    } else {
        alias = None;
        tk_underscore = None;
    }

    let tk_semicolon = parse_punct(tokens, ';', "extern crate");

    Some(ExternCrate {
        attributes,
        vis_marker,
        tk_extern,
        tk_crate,
        name,
        tk_as,
        alias,
        tk_underscore,
        tk_semicolon,
    })
}

pub(crate) fn parse_extern_block(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> ExternBlock {
    consume_extern_block(tokens, attributes, vis_marker).expect("cannot parse extern block")
}

fn consume_extern_block(
    tokens: &mut TokenIter,
    attributes: Vec<Attribute>,
    vis_marker: Option<VisMarker>,
) -> Option<ExternBlock> {
    let tk_unsafe = consume_ident(tokens, "unsafe");
    let tk_extern = consume_ident(tokens, "extern")?;

    let extern_abi = match tokens.peek() {
        Some(TokenTree::Literal(lit)) => {
            let lit = Some(lit.clone());
            tokens.next();
            lit
        }
        _ => None,
    };

    let (tk_braces, inner_attributes, body_items) = match tokens.next() {
        Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
            parse_impl_body(group, true)
        }
        _ => {
            // Only here we know that it's not an extern crate or extern block, so try other options on call-site (fn).
            return None;
        }
    };

    Some(ExternBlock {
        attributes,
        vis_marker,
        tk_unsafe,
        tk_extern,
        extern_abi,
        tk_braces,
        inner_attributes,
        body_items,
    })
}
