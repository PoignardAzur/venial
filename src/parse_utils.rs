use crate::types::{Attribute, VisMarker};
use crate::types_edition::GroupSpan;
use proc_macro2::{Delimiter, Ident, Punct, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn parse_ident(token: TokenTree) -> Result<Ident, TokenTree> {
    match token {
        TokenTree::Group(_) => Err(token),
        TokenTree::Ident(ident) => Ok(ident),
        TokenTree::Punct(_) => Err(token),
        TokenTree::Literal(_) => Err(token),
    }
}

pub(crate) fn consume_attributes(tokens: &mut TokenIter) -> Vec<Attribute> {
    let mut attributes = Vec::new();

    loop {
        let hashbang = match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => punct.clone(),
            _ => break,
        };
        tokens.next();

        let group = match tokens.next() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Bracket => group,
            _ => panic!("cannot parse attribute: expected '[' after '#' token"),
        };

        let tk_braces = GroupSpan::new(&group);
        let mut attribute_tokens = group.stream().into_iter().peekable();

        let mut path = Vec::new();
        loop {
            match attribute_tokens.peek() {
                None => break,
                Some(TokenTree::Group(_)) => break,
                Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => break,
                Some(TokenTree::Ident(_)) => (),
                Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => (),
                Some(token) => panic!("cannot parse attribute: expected one of `(`, `::`, `=`, `[`, `]`, or `{{`, found {:?}", token),
            };
            path.push(attribute_tokens.next().unwrap());
        }

        let mut tk_equals = None;
        let mut tk_group = None;
        let mut value = None;
        match attribute_tokens.peek() {
            None => (),
            Some(TokenTree::Group(group)) => {
                tk_group = Some(GroupSpan::new(group));
                value = Some(group.stream().into_iter().collect());
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {
                tk_equals = Some(punct.clone());
                attribute_tokens.next();
                value = Some(attribute_tokens.into_iter().collect());
            }
            _ => unreachable!(),
        };

        attributes.push(Attribute {
            tk_hashbang: hashbang,
            tk_braces,
            path,
            tk_equals,
            tk_group,
            value,
        });
    }

    attributes
}

pub(crate) fn consume_vis_marker(tokens: &mut TokenIter) -> Option<VisMarker> {
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "pub" => {
            let pub_token = tokens
                .next()
                .expect("cannot parse declaration: expected token after 'pub'");
            match tokens.peek() {
                Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                    Some(VisMarker {
                        tk_token1: pub_token,
                        tk_token2: Some(tokens.next().unwrap()),
                    })
                }
                _ => Some(VisMarker {
                    tk_token1: pub_token,
                    tk_token2: None,
                }),
            }
        }
        Some(TokenTree::Ident(ident)) if ident == "crate" => Some(VisMarker {
            tk_token1: tokens.next().unwrap(),
            tk_token2: None,
        }),
        _ => None,
    }
}

// Consumes tokens until a separator is reached *unless* the
// separator in between angle brackets
// eg consume_stuff_until(..., |token| token == ',') will consume all
// of `Foobar<A, B>,` except for the last comma
pub(crate) fn consume_stuff_until(
    tokens: &mut TokenIter,
    predicate: impl FnMut(&TokenTree) -> bool,
) -> Vec<TokenTree> {
    let mut output_tokens = Vec::new();
    let mut bracket_count = 0;
    let mut predicate = predicate;
    let mut prev_token_is_dash = false;

    loop {
        let token = tokens.peek();
        prev_token_is_dash = match &token {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                bracket_count += 1;
                false
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == '>' && !prev_token_is_dash => {
                bracket_count -= 1;
                false
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == '-' => true,
            Some(token) if predicate(token) && bracket_count == 0 => {
                break;
            }
            None => {
                break;
            }
            _ => false,
        };

        assert!(bracket_count >= 0, "Unbalanced brackets in type expression");

        output_tokens.push(tokens.next().unwrap());
    }

    output_tokens
}

pub(crate) fn consume_comma(tokens: &mut TokenIter) -> Option<Punct> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
            let punct = punct.clone();
            tokens.next().unwrap();
            Some(punct)
        }
        _ => None,
    }
}
