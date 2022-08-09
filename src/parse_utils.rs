use crate::parse_type::consume_generic_args;
use crate::types::{Attribute, AttributeValue, Path, PathSegment, VisMarker};
use crate::types_edition::GroupSpan;
use proc_macro2::{Delimiter, Ident, Punct, Spacing, TokenStream, TokenTree};
use std::iter::Peekable;

pub(crate) type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn tokens_from_slice(slice: &[TokenTree]) -> TokenIter {
    let stream = TokenStream::from_iter(slice.iter().cloned());
    stream.into_iter().peekable()
}

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

        let value = match attribute_tokens.peek() {
            None => AttributeValue::Empty,
            Some(TokenTree::Group(group)) => {
                let tk_group = GroupSpan::new(group);
                let value = group.stream().into_iter().collect();
                AttributeValue::Group(tk_group, value)
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {
                let tk_equals = punct.clone();
                attribute_tokens.next();
                let value = attribute_tokens.into_iter().collect();
                AttributeValue::Equals(tk_equals, value)
            }
            _ => unreachable!(),
        };

        attributes.push(Attribute {
            tk_hashbang: hashbang,
            tk_brackets: tk_braces,
            path,
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
    must_find_predicate: bool,
) -> Vec<TokenTree> {
    let mut output_tokens = Vec::new();
    let mut bracket_count = 0;
    let mut predicate = predicate;
    let mut predicate_met = false;
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
                predicate_met = true;
                break;
            }
            None => {
                break;
            }
            _ => false,
        };

        // If we imagine angle brackets as a token group, this is equivalent to reaching
        // the end of the group's token stream.
        if bracket_count < 0 {
            break;
        }

        output_tokens.push(tokens.next().unwrap());
    }

    if must_find_predicate && !predicate_met {
        match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => {
                panic!("unbalanced angle brackets in type expression");
            }
            None => {
                panic!("unexpected end of token stream in type expression");
            }
            _ => unreachable!(),
        }
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

/// Parse `::`, as in path separator or turbofish.
///
/// Does not advance `tokens` if the double colon is not found.
pub(crate) fn consume_colon2(tokens: &mut TokenIter) -> Option<[Punct; 2]> {
    // TODO consider multiple-lookahead instead of potentially cloning many tokens
    let before_start = tokens.clone();

    if let Some(TokenTree::Punct(first)) = tokens.next() {
        if first.as_char() == ':' && first.spacing() == Spacing::Joint {
            if let Some(TokenTree::Punct(second)) = tokens.next() {
                if second.as_char() == ':' && second.spacing() == Spacing::Alone {
                    return Some([first, second]);
                }
            }
        }
    }

    *tokens = before_start;
    None
}

/// Tries to parse a path expressions; returns `None` if not matching.
pub(crate) fn consume_path(mut tokens: TokenIter) -> Option<Path> {
    let mut segments = vec![];

    // Leading `::` is optional
    let mut tk_separator_colons = consume_colon2(&mut tokens);

    loop {
        // path elem
        let ident = match tokens.next() {
            Some(TokenTree::Ident(ident)) => ident.clone(),
            _ => return None, // end of tokens OR not a path
        };

        let generic_args = consume_generic_args(&mut tokens);

        segments.push(PathSegment {
            tk_separator_colons,
            ident,
            generic_args,
        });

        if tokens.peek().is_none() {
            break;
        }

        // Intermediate `::` are not optional
        if let Some(separator) = consume_colon2(&mut tokens) {
            tk_separator_colons = Some(separator);
        } else {
            return None;
        }
    }

    Some(Path { segments })
}
