use crate::{parse_utils::consume_stuff_until, types::Expression};
use proc_macro2::{Spacing, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

// Consumes tokens until a comma is reached, except in
// various corner cases related to expression syntax.
// eg consume_expression(...) will consume all
// of `a + |b, c| d, e::<F, G>(), h,` except for the last comma
pub(crate) fn consume_expression(tokens: &mut TokenIter) -> Expression {
    let mut output_tokens = Vec::new();

    // TODO - handle attributes
    // TODO - handle closures
    // TODO - handle <T as Trait>::xxx syntax

    // TODO - use matches instead?
    #[derive(Debug, PartialEq)]
    enum PrevToken {
        Any,
        FirstColon,
        DoubleColon,
    }
    let mut prev_token = PrevToken::Any;

    loop {
        let token = tokens.peek();
        prev_token = match &token {
            Some(TokenTree::Punct(punct))
                if punct.as_char() == ':' && punct.spacing() == Spacing::Joint =>
            {
                output_tokens.push(tokens.next().unwrap());
                PrevToken::FirstColon
            }
            Some(TokenTree::Punct(punct))
                if punct.as_char() == ':' && prev_token == PrevToken::FirstColon =>
            {
                output_tokens.push(tokens.next().unwrap());
                PrevToken::DoubleColon
            }

            Some(TokenTree::Punct(punct))
                if punct.as_char() == '<' && prev_token == PrevToken::DoubleColon =>
            {
                let mut turbofish_contents = consume_stuff_until(tokens, |_| true);
                output_tokens.append(&mut turbofish_contents);
                PrevToken::Any
            }

            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => break,
            None => break,

            _ => {
                output_tokens.push(tokens.next().unwrap());
                PrevToken::Any
            }
        };
    }

    Expression {
        tokens: output_tokens,
    }
}
