use crate::types::*;
use proc_macro2::{token_stream, Delimiter, Ident, TokenStream, TokenTree};

fn parse_ident(token: TokenTree) -> Option<Ident> {
    match token {
        TokenTree::Group(_group) => None,
        TokenTree::Ident(ident) => Some(ident),
        TokenTree::Punct(_punct) => None,
        TokenTree::Literal(_literal) => None,
    }
}

fn consume_until_period(
    first_token: Option<TokenTree>,
    tokens: &mut token_stream::IntoIter,
) -> Vec<TokenTree> {
    let mut tokens_before_period = Vec::new();

    if let Some(token) = first_token {
        tokens_before_period.push(token)
    }

    for token in tokens.into_iter() {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == ',' => break,
            _ => {
                tokens_before_period.push(token);
            }
        }
    }

    tokens_before_period
}

pub fn parse_tuple_fields(tokens: TokenStream) -> Vec<TupleField> {
    // TODO - attributes
    // TODO - skip generic params
    let mut fields = Vec::new();

    let mut tokens = tokens.into_iter();
    loop {
        let next_token = if let Some(next_token) = tokens.next() {
            next_token
        } else {
            break;
        };

        fields.push(TupleField {
            ty: TyExpr {
                tokens: consume_until_period(Some(next_token), &mut tokens),
            },
        });
    }

    fields
}

pub fn parse_named_fields(tokens: TokenStream) -> Vec<NamedField> {
    // TODO - attributes
    // TODO - skip generic params
    let mut fields = Vec::new();

    let mut tokens = tokens.into_iter();
    loop {
        let next_token = if let Some(next_token) = tokens.next() {
            next_token
        } else {
            break;
        };

        let ident = parse_ident(next_token).unwrap();

        match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => (),
            _ => panic!("cannot parse type"),
        };

        let next_token = tokens.next().unwrap();
        fields.push(NamedField {
            name: ident.to_string(),
            ty: TyExpr {
                tokens: consume_until_period(Some(next_token), &mut tokens),
            },
        });
    }

    fields
}

pub fn parse_type(tokens: TokenStream) -> TypeDeclaration {
    let mut tokens = tokens.into_iter();

    let next_token = tokens.next().unwrap();

    if let Some(ident) = parse_ident(next_token) {
        if ident == "struct" {
            let next_token = tokens.next().unwrap();
            let struct_name = parse_ident(next_token).unwrap();

            let next_token = tokens.next().unwrap();
            let struct_fields = match next_token {
                TokenTree::Punct(punct) if punct.as_char() == ';' => StructFields::Unit,
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    StructFields::Tuple(parse_tuple_fields(group.stream()))
                }
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    StructFields::Named(parse_named_fields(group.stream()))
                }
                _ => panic!("cannot parse type"),
            };

            return TypeDeclaration::Struct(Struct {
                name: struct_name.to_string(),
                contents: struct_fields,
            });
        } else if ident == "enum" {
            todo!()
        } else {
            panic!("cannot parse type")
        }
    } else {
        panic!("cannot parse type")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;
    use quote::quote;

    #[test]
    fn parse_unit_struct() {
        let struct_type = parse_type(quote!(
            struct Hello;
        ));

        assert_debug_snapshot!(struct_type);
    }

    #[test]
    fn parse_tuple_struct() {
        let struct_type = parse_type(quote!(
            struct Hello(A, B);
        ));

        assert_debug_snapshot!(struct_type);
    }

    #[test]
    fn parse_normal_struct() {
        let struct_type = parse_type(quote!(
            struct Hello {
                a: A,
                b: B,
            }
        ));

        assert_debug_snapshot!(struct_type);
    }
}
