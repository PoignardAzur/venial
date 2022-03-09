use crate::types::{
    Attribute, Enum, EnumDiscriminant, EnumVariant, GenericParams, NamedField, Struct,
    StructFields, TupleField, TyExpr, TypeDeclaration, VisMarker, WhereClauses,
};
use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

fn parse_ident(token: TokenTree) -> Option<Ident> {
    match token {
        TokenTree::Group(_group) => None,
        TokenTree::Ident(ident) => Some(ident),
        TokenTree::Punct(_punct) => None,
        TokenTree::Literal(_literal) => None,
    }
}

fn consume_attributes(tokens: &mut TokenIter) -> Vec<Attribute> {
    let mut attributes = Vec::new();

    loop {
        match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => (),
            _ => break,
        };
        let hashbang = tokens.next().unwrap();

        let child_tokens = match tokens.next().unwrap() {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket => group.stream(),
            _ => panic!("cannot parse type"),
        };

        attributes.push(Attribute {
            _hashbang: hashbang,
            child_tokens: child_tokens.into_iter().collect(),
        })
    }

    return attributes;
}

fn consume_vis_marker(tokens: &mut TokenIter) -> Option<VisMarker> {
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "pub" => {
            let pub_token = tokens.next().unwrap();
            match tokens.peek() {
                Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                    Some(VisMarker {
                        _token1: pub_token,
                        _token2: Some(tokens.next().unwrap()),
                    })
                }
                _ => Some(VisMarker {
                    _token1: pub_token,
                    _token2: None,
                }),
            }
        }
        Some(TokenTree::Ident(ident)) if ident == "crate" => Some(VisMarker {
            _token1: tokens.next().unwrap(),
            _token2: None,
        }),
        _ => None,
    }
}

// Consumes tokens until a separator is reached *unless* the
// separator in between angle brackets
// eg consume_stuff_until(..., ',') will consume all
// of `Foobar<A, B>,` except for the last comma
fn consume_stuff_until(
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

        output_tokens.push(tokens.next().unwrap());
    }

    output_tokens
}

fn consume_generic_params(tokens: &mut TokenIter) -> Option<GenericParams> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => (),
        _ => return None,
    };

    let param_tokens = consume_stuff_until(tokens, |_| true);

    Some(GenericParams {
        tokens: param_tokens,
    })
}

fn consume_where_clauses(tokens: &mut TokenIter) -> Option<WhereClauses> {
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "where" => (),
        _ => return None,
    }

    let where_clause_tokens = consume_stuff_until(tokens, |token| match token {
        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
        TokenTree::Punct(punct) if punct.as_char() == ';' => true,
        _ => false,
    });

    return Some(WhereClauses {
        tokens: where_clause_tokens,
    });
}

fn consume_field_type(tokens: &mut TokenIter) -> Vec<TokenTree> {
    let field_type_tokens = consume_stuff_until(tokens, |token| match token {
        TokenTree::Punct(punct) if punct.as_char() == ',' => true,
        _ => false,
    });

    // consume period, if any
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
            tokens.next();
        }
        _ => (),
    };

    return field_type_tokens;
}

fn consume_enum_discriminant(tokens: &mut TokenIter) -> Option<EnumDiscriminant> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => (),
        _ => return None,
    };

    let enum_discriminant_tokens = consume_stuff_until(tokens, |token| match token {
        TokenTree::Punct(punct) if punct.as_char() == ',' => true,
        _ => false,
    });

    return Some(EnumDiscriminant {
        tokens: enum_discriminant_tokens,
    });
}

fn parse_tuple_fields(tokens: TokenStream) -> Vec<TupleField> {
    let mut fields = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        fields.push(TupleField {
            attributes,
            vis_marker,
            ty: TyExpr {
                tokens: consume_field_type(&mut tokens),
            },
        });
    }

    fields
}

fn parse_named_fields(tokens: TokenStream) -> Vec<NamedField> {
    let mut fields = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => (),
            _ => panic!("cannot parse type"),
        };

        tokens.peek().unwrap();
        fields.push(NamedField {
            attributes,
            vis_marker,
            name: ident.to_string(),
            ty: TyExpr {
                tokens: consume_field_type(&mut tokens),
            },
        });
    }

    fields
}

fn parse_enum_variants(tokens: TokenStream) -> Vec<EnumVariant> {
    let mut variants = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        let next_token = tokens.peek();
        let contents = match next_token {
            None => StructFields::Unit,
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => StructFields::Unit,
            Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => StructFields::Unit,
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                let fields = group.stream();
                // Consume group
                tokens.next();
                StructFields::Tuple(parse_tuple_fields(fields))
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                let fields = group.stream();
                // Consume group
                tokens.next();
                StructFields::Named(parse_named_fields(fields))
            }
            _ => panic!("cannot parse type"),
        };

        let enum_discriminant = consume_enum_discriminant(&mut tokens);

        // Consume period, if any
        match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
                tokens.next();
            }
            _ => (),
        };

        variants.push(EnumVariant {
            attributes,
            vis_marker,
            name: ident.to_string(),
            contents,
            discriminant: enum_discriminant,
        });
    }

    variants
}

// TODO doc
pub fn parse_type(tokens: TokenStream) -> TypeDeclaration {
    let mut tokens = tokens.into_iter().peekable();

    let attributes = consume_attributes(&mut tokens);
    let vis_marker = consume_vis_marker(&mut tokens);

    if let Some(ident) = parse_ident(tokens.next().unwrap()) {
        if ident == "struct" {
            let struct_name = parse_ident(tokens.next().unwrap()).unwrap();

            let generic_params = consume_generic_params(&mut tokens);
            let mut where_clauses = consume_where_clauses(&mut tokens);

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

            if matches!(struct_fields, StructFields::Unit | StructFields::Tuple(_)) {
                where_clauses = consume_where_clauses(&mut tokens);
            }

            return TypeDeclaration::Struct(Struct {
                attributes,
                vis_marker,
                name: struct_name.to_string(),
                generic_params,
                where_clauses,
                fields: struct_fields,
            });
        } else if ident == "enum" {
            let next_token = tokens.next().unwrap();
            let enum_name = parse_ident(next_token).unwrap();

            let generic_params = consume_generic_params(&mut tokens);
            let where_clauses = consume_where_clauses(&mut tokens);

            let next_token = tokens.next().unwrap();
            let enum_variants = match next_token {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    parse_enum_variants(group.stream())
                }
                _ => panic!("cannot parse type"),
            };

            return TypeDeclaration::Enum(Enum {
                attributes,
                vis_marker,
                name: enum_name.to_string(),
                generic_params,
                where_clauses,
                variants: enum_variants,
            });
        } else {
            panic!("cannot parse type")
        }
    } else {
        panic!("cannot parse type")
    }
}
