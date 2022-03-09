use crate::types::*;
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
            _ => return attributes,
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

fn consume_generic_params(tokens: &mut TokenIter) -> Vec<TokenTree> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => (),
        _ => return Vec::new(),
    };

    let mut param_tokens = Vec::new();
    let mut bracket_count = 0;
    loop {
        let token = tokens.next().unwrap();
        match &token {
            TokenTree::Punct(punct) if punct.as_char() == '<' => {
                bracket_count += 1;
            }
            TokenTree::Punct(punct) if punct.as_char() == '>' => {
                bracket_count -= 1;
            }
            _ => {}
        };

        param_tokens.push(token);

        if bracket_count == 0 {
            return param_tokens;
        }
    }
}

fn consume_where_clauses(tokens: &mut TokenIter) -> Vec<TokenTree> {
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "where" => (),
        _ => return Vec::new(),
    }

    let mut where_clause_tokens = Vec::new();
    let mut bracket_count = 0;
    loop {
        match tokens.peek().unwrap() {
            TokenTree::Punct(punct) if punct.as_char() == '<' => {
                bracket_count += 1;
            }
            TokenTree::Punct(punct) if punct.as_char() == '>' => {
                bracket_count -= 1;
            }
            TokenTree::Group(group)
                if group.delimiter() == Delimiter::Brace && bracket_count == 0 =>
            {
                return where_clause_tokens;
            }
            TokenTree::Punct(punct) if punct.as_char() == ';' && bracket_count == 0 => {
                return where_clause_tokens;
            }
            _ => {}
        };

        where_clause_tokens.push(tokens.next().unwrap());
    }
}

fn consume_field_type(tokens: &mut TokenIter) -> Vec<TokenTree> {
    let mut field_type_tokens = Vec::new();
    let mut bracket_count = 0;
    loop {
        match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                bracket_count += 1;
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => {
                bracket_count -= 1;
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' && bracket_count == 0 => {
                // consume period
                tokens.next();
                return field_type_tokens;
            }
            None => {
                return field_type_tokens;
            }
            _ => {}
        };

        field_type_tokens.push(tokens.next().unwrap());
    }
}

pub fn parse_tuple_fields(tokens: TokenStream) -> Vec<TupleField> {
    // TODO - attributes
    // TODO - skip generic params
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

pub fn parse_named_fields(tokens: TokenStream) -> Vec<NamedField> {
    // TODO - attributes
    // TODO - skip generic params
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

pub fn parse_enum_variants(tokens: TokenStream) -> Vec<EnumVariant> {
    // TODO - attributes
    // TODO - skip generic params
    let mut variants = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);

        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        let next_token = tokens.next();
        let contents = match next_token {
            None => StructFields::Unit,
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => StructFields::Unit,
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                // Consume period, if any
                tokens.next();
                StructFields::Tuple(parse_tuple_fields(group.stream()))
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                // Consume period, if any
                tokens.next();
                StructFields::Named(parse_named_fields(group.stream()))
            }
            _ => panic!("cannot parse type"),
        };

        variants.push(EnumVariant {
            attributes,
            name: ident.to_string(),
            contents,
        });
    }

    variants
}

pub fn parse_type(tokens: TokenStream) -> TypeDeclaration {
    let mut tokens = tokens.into_iter().peekable();

    let attributes = consume_attributes(&mut tokens);
    let vis_marker = consume_vis_marker(&mut tokens);

    if let Some(ident) = parse_ident(tokens.next().unwrap()) {
        if ident == "struct" {
            let struct_name = parse_ident(tokens.next().unwrap()).unwrap();

            consume_generic_params(&mut tokens);
            consume_where_clauses(&mut tokens);

            let next_token = tokens.next().unwrap();
            let struct_fields = match next_token {
                TokenTree::Punct(punct) if punct.as_char() == ';' => StructFields::Unit,
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    StructFields::Tuple(parse_tuple_fields(group.stream()))
                    //consume_where_clauses(&mut tokens);
                }
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    StructFields::Named(parse_named_fields(group.stream()))
                }
                _ => panic!("cannot parse type"),
            };

            return TypeDeclaration::Struct(Struct {
                attributes,
                vis_marker,
                name: struct_name.to_string(),
                fields: struct_fields,
            });
        } else if ident == "enum" {
            let next_token = tokens.next().unwrap();
            let enum_name = parse_ident(next_token).unwrap();

            consume_generic_params(&mut tokens);
            consume_where_clauses(&mut tokens);

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
                variants: enum_variants,
            });
        } else {
            panic!("cannot parse type")
        }
    } else {
        panic!("cannot parse type")
    }
}
