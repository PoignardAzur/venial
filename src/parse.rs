use crate::{
    types::{
        Attribute, Declaration, Enum, EnumDiscriminant, EnumVariant, Function, FunctionParameter,
        FunctionQualifiers, GenericBound, GenericParam, GenericParams, NamedField,
        NamedStructFields, Struct, StructFields, TupleField, TupleStructFields, TyExpr, Union,
        VisMarker, WhereClause, WhereClauseItem,
    },
    Punctuated,
};
use proc_macro2::{Delimiter, Group, Ident, Punct, TokenStream, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

// TODO - improve panic messages

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
        let hashbang = match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => punct.clone(),
            _ => break,
        };
        tokens.next();

        let group = match tokens.next().unwrap() {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket => group,
            _ => panic!("cannot parse type"),
        };

        attributes.push(Attribute {
            _hashbang: hashbang,
            child_tokens: group.stream().into_iter().collect(),
            _braces: group,
        });
    }

    attributes
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

        // We're out of the brack group we were called in
        // TODO - explain better
        if bracket_count < 0 {
            break;
        }

        output_tokens.push(tokens.next().unwrap());
    }

    output_tokens
}

fn consume_period(tokens: &mut TokenIter) -> Option<Punct> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
            let punct = punct.clone();
            tokens.next().unwrap();
            Some(punct)
        }
        _ => None,
    }
}

fn consume_generic_params(tokens: &mut TokenIter) -> Option<GenericParams> {
    let gt: Punct;
    let mut generic_params = Punctuated::new();
    let lt: Punct;

    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
            gt = punct.clone();
        }
        _ => return None,
    };
    // consume '<'
    tokens.next();

    loop {
        let prefix = match tokens.peek().unwrap() {
            TokenTree::Punct(punct) if punct.as_char() == '>' => {
                lt = punct.clone();
                break;
            }
            TokenTree::Punct(punct) if punct.as_char() == '\'' => Some(tokens.next().unwrap()),
            TokenTree::Ident(ident) if ident == "const" => Some(tokens.next().unwrap()),
            TokenTree::Ident(_ident) => None,
            token => {
                dbg!(&token);
                panic!("cannot parse type")
            }
        };

        let name = parse_ident(tokens.next().unwrap()).unwrap();

        let bound = match tokens.peek().unwrap() {
            TokenTree::Punct(punct) if punct.as_char() == ':' => {
                let colon = punct.clone();
                // consume ':'
                tokens.next();

                let bound_tokens = consume_stuff_until(tokens, |token| match token {
                    TokenTree::Punct(punct) if punct.as_char() == ',' => true,
                    _ => false,
                });

                Some(GenericBound {
                    _colon: colon,
                    tokens: bound_tokens,
                })
            }
            TokenTree::Punct(punct) if punct.as_char() == ',' => None,
            TokenTree::Punct(punct) if punct.as_char() == '>' => None,
            token => {
                dbg!(&token);
                panic!("cannot parse type")
            }
        };

        let period = consume_period(tokens);

        generic_params.push(
            GenericParam {
                _prefix: prefix,
                name,
                bound,
            },
            period,
        );
    }

    // consume '>'
    tokens.next();

    Some(GenericParams {
        _l_bracket: gt,
        params: generic_params,
        _r_bracket: lt,
    })
}

fn consume_where_clause(tokens: &mut TokenIter) -> Option<WhereClause> {
    let where_token: Ident;
    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "where" => {
            where_token = ident.clone();
        }
        _ => return None,
    }
    tokens.next();

    let mut items = Punctuated::new();
    loop {
        match tokens.peek().unwrap() {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => break,
            TokenTree::Punct(punct) if punct.as_char() == ';' => break,
            _ => (),
        };

        let left_side = consume_stuff_until(tokens, |token| match token {
            TokenTree::Punct(punct) if punct.as_char() == ':' => true,
            _ => false,
        });

        let colon = match tokens.next().unwrap() {
            TokenTree::Punct(punct) if punct.as_char() == ':' => punct.clone(),
            _ => panic!("cannot parse type"),
        };
        let bound_tokens = consume_stuff_until(tokens, |token| match token {
            TokenTree::Punct(punct) if punct.as_char() == ',' => true,
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
            TokenTree::Punct(punct) if punct.as_char() == ';' => true,
            _ => false,
        });

        let period = consume_period(tokens);

        items.push(
            WhereClauseItem {
                left_side,
                bound: GenericBound {
                    _colon: colon,
                    tokens: bound_tokens,
                },
            },
            period,
        );
    }

    Some(WhereClause {
        _where: where_token,
        items,
    })
}

fn consume_field_type(tokens: &mut TokenIter) -> Vec<TokenTree> {
    let field_type_tokens = consume_stuff_until(tokens, |token| match token {
        TokenTree::Punct(punct) if punct.as_char() == ',' => true,
        _ => false,
    });

    consume_period(tokens);

    field_type_tokens
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

    Some(EnumDiscriminant {
        tokens: enum_discriminant_tokens,
    })
}

fn parse_tuple_fields(token_group: Group) -> TupleStructFields {
    let mut fields = Punctuated::new();

    let mut tokens = token_group.stream().into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        let ty_tokens = consume_field_type(&mut tokens);
        let period = consume_period(&mut tokens);

        fields.push(
            TupleField {
                attributes,
                vis_marker,
                ty: TyExpr { tokens: ty_tokens },
            },
            period,
        );
    }

    TupleStructFields {
        fields,
        tk_parens: token_group,
    }
}

fn parse_named_fields(token_group: Group) -> NamedStructFields {
    let mut fields = Punctuated::new();

    let mut tokens = token_group.stream().into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        let colon = match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct,
            _ => panic!("cannot parse type"),
        };
        tokens.peek().unwrap();

        let ty_tokens = consume_field_type(&mut tokens);
        let period = consume_period(&mut tokens);

        fields.push(
            NamedField {
                attributes,
                vis_marker,
                name: ident,
                _colon: colon,
                ty: TyExpr { tokens: ty_tokens },
            },
            period,
        );
    }

    NamedStructFields {
        fields,
        tk_braces: token_group,
    }
}

fn parse_enum_variants(tokens: TokenStream) -> Punctuated<EnumVariant> {
    let mut variants = Punctuated::new();

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
                let group = group.clone();
                // Consume group
                tokens.next();
                StructFields::Tuple(parse_tuple_fields(group))
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                let group = group.clone();
                // Consume group
                tokens.next();
                StructFields::Named(parse_named_fields(group))
            }
            _ => panic!("cannot parse type"),
        };

        let enum_discriminant = consume_enum_discriminant(&mut tokens);

        let period = consume_period(&mut tokens);

        variants.push(
            EnumVariant {
                attributes,
                vis_marker,
                name: ident,
                contents,
                discriminant: enum_discriminant,
            },
            period,
        );
    }

    variants
}

fn consume_fn_qualifiers(tokens: &mut TokenIter) -> FunctionQualifiers {
    let mut qualifiers = FunctionQualifiers::default();

    qualifiers.tk_default = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "default" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    qualifiers.tk_const = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "const" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    qualifiers.tk_async = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "async" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };
    qualifiers.tk_unsafe = match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "unsafe" => {
            let ident = ident.clone();
            tokens.next();
            Some(ident)
        }
        _ => None,
    };

    match tokens.peek() {
        Some(TokenTree::Ident(ident)) if ident == "extern" => {
            qualifiers.tk_extern = Some(ident.clone());
            tokens.next();

            match tokens.peek() {
                Some(TokenTree::Literal(literal)) => {
                    qualifiers.extern_abi = Some(literal.clone());
                    tokens.next();
                }
                _ => (),
            }
        }
        _ => (),
    };

    qualifiers
}

fn consume_fn_return(tokens: &mut TokenIter) -> Option<TyExpr> {
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '-' => (),
        _ => return None,
    };
    let _dash = tokens.next().unwrap();

    match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => (),
        _ => panic!("expect '>' token"),
    };

    Some(TyExpr {
        tokens: (consume_stuff_until(tokens, |token| match token {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
            TokenTree::Ident(i) if i == &Ident::new("where", i.span()) => true,
            TokenTree::Punct(punct) if punct.as_char() == ';' => true,
            _ => false,
        })),
    })
}

fn parse_fn_params(tokens: TokenStream) -> Punctuated<FunctionParameter> {
    let mut fields = Punctuated::new();

    let mut tokens = tokens.into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }
        let attributes = consume_attributes(&mut tokens);

        // TODO - handle non-ident argument names
        let ident = parse_ident(tokens.next().unwrap()).unwrap();

        // TODO - Handle self parameter
        match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => (),
            _ => panic!("cannot parse type"),
        };

        let ty_tokens = consume_field_type(&mut tokens);
        let period = consume_period(&mut tokens);

        fields.push(
            FunctionParameter {
                attributes,
                name: ident,
                ty: TyExpr { tokens: ty_tokens },
            },
            period,
        );
    }

    fields
}

// TODO - Return Result<...>, handle case where TokenStream is valid declaration,
// but not a type or function.

/// Parses the token stream of a type declaration.
///
/// For instance, if you're implementing a derive macro, you can pass the
/// token stream as-is.
///
/// ## Panics
///
/// Panics if given a token stream that doesn't parse as a valid Rust
/// type declaration. If the token stream is from an attribute or a derive
/// macro, there should be no way for this to happen, as Rust will emit an
/// error instead of calling this macro.
///
/// ## Example
///
/// ```
/// # use venial::{parse_declaration, Declaration};
/// # use quote::quote;
/// let struct_type = parse_declaration(quote!(
///     struct Hello {
///         foo: Foo,
///         bar: Bar,
///     }
/// ));
/// assert!(matches!(struct_type, Declaration::Struct(_)));
/// ```
///
pub fn parse_declaration(tokens: TokenStream) -> Declaration {
    let mut tokens = tokens.into_iter().peekable();

    let attributes = consume_attributes(&mut tokens);
    let vis_marker = consume_vis_marker(&mut tokens);

    // TODO - remove next_token vars

    if let Some(ident) = parse_ident(tokens.peek().unwrap().clone()) {
        if ident == "struct" {
            // struct keyword
            tokens.next().unwrap();

            let struct_name = parse_ident(tokens.next().unwrap()).unwrap();

            let generic_params = consume_generic_params(&mut tokens);
            let mut where_clauses = consume_where_clause(&mut tokens);

            let next_token = tokens.peek().unwrap();
            let struct_fields = match next_token {
                TokenTree::Punct(punct) if punct.as_char() == ';' => StructFields::Unit,
                // TODO - add test
                TokenTree::Ident(ident) if ident == "where" => StructFields::Unit,
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    StructFields::Tuple(parse_tuple_fields(group.clone()))
                }
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    StructFields::Named(parse_named_fields(group.clone()))
                }
                _ => panic!("cannot parse type"),
            };

            if matches!(struct_fields, StructFields::Tuple(_)) {
                assert!(where_clauses.is_none());
                where_clauses = consume_where_clause(&mut tokens);
            }

            let semicolon = match tokens.peek() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => {
                    let punct = punct.clone();
                    tokens.next().unwrap();
                    Some(punct)
                }
                _ => None,
            };

            Declaration::Struct(Struct {
                attributes,
                vis_marker,
                _struct: ident.clone(),
                name: struct_name,
                generic_params,
                where_clause: where_clauses,
                fields: struct_fields,
                _semicolon: semicolon,
            })
        } else if ident == "enum" {
            // enum keyword
            tokens.next().unwrap();

            let next_token = tokens.next().unwrap();
            let enum_name = parse_ident(next_token).unwrap();

            let generic_params = consume_generic_params(&mut tokens);
            let where_clauses = consume_where_clause(&mut tokens);

            let next_token = tokens.next().unwrap();
            let (group, enum_variants) = match next_token {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), parse_enum_variants(group.stream()))
                }
                _ => panic!("cannot parse type"),
            };

            Declaration::Enum(Enum {
                attributes,
                vis_marker,
                _enum: ident.clone(),
                name: enum_name,
                generic_params,
                where_clauses,
                tk_braces: group,
                variants: enum_variants,
            })
        } else if ident == "union" {
            // union keyword
            tokens.next().unwrap();

            let next_token = tokens.next().unwrap();
            let union_name = parse_ident(next_token).unwrap();

            let generic_params = consume_generic_params(&mut tokens);
            let where_clauses = consume_where_clause(&mut tokens);

            let next_token = tokens.next().unwrap();
            let (group, union_fields) = match next_token {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), parse_named_fields(group.clone()))
                }
                _ => panic!("cannot parse type"),
            };

            Declaration::Union(Union {
                attributes,
                vis_marker,
                _union: ident.clone(),
                name: union_name,
                generic_params,
                where_clauses,
                tk_braces: group,
                fields: union_fields,
            })
        } else if matches!(
            ident.to_string().as_str(),
            "default" | "const" | "async" | "unsafe" | "extern" | "fn"
        ) {
            let qualifiers = consume_fn_qualifiers(&mut tokens);

            // fn keyword
            tokens.next().unwrap();

            let fn_name = parse_ident(tokens.next().unwrap()).unwrap();

            let generic_params = consume_generic_params(&mut tokens);

            let next_token = tokens.next().unwrap();
            let params = match next_token {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    parse_fn_params(group.stream())
                }
                _ => panic!("cannot parse function"),
            };

            let return_ty = consume_fn_return(&mut tokens);

            let where_clauses = consume_where_clause(&mut tokens);

            let next_token = tokens.next().unwrap();
            let function_body = match &next_token {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    Some(next_token)
                }
                TokenTree::Punct(punct) if punct.as_char() == ';' => None,
                _ => panic!("cannot parse function"),
            };

            Declaration::Function(Function {
                attributes,
                vis_marker,
                qualifiers,
                name: fn_name,
                generic_params,
                params,
                where_clauses,
                return_ty,
                body: function_body,
            })
        } else {
            panic!("cannot parse type")
        }
    } else {
        panic!("cannot parse type")
    }
}
