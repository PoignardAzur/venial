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

fn parse_ident(token: TokenTree) -> Result<Ident, TokenTree> {
    match token {
        TokenTree::Group(_) => Err(token),
        TokenTree::Ident(ident) => Ok(ident),
        TokenTree::Punct(_) => Err(token),
        TokenTree::Literal(_) => Err(token),
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

        let group = match tokens.next() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Bracket => group,
            _ => panic!("cannot parse attribute: expected '[' after '#' token"),
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
            let pub_token = tokens
                .next()
                .expect("cannot parse declaration: expected token after 'pub'");
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

fn consume_declaration_name(tokens: &mut TokenIter) -> Ident {
    let token = tokens
        .next()
        .expect("cannot parse declaration: expected identifier, found end-of-stream");
    parse_ident(token).unwrap_or_else(|token| {
        panic!(
            "cannot parse declaration: expected identifier, found token {:?}",
            token
        );
    })
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

    // TODO - handle closures

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
        let token = tokens
            .peek()
            .expect("cannot parse generic params: expected token after '>'");
        let prefix = match token {
            TokenTree::Punct(punct) if punct.as_char() == '>' => {
                lt = punct.clone();
                break;
            }
            TokenTree::Punct(punct) if punct.as_char() == '\'' => Some(tokens.next().unwrap()),
            TokenTree::Ident(ident) if ident == "const" => Some(tokens.next().unwrap()),
            TokenTree::Ident(_ident) => None,
            token => {
                panic!("cannot parse generic params: unexpected token {:?}", token)
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
                panic!("cannot parse generic params: unexpected token {:?}", token)
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
        let token = tokens
            .peek()
            .expect("cannot parse where clause: expected tokens");
        match token {
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
            token => panic!(
                "cannot parse where clause: expected colon, found token {:?}",
                token
            ),
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

    if field_type_tokens.is_empty() && consume_period(tokens).is_some() {
        panic!("cannot parse type: unexpected token ','");
    } else if field_type_tokens.is_empty() {
        panic!("cannot parse type: expected tokens, found end-of-stream");
    }

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
            token => panic!(
                "cannot parse named fields: expected ':', found token {:?}",
                token
            ),
        };

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

        let contents = match tokens.peek() {
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
            token => panic!("cannot parse enum variant: unexpected token {:?}", token),
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
        _ => panic!("cannot parse fn return: expected '>' after '-' token"),
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
            _ => panic!("cannot parse fn params"),
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

    match tokens.peek().cloned() {
        Some(TokenTree::Ident(keyword)) if keyword == "struct" => {
            // struct keyword
            tokens.next().unwrap();

            let struct_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);
            let mut where_clause = consume_where_clause(&mut tokens);

            let struct_fields = match tokens.peek().unwrap() {
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
                token => panic!("cannot parse struct: unexpected token {:?}", token),
            };

            if matches!(struct_fields, StructFields::Tuple(_)) {
                assert!(where_clause.is_none());
                where_clause = consume_where_clause(&mut tokens);
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
                _struct: keyword.clone(),
                name: struct_name,
                generic_params,
                where_clause,
                fields: struct_fields,
                _semicolon: semicolon,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "enum" => {
            // enum keyword
            tokens.next().unwrap();

            let enum_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);
            let where_clause = consume_where_clause(&mut tokens);

            let (group, enum_variants) = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), parse_enum_variants(group.stream()))
                }
                token => panic!("cannot parse enum: unexpected token {:?}", token),
            };

            Declaration::Enum(Enum {
                attributes,
                vis_marker,
                _enum: keyword.clone(),
                name: enum_name,
                generic_params,
                where_clause,
                tk_braces: group,
                variants: enum_variants,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "union" => {
            // union keyword
            tokens.next().unwrap();

            let union_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);
            let where_clause = consume_where_clause(&mut tokens);

            let (group, union_fields) = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), parse_named_fields(group.clone()))
                }
                token => panic!("cannot parse union: unexpected token {:?}", token),
            };

            Declaration::Union(Union {
                attributes,
                vis_marker,
                _union: keyword.clone(),
                name: union_name,
                generic_params,
                where_clause,
                tk_braces: group,
                fields: union_fields,
            })
        }
        Some(TokenTree::Ident(keyword))
            if matches!(
                keyword.to_string().as_str(),
                "default" | "const" | "async" | "unsafe" | "extern" | "fn"
            ) =>
        {
            let qualifiers = consume_fn_qualifiers(&mut tokens);

            // fn keyword
            tokens.next().unwrap();

            let fn_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);

            let params = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    parse_fn_params(group.stream())
                }
                _ => panic!("cannot parse function"),
            };

            let return_ty = consume_fn_return(&mut tokens);

            let where_clause = consume_where_clause(&mut tokens);

            let function_body = match &tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    Some(group.clone())
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
                where_clause,
                return_ty,
                body: function_body,
            })
        }
        Some(token) => {
            panic!(
                "cannot parse declaration: expected keyword struct/enum/union/fn, found token {:?}",
                token
            );
        }
        None => {
            panic!("cannot parse type: expected keyword struct/enum/union/fn, found end-of-stream");
        }
    }
}
