use crate::error::Error;
use crate::parse_utils::{
    consume_attributes, consume_comma, consume_stuff_until, consume_vis_marker, parse_ident,
};
use crate::punctuated::Punctuated;
use crate::types::{
    EnumVariant, EnumVariantValue, GenericBound, GenericParam, GenericParamList, NamedField,
    NamedStructFields, StructFields, TupleField, TupleStructFields, TyExpr, WhereClause,
    WhereClauseItem,
};
use crate::types_edition::GroupSpan;
use proc_macro2::{Delimiter, Group, Ident, Punct, TokenStream, TokenTree};
use std::iter::Peekable;

type TokenIter = Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn consume_declaration_name(tokens: &mut TokenIter) -> Ident {
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

pub(crate) fn consume_generic_params(tokens: &mut TokenIter) -> Option<GenericParamList> {
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

                let bound_tokens = consume_stuff_until(
                    tokens,
                    |token| match token {
                        TokenTree::Punct(punct) if punct.as_char() == ',' => true,
                        _ => false,
                    },
                    false,
                );

                Some(GenericBound {
                    tk_colon: colon,
                    tokens: bound_tokens,
                })
            }
            TokenTree::Punct(punct) if punct.as_char() == ',' => None,
            TokenTree::Punct(punct) if punct.as_char() == '>' => None,
            token => {
                panic!("cannot parse generic params: unexpected token {:?}", token)
            }
        };

        let comma = consume_comma(tokens);

        generic_params.push(
            GenericParam {
                tk_prefix: prefix,
                name,
                bound,
            },
            comma,
        );
    }

    // consume '>'
    tokens.next();

    Some(GenericParamList {
        tk_l_bracket: gt,
        params: generic_params,
        tk_r_bracket: lt,
    })
}

pub(crate) fn consume_where_clause(tokens: &mut TokenIter) -> Option<WhereClause> {
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

        let left_side = consume_stuff_until(
            tokens,
            |token| match token {
                TokenTree::Punct(punct) if punct.as_char() == ':' => true,
                _ => false,
            },
            false,
        );

        let colon = match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct.clone(),
            Some(token) => panic!(
                "cannot parse where clause: expected ':', found token {:?}",
                token
            ),
            None => {
                panic!("cannot parse where clause: expected colon, found end of stream")
            }
        };
        let bound_tokens = consume_stuff_until(
            tokens,
            |token| match token {
                TokenTree::Punct(punct) if punct.as_char() == ',' => true,
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
                TokenTree::Punct(punct) if punct.as_char() == ';' => true,
                _ => false,
            },
            true,
        );

        let comma = consume_comma(tokens);

        items.push(
            WhereClauseItem {
                left_side,
                bound: GenericBound {
                    tk_colon: colon,
                    tokens: bound_tokens,
                },
            },
            comma,
        );
    }

    Some(WhereClause {
        tk_where: where_token,
        items,
    })
}

pub(crate) fn consume_field_type(tokens: &mut TokenIter) -> Vec<TokenTree> {
    let field_type_tokens = consume_stuff_until(
        tokens,
        |token| match token {
            TokenTree::Punct(punct) if punct.as_char() == ',' => true,
            _ => false,
        },
        false,
    );

    if field_type_tokens.is_empty() && consume_comma(tokens).is_some() {
        panic!("cannot parse type: unexpected token ','");
    } else if field_type_tokens.is_empty() {
        panic!("cannot parse type: expected tokens, found end-of-stream");
    }

    field_type_tokens
}

pub(crate) fn consume_enum_discriminant(
    tokens: &mut TokenIter,
) -> Result<Option<EnumVariantValue>, Error> {
    let equal: Punct;
    match tokens.peek() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {
            equal = punct.clone();
        }
        _ => return Ok(None),
    };

    // consume '='
    tokens.next();

    let value_token = tokens.next().unwrap();

    // If the value expression has more than one token, we output an error.
    match tokens.peek() {
        None => (),
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => (),
        Some(_token) => return Err(Error::new("Complex values for enum variants are not supported unless they are between parentheses.")),
    }

    Ok(Some(EnumVariantValue {
        tk_equal: equal,
        value: value_token,
    }))
}

pub(crate) fn parse_tuple_fields(token_group: Group) -> TupleStructFields {
    let mut fields = Punctuated::new();

    let mut tokens = token_group.stream().into_iter().peekable();
    loop {
        if tokens.peek().is_none() {
            break;
        }

        let attributes = consume_attributes(&mut tokens);
        let vis_marker = consume_vis_marker(&mut tokens);

        let ty_tokens = consume_field_type(&mut tokens);

        let comma = consume_comma(&mut tokens);

        fields.push(
            TupleField {
                attributes,
                vis_marker,
                ty: TyExpr { tokens: ty_tokens },
            },
            comma,
        );
    }

    TupleStructFields {
        fields,
        tk_parens: GroupSpan::new(&token_group),
    }
}

pub(crate) fn parse_named_fields(token_group: Group) -> NamedStructFields {
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
        let comma = consume_comma(&mut tokens);

        fields.push(
            NamedField {
                attributes,
                vis_marker,
                name: ident,
                tk_colon: colon,
                ty: TyExpr { tokens: ty_tokens },
            },
            comma,
        );
    }

    NamedStructFields {
        fields,
        tk_braces: GroupSpan::new(&token_group),
    }
}

pub(crate) fn parse_enum_variants(tokens: TokenStream) -> Result<Punctuated<EnumVariant>, Error> {
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

        let comma = consume_comma(&mut tokens);

        variants.push(
            EnumVariant {
                attributes,
                vis_marker,
                name: ident,
                contents,
                value: enum_discriminant?,
            },
            comma,
        );
    }

    Ok(variants)
}
