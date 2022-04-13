use crate::parse_expr::consume_expression;
use crate::parse_fn::{consume_fn_qualifiers, consume_fn_return, parse_fn_params};
use crate::parse_type::{
    consume_declaration_name, consume_generic_params, consume_where_clause, parse_enum_variants,
    parse_named_fields, parse_tuple_fields,
};
use crate::parse_utils::{consume_attributes, consume_comma, consume_vis_marker};
use crate::punctuated::Punctuated;
use crate::types::{Declaration, Enum, Expression, Function, Struct, StructFields, Union};
use crate::types_edition::GroupSpan;
use proc_macro2::{Delimiter, TokenStream, TokenTree};

#[doc(hidden)]
pub fn parse_expression_list(tokens: TokenStream) -> Punctuated<Expression> {
    let mut tokens = tokens.into_iter().peekable();
    let mut expressions = Punctuated::new();

    while tokens.peek().is_some() {
        let expression = consume_expression(&mut tokens);
        let expression = expression;

        let comma = consume_comma(&mut tokens);

        expressions.push(expression, comma);
    }

    expressions
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
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    StructFields::Tuple(parse_tuple_fields(group))
                }
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    StructFields::Named(parse_named_fields(group))
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
                tk_struct: keyword,
                name: struct_name,
                generic_params,
                where_clause,
                fields: struct_fields,
                tk_semicolon: semicolon,
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
                tk_enum: keyword,
                name: enum_name,
                generic_params,
                where_clause,
                tk_braces: GroupSpan::new(&group),
                variants: enum_variants,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "union" => {
            // union keyword
            tokens.next().unwrap();

            let union_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);
            let where_clause = consume_where_clause(&mut tokens);

            let union_fields = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    parse_named_fields(group)
                }
                token => panic!("cannot parse union: unexpected token {:?}", token),
            };

            Declaration::Union(Union {
                attributes,
                vis_marker,
                tk_union: keyword,
                name: union_name,
                generic_params,
                where_clause,
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
