use crate::error::Error;
use crate::parse_fn::consume_macro;
use crate::parse_impl::{
    consume_either_fn_type_const_static_impl, parse_const_or_static, parse_impl, parse_trait,
};
use crate::parse_mod::{parse_mod, parse_use_declaration};
use crate::parse_type::{
    consume_generic_params, consume_item_name, consume_where_clause, parse_enum_variants,
    parse_named_fields, parse_tuple_fields,
};
use crate::parse_utils::{consume_outer_attributes, consume_punct, consume_vis_marker};
use crate::types::{Enum, Fields, Item, Struct, Union};
use crate::types_edition::GroupSpan;
use proc_macro2::token_stream::IntoIter;
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use std::iter::Peekable;

/// Parses the token stream of an item declaration.
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
/// Panics if there are leftover tokens.
///
/// ## Example
///
/// ```
/// # use venial::{parse_item, Item};
/// # use quote::quote;
/// let struct_type = parse_item(quote!(
///     struct Hello {
///         foo: Foo,
///         bar: Bar,
///     }
/// ));
/// assert!(matches!(struct_type, Ok(Item::Struct(_))));
/// ```
///
/// ## Errors
///
/// Venial doesn't support enum discriminants with multiple non-grouped tokens. E.g.:
///
/// ```rust
/// # #[cfg(FALSE)]
/// enum MyEnum {
///     A = 42,           // Ok
///     B = "hello",      // Ok
///     C = CONSTANT,     // Ok
///     D = FOO + BAR,    // MACRO ERROR
///     E = (FOO + BAR),  // Ok
/// }
/// ```
pub fn parse_item(tokens: TokenStream) -> Result<Item, Error> {
    let mut tokens = tokens.into_iter().peekable();
    let declaration = consume_item(&mut tokens);

    if tokens.peek().is_some() {
        panic!(
            "unexpected trailing tokens after declaration: {}",
            tokens.collect::<TokenStream>()
        );
    }

    declaration
}

/// Consume an item declaration from a token stream.
///
/// This is the same as [parse_item], except it doesn't panic if there are
/// leftover tokens.
///
/// ## Panics
///
/// Panics if given a token stream that doesn't parse as a valid Rust
/// type declaration.
///
/// ## Errors
///
/// Venial doesn't support enum discriminants with multiple non-grouped tokens.
pub fn consume_item(tokens: &mut Peekable<IntoIter>) -> Result<Item, Error> {
    let attributes = consume_outer_attributes(tokens);
    let vis_marker = consume_vis_marker(tokens);

    let declaration = match tokens.peek().cloned() {
        Some(TokenTree::Ident(keyword)) if keyword == "struct" => {
            // struct keyword
            tokens.next().unwrap();

            let struct_name = consume_item_name(tokens);
            let generic_params = consume_generic_params(tokens);
            let mut where_clause = consume_where_clause(tokens);

            let struct_fields = match tokens
                .peek()
                .expect("cannot parse struct: missing body or semicolon")
            {
                TokenTree::Punct(punct) if punct.as_char() == ';' => Fields::Unit,
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    Fields::Tuple(parse_tuple_fields(group))
                }
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    let group = group.clone();
                    // Consume group
                    tokens.next();
                    Fields::Named(parse_named_fields(group))
                }
                token => panic!("cannot parse struct: unexpected token {:?}", token),
            };

            if matches!(struct_fields, Fields::Tuple(_)) {
                assert!(where_clause.is_none());
                where_clause = consume_where_clause(tokens);
            }

            let semicolon = consume_punct(tokens, ';');

            Item::Struct(Struct {
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

            let enum_name = consume_item_name(tokens);
            let generic_params = consume_generic_params(tokens);
            let where_clause = consume_where_clause(tokens);

            let (group, enum_variants) = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), parse_enum_variants(group.stream()))
                }
                token => panic!("cannot parse enum: unexpected token {:?}", token),
            };

            Item::Enum(Enum {
                attributes,
                vis_marker,
                tk_enum: keyword,
                name: enum_name,
                generic_params,
                where_clause,
                tk_braces: GroupSpan::new(&group),
                variants: enum_variants?,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "union" => {
            // union keyword
            tokens.next().unwrap();

            let union_name = consume_item_name(tokens);
            let generic_params = consume_generic_params(tokens);
            let where_clause = consume_where_clause(tokens);

            let union_fields = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    parse_named_fields(group)
                }
                token => panic!("cannot parse union: unexpected token {:?}", token),
            };

            Item::Union(Union {
                attributes,
                vis_marker,
                tk_union: keyword,
                name: union_name,
                generic_params,
                where_clause,
                fields: union_fields,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "mod" => {
            let mod_decl = parse_mod(tokens, attributes, vis_marker);
            Item::Module(mod_decl)
        }
        Some(TokenTree::Ident(keyword)) if keyword == "trait" => {
            let trait_decl = parse_trait(tokens, attributes, vis_marker);
            Item::Trait(trait_decl)
        }
        Some(TokenTree::Ident(keyword)) if keyword == "impl" => {
            let impl_decl = parse_impl(tokens, attributes);
            Item::Impl(impl_decl)
        }
        Some(TokenTree::Ident(keyword)) if keyword == "static" => {
            let static_decl = parse_const_or_static(tokens, attributes, vis_marker);
            Item::Constant(static_decl)
        }
        Some(TokenTree::Ident(keyword)) if keyword == "use" => {
            let use_decl = parse_use_declaration(tokens, attributes, vis_marker);

            Item::UseDeclaration(use_decl)
        }
        // Note: fn qualifiers appear always in this order in Rust: default const async unsafe extern fn
        Some(TokenTree::Ident(keyword))
            if matches!(
                keyword.to_string().as_str(),
                "default" | "const" | "async" | "unsafe" | "extern" | "fn" | "type"
            ) =>
        {
            // Reuse impl parsing
            consume_either_fn_type_const_static_impl(
                tokens,
                attributes,
                vis_marker,
                "fn/type/const/static/extern/extern crate",
            )
        }
        Some(token) => {
            if let Some(macro_) = consume_macro(tokens, attributes) {
                Item::Macro(macro_)
            } else {
                panic!(
                    "cannot parse declaration: expected keyword struct/enum/union/type/trait/impl/mod/default/const/async/unsafe/extern/fn/static or macro, found token {:?}",
                    token
                );
            }
        }
        None => {
            panic!("cannot parse type: expected keyword struct/enum/union/type/trait/impl/mod/default/const/async/unsafe/extern/fn/static or macro, found end-of-stream");
        }
    };
    Ok(declaration)
}
