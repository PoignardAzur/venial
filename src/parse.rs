use crate::error::Error;
use crate::parse_impl::{consume_constant, consume_fn_const_or_type, consume_for, parse_impl_body};
use crate::parse_type::{
    consume_declaration_name, consume_generic_params, consume_where_clause, parse_enum_variants,
    parse_named_fields, parse_tuple_fields,
};
use crate::parse_utils::{consume_outer_attributes, consume_stuff_until, consume_vis_marker};
use crate::types::{Declaration, Enum, Impl, Mod, Struct, StructFields, Union};
use crate::types_edition::GroupSpan;
use crate::{ImplMember, TyExpr};
use proc_macro2::token_stream::IntoIter;
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use std::iter::Peekable;

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
/// assert!(matches!(struct_type, Ok(Declaration::Struct(_))));
/// ```
///
/// ## Errors
///
/// Venial doesn't support enum discriminants with multiple non-grouped tokens. Eg:
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

pub fn parse_declaration(tokens: TokenStream) -> Result<Declaration, Error> {
    let mut tokens = tokens.into_iter().peekable();
    parse_declaration_tokens(&mut tokens)
}

fn parse_declaration_tokens(mut tokens: &mut Peekable<IntoIter>) -> Result<Declaration, Error> {
    // FIXME remove redundant '&mut' once the risk of merge conflicts is smaller
    // TODO handle inner attrs
    let attributes = consume_outer_attributes(&mut tokens);
    let vis_marker = consume_vis_marker(&mut tokens);

    let declaration = match tokens.peek().cloned() {
        Some(TokenTree::Ident(keyword)) if keyword == "struct" => {
            // struct keyword
            tokens.next().unwrap();

            let struct_name = consume_declaration_name(&mut tokens);
            let generic_params = consume_generic_params(&mut tokens);
            let mut where_clause = consume_where_clause(&mut tokens);

            let struct_fields = match tokens
                .peek()
                .expect("cannot parse struct: missing body or semicolon")
            {
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
                variants: enum_variants?,
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
        Some(TokenTree::Ident(keyword)) if keyword == "mod" => {
            // mod keyword
            tokens.next().unwrap();

            let module_name = consume_declaration_name(&mut tokens);

            let (group, stream) = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    (group.clone(), group.stream())
                }
                token => panic!("cannot parse mod: unexpected token {:?}", token),
            };

            let mut members = vec![];
            let mut tokens = stream.into_iter().peekable();
            loop {
                let item = parse_declaration_tokens(&mut tokens)?;
                members.push(item);
                if tokens.peek().is_none() {
                    break;
                }
            }
            Declaration::Mod(Mod {
                attributes,
                vis_marker,
                tk_mod: keyword,
                name: module_name,
                tk_braces: GroupSpan::new(&group),
                // FIXME inner attributes, use statements
                members,
            })
        }
        Some(TokenTree::Ident(keyword)) if keyword == "impl" => {
            // impl keyword
            tokens.next().unwrap();

            let impl_generic_params = consume_generic_params(&mut tokens);
            let trait_or_self_ty = consume_stuff_until(
                &mut tokens,
                |tk| match tk {
                    TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
                    TokenTree::Ident(ident) if ident == "for" || ident == "where" => true,
                    _ => false,
                },
                true,
            );

            let (tk_for, trait_ty, self_ty) = if let Some(tk_for) = consume_for(&mut tokens) {
                let self_ty = consume_stuff_until(
                    &mut tokens,
                    |tk| match tk {
                        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => true,
                        TokenTree::Ident(ident) if ident == "where" => true,
                        _ => false,
                    },
                    true,
                );

                (
                    Some(tk_for),
                    Some(TyExpr {
                        tokens: trait_or_self_ty,
                    }),
                    TyExpr { tokens: self_ty },
                )
            } else {
                (
                    None,
                    None,
                    TyExpr {
                        tokens: trait_or_self_ty,
                    },
                )
            };

            let where_clause = consume_where_clause(&mut tokens);

            let (tk_braces, inner_attributes, body_items) = match tokens.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                    parse_impl_body(group)
                }
                token => panic!("cannot parse impl: unexpected token {:?}", token),
            };

            Declaration::Impl(Impl {
                attributes,
                tk_impl: keyword,
                impl_generic_params,
                trait_ty,
                tk_for,
                self_ty,
                where_clause,
                body_items,
                inner_attributes,
                tk_braces,
            })
        }
        // Note: fn qualifiers appear always in this order in Rust
        Some(TokenTree::Ident(keyword))
            if matches!(
                keyword.to_string().as_str(),
                "default" | "const" | "async" | "unsafe" | "extern" | "fn" | "type"
            ) =>
        {
            // Reuse impl parsing
            match consume_fn_const_or_type(&mut tokens, attributes, vis_marker, "declaration") {
                ImplMember::Method(function) => Declaration::Function(function),
                ImplMember::Constant(constant) => Declaration::Constant(constant),
                ImplMember::AssocTy(ty_def) => Declaration::TyDefinition(ty_def),
            }

        }
        Some(token) => {
            panic!(
                "cannot parse declaration: expected keyword struct/enum/union/type/impl/default/const/async/unsafe/extern/fn, found token {:?}",
                token
            );
        }
        None => {
            panic!("cannot parse type: expected keyword struct/enum/union/type/impl/default/const/async/unsafe/extern/fn, found end-of-stream");
        }
    };
    Ok(declaration)
}
