#![allow(missing_docs)]

use proc_macro2::{Ident, Literal, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt as _};

// TODO - handle unions

/// The declaration of a Rust type.
///
/// **Example input:**
///
/// ```no_run
/// struct MyUnitStruct;
/// struct MyTupleStruct(i32, f32);
/// struct MyRegularStruct {
///     // ...
/// }
/// enum MyEnum {
///     // ...
/// }
/// ```
#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum Declaration {
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Function(Function),
}

// TODO - fn Declaration::name()
// TODO - fn Declaration::as_struct()
// TODO - fn Declaration::as_enum()

/// Declaration of a struct.
///
/// **Example input:**
///
/// ```no_run
/// struct MyUnitStruct;
/// struct MyTupleStruct(i32, f32);
/// struct MyRegularStruct {
///     // ...
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Struct {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clauses: Option<WhereClauses>,
    pub fields: StructFields,
}

// TODO - fn Struct::field_names()
// TODO - fn Struct::field_types()

/// Fields of a [`Struct`] or an [`EnumVariant`].
#[derive(Clone, Debug)]
pub enum StructFields {
    Unit,
    Tuple(Vec<TupleField>),
    Named(Vec<NamedField>),
}

/// Declaration of an enum.
///
/// **Example input:**
///
/// ```no_run
/// enum MyEnum {
///     // ...
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Enum {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clauses: Option<WhereClauses>,
    pub variants: Vec<EnumVariant>,
}

// TODO - fn Enum::is_c_enum()

/// The individual variant of an [`Enum`].
///
/// The variant can either be a c-like variant, hold one or multiple types,
/// or hold named fields depending on the value of `contents`.
#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub contents: StructFields,

    /// The value of the variant, normally for c-like enums.
    pub discriminant: Option<EnumDiscriminant>,
}

// TODO - fn EnumVariant::is_empty_variant()
// TODO - fn EnumVariant::get_single_type()

/// Declaration of an union.
///
/// **Example input:**
///
/// ```no_run
/// union MyUnion {
///     // ...
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Union {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clauses: Option<WhereClauses>,
    pub fields: Vec<NamedField>,
}

/// Declaration of a function.
///
/// **Example input:**
///
/// ```no_run
/// const fn hello(a: i32, b: f32) -> f32 { return 0.0; }
/// unsafe fn eval(c: String, b: i32) { return; }
/// fn do_thing<T: Clone>(t: T) where T: Default;
/// ```
#[derive(Clone, Debug)]
pub struct Function {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub qualifiers: FunctionQualifiers,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub params: Vec<FunctionParameter>,
    pub where_clauses: Option<WhereClauses>,
    pub return_ty: Option<TyExpr>,
    pub body: Option<TokenTree>,
}

/// Keywords giving special information on a function.
///
/// Possible qualifiers are `default`, `const`, `async`, `unsafe` and `extern`,
/// always in that order.
#[derive(Clone, Debug, Default)]
pub struct FunctionQualifiers {
    pub is_default: bool,
    pub is_const: bool,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub is_extern: bool,
    pub extern_abi: Option<Literal>,
}

/// A parameter of a [`Function`]
///
/// In the following code, the function parameters captured are `a: i32` and `b: f32`
///
/// ```no_run
/// pub fn hello_world(a: i32, b: f32) {}
/// ```
#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub ty: TyExpr,
}

/// A field of a tuple [`Struct`] or tuple-like [`EnumVariant`].
///
/// For instance, in `struct MyTuple(A, B, C);` A, B, C are each a tuple field.
#[derive(Clone, Debug)]
pub struct TupleField {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub ty: TyExpr,
}

/// A field of a [`Struct`] or struct-like [`EnumVariant`].
///
/// For instance, in the following code, `foo` and `bar` are each a struct field:
///
/// ```no_run
/// struct MyStruct {
///     foo: i32,
///     bar: f32,
/// }
/// ```
#[derive(Clone, Debug)]
pub struct NamedField {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub ty: TyExpr,
}

// --- Token groups ---

// TODO - parse better
// see https://doc.rust-lang.org/reference/attributes.html

/// An outer attribute.
///
/// **Example input:**
///
/// ```no_run
/// # #[cfg(FALSE)]
/// #[hello(world)]
/// # struct Foo;
/// ```
#[derive(Clone)]
pub struct Attribute {
    pub _hashbang: TokenTree,
    pub child_tokens: Vec<TokenTree>,
}

/// Visibility marker, eg `pub`, `pub(crate)`, `pub(super)`, etc.
#[derive(Clone)]
pub struct VisMarker {
    /// `pub` token.
    pub _token1: TokenTree,
    /// `(...)` token, if any.
    pub _token2: Option<TokenTree>,
}

/// The generic parameters declared right after your type's name.
///
/// **Example input:**
///
/// ```no_run
/// struct MyUnitStruct<A, B, C>(A, B, C);
/// ```
#[derive(Clone)]
pub struct GenericParams {
    pub tokens: Vec<TokenTree>,
}

/// All the stuff that comes after the `where` keyword.
#[derive(Clone)]
pub struct WhereClauses {
    pub tokens: Vec<TokenTree>,
}

/// Type expression in a [`TupleField`] or [`NamedField`].
///
/// **Example input:**
///
/// ```no_run
/// # struct MyUnitStruct<'a> {
///     foo: i32,
///     bar: &'a mut Vec<(u64, Option<bool>, f64)>,
/// };
/// ```
#[derive(Clone)]
pub struct TyExpr {
    pub tokens: Vec<TokenTree>,
}

/// The value of an [`EnumVariant`], normally for c-like enums.
///
/// **Example input:**
///
/// ```no_run
/// enum MyEnum {
///     A = 42,
///     # #[cfg(FALSE)]
///     B = (some + arbitrary.expression()),
/// }
/// ```
#[derive(Clone)]
pub struct EnumDiscriminant {
    pub tokens: Vec<TokenTree>,
}

// ---

impl std::fmt::Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("#")?;
        let mut list = f.debug_list();
        for token in &self.child_tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl std::fmt::Debug for VisMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self._token2 {
            None => f.write_str(&self._token1.to_string()),
            Some(TokenTree::Group(group)) => {
                let mut list = f.debug_tuple(&self._token1.to_string());
                for token in group.stream() {
                    match &token {
                        TokenTree::Group(_group) => list.field(&token),
                        TokenTree::Ident(_ident) => list.field(&token.to_string()),
                        TokenTree::Punct(_punct) => list.field(&token.to_string()),
                        TokenTree::Literal(_literal) => list.field(&token.to_string()),
                    };
                }
                list.finish()
            }
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Debug for GenericParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl std::fmt::Debug for WhereClauses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl std::fmt::Debug for TyExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl std::fmt::Debug for EnumDiscriminant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl ToTokens for Attribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self._hashbang.clone());
        for token in &self.child_tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for VisMarker {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self._token1.to_tokens(tokens);
        self._token2.to_tokens(tokens);
    }
}

impl ToTokens for GenericParams {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for WhereClauses {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for TyExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for EnumDiscriminant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}
