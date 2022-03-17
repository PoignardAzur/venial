#![allow(missing_docs)]

use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt as _};

use crate::Punctuated;

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
/// # #[cfg(FALSE)]
/// union MyUnion {
///     // ...
/// }
/// fn foobar() {}
/// ```
#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum Declaration {
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Function(Function),
}

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
    pub _struct: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clause: Option<WhereClause>,
    pub fields: StructFields,
    pub _semicolon: Option<Punct>,
}

/// Fields of a [`Struct`] or an [`EnumVariant`].
#[derive(Clone, Debug)]
pub enum StructFields {
    Unit,
    Tuple(TupleStructFields),
    Named(NamedStructFields),
}

#[derive(Clone)]
pub struct TupleStructFields {
    pub fields: Punctuated<TupleField>,
    pub tk_parens: Group,
}

#[derive(Clone)]
pub struct NamedStructFields {
    pub fields: Punctuated<NamedField>,
    pub tk_braces: Group,
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
#[derive(Clone)]
pub struct Enum {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub _enum: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clause: Option<WhereClause>,
    pub tk_braces: Group,
    pub variants: Punctuated<EnumVariant>,
}

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

/// Declaration of an union.
///
/// **Example input:**
///
/// ```no_run
/// # #[cfg(FALSE)]
/// union MyUnion {
///     // ...
/// }
/// ```
#[derive(Clone)]
pub struct Union {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub _union: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub where_clause: Option<WhereClause>,
    pub tk_braces: Group,
    pub fields: NamedStructFields,
}

/// Declaration of a function.
///
/// **Example input:**
///
/// ```no_run
/// const fn hello(a: i32, b: f32) -> f32 { return 0.0; }
/// unsafe fn eval(c: String, b: i32) { return; }
/// # #[cfg(FALSE)]
/// fn do_thing<T: Clone>(t: T) where T: Default;
/// ```
#[derive(Clone, Debug)]
pub struct Function {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub qualifiers: FunctionQualifiers,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub params: Punctuated<FunctionParameter>,
    pub where_clause: Option<WhereClause>,
    pub return_ty: Option<TyExpr>,
    pub body: Option<Group>,
}

/// Keywords giving special information on a function.
///
/// Possible qualifiers are `default`, `const`, `async`, `unsafe` and `extern`,
/// always in that order.
#[derive(Clone, Debug, Default)]
pub struct FunctionQualifiers {
    pub tk_default: Option<Ident>,
    pub tk_const: Option<Ident>,
    pub tk_async: Option<Ident>,
    pub tk_unsafe: Option<Ident>,
    pub tk_extern: Option<Ident>,
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
    pub _colon: Punct,
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
    pub _hashbang: Punct,
    pub _braces: Group,
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
    pub _l_bracket: Punct,
    pub params: Punctuated<GenericParam>,
    pub _r_bracket: Punct,
}

/// A parameter in a type's generic list.
///
/// **Example input:**
///
/// ```no_run
/// # struct MyUnitStruct<
/// 'a, B, const C: usize,
/// # >(&'a [B; C]);
/// ```
#[derive(Clone)]
pub struct GenericParam {
    /// Either `'` for lifetimes, `const` for const parameters, or None for type parameters.
    pub _prefix: Option<TokenTree>,
    pub name: Ident,
    pub bound: Option<GenericBound>,
}

/// A parameter bound in a type's generic list.
///
/// For instance, this is the `: Clone` in `struct MyStruct <T: Clone>(T);`
#[derive(Clone)]
pub struct GenericBound {
    pub _colon: Punct,
    pub tokens: Vec<TokenTree>,
}

/// Generic arguments deduced from a type's [GenericParams].
///
/// For instance, `<'a: 'static, T, U: Clone, const N: usize>` becomes `<'a, T, U, N>`.
/// This is useful when creating wrapper types in derive macros.
pub struct InlineGenericArgs<'a>(pub(crate) &'a GenericParams);

/// All the stuff that comes after the `where` keyword.
#[derive(Clone)]
pub struct WhereClause {
    pub _where: Ident,
    pub items: Punctuated<WhereClauseItem>,
}

/// An item in a where clause
///
/// This is the `T: Clone` in the following code:
///
/// ```
/// struct MyStruct<T>(T) where T: Clone;
/// ```
#[derive(Clone)]
pub struct WhereClauseItem {
    pub left_side: Vec<TokenTree>,
    pub bound: GenericBound,
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

// --- Debug impls ---

struct TokenRef<'a>(&'a TokenTree);

impl<'a> std::fmt::Debug for TokenRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            TokenTree::Group(_group) => self.0.fmt(f),
            TokenTree::Ident(_ident) => f.write_str(&self.0.to_string()),
            TokenTree::Punct(_punct) => write!(f, "\"{}\"", &self.0.to_string()),
            TokenTree::Literal(_literal) => f.write_str(&self.0.to_string()),
        }
    }
}

impl std::fmt::Debug for TupleStructFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fields.fmt(f)
    }
}

impl std::fmt::Debug for NamedStructFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fields.fmt(f)
    }
}

impl std::fmt::Debug for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Enum")
            .field("attributes", &self.attributes)
            .field("vis_marker", &self.vis_marker)
            .field("_enum", &self._enum)
            .field("name", &self.name)
            .field("generic_params", &self.generic_params)
            .field("where_clauses", &self.where_clause)
            .field("variants", &self.variants)
            .finish()
    }
}

impl std::fmt::Debug for Union {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Union")
            .field("attributes", &self.attributes)
            .field("vis_marker", &self.vis_marker)
            .field("_union", &self._union)
            .field("name", &self.name)
            .field("generic_params", &self.generic_params)
            .field("where_clauses", &self.where_clause)
            .field("fields", &self.fields)
            .finish()
    }
}

impl std::fmt::Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("#")?;
        let mut list = f.debug_list();
        for token in &self.child_tokens {
            list.entry(&TokenRef(&token));
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
                    list.field(&TokenRef(&token));
                }
                list.finish()
            }
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Debug for GenericParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.params.fmt(f)
    }
}

impl std::fmt::Debug for GenericParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("GenericParam");
        f.field("name", &self.name.to_string());
        f.field("bound", &self.bound);
        f.finish()
    }
}

impl std::fmt::Debug for GenericBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            list.entry(&TokenRef(&token));
        }
        list.finish()
    }
}

impl std::fmt::Debug for WhereClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.items.fmt(f)
    }
}

impl std::fmt::Debug for WhereClauseItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in quote::quote!(#self) {
            list.entry(&TokenRef(&token));
        }
        list.finish()
    }
}

impl std::fmt::Debug for TyExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            list.entry(&TokenRef(&token));
        }
        list.finish()
    }
}

impl std::fmt::Debug for EnumDiscriminant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            list.entry(&TokenRef(&token));
        }
        list.finish()
    }
}

// --- ToTokens impls ---

impl ToTokens for Declaration {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.to_tokens(tokens),
            Declaration::Enum(enum_decl) => enum_decl.to_tokens(tokens),
            Declaration::Union(union_decl) => union_decl.to_tokens(tokens),
            Declaration::Function(function_decl) => function_decl.to_tokens(tokens),
        }
    }
}

impl ToTokens for Struct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self._struct.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);

        if matches!(&self.fields, StructFields::Named(_)) {
            self.where_clause.to_tokens(tokens);
            self.fields.to_tokens(tokens);
        } else {
            self.fields.to_tokens(tokens);
            self.where_clause.to_tokens(tokens);
            self._semicolon.to_tokens(tokens);
        }
    }
}

impl ToTokens for StructFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            StructFields::Unit => (),
            StructFields::Tuple(fields) => fields.to_tokens(tokens),
            StructFields::Named(fields) => fields.to_tokens(tokens),
        }
    }
}

impl ToTokens for TupleStructFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_parens.to_tokens(tokens);
    }
}

impl ToTokens for NamedStructFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_braces.to_tokens(tokens);
    }
}

impl ToTokens for Enum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self._enum.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.tk_braces.to_tokens(tokens);
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.contents.to_tokens(tokens);
        self.discriminant.to_tokens(tokens);
    }
}

impl ToTokens for Union {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self._union.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.tk_braces.to_tokens(tokens);
    }
}

// FIXME
impl ToTokens for Function {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.qualifiers.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.params.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        //self.return_ty.to_tokens(tokens);
        //self.body.to_tokens(tokens);
    }
}

impl ToTokens for FunctionQualifiers {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_default.to_tokens(tokens);
        self.tk_const.to_tokens(tokens);
        self.tk_async.to_tokens(tokens);
        self.tk_unsafe.to_tokens(tokens);
        self.tk_extern.to_tokens(tokens);
        self.extern_abi.to_tokens(tokens);
    }
}

// FIXME
impl ToTokens for FunctionParameter {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.name.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for TupleField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for NamedField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self._colon.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for Attribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self._hashbang.clone());
        tokens.append(self._braces.clone());
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
        tokens.append(self._l_bracket.clone());
        self.params.to_tokens(tokens);
        tokens.append(self._r_bracket.clone());
    }
}

impl ToTokens for GenericParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self._prefix.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.bound.to_tokens(tokens);
    }
}

impl ToTokens for GenericBound {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self._colon.to_tokens(tokens);
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for InlineGenericArgs<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(Punct::new('<', Spacing::Alone));

        for param in &self.0.params.inner {
            if param.0.is_lifetime() {
                param.0._prefix.to_tokens(tokens);
            }
            tokens.append(param.0.name.clone());
            tokens.append(Punct::new(',', Spacing::Alone));
        }

        tokens.append(Punct::new('>', Spacing::Alone));
    }
}

impl ToTokens for WhereClause {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self._where.clone());
        self.items.to_tokens(tokens);
    }
}

impl ToTokens for WhereClauseItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.left_side {
            tokens.append(token.clone());
        }
        self.bound.to_tokens(tokens);
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

// --- Default impls ---

impl Default for GenericParams {
    fn default() -> Self {
        Self {
            _l_bracket: Punct::new('<', Spacing::Alone),
            params: Punctuated::new(),
            _r_bracket: Punct::new('>', Spacing::Alone),
        }
    }
}

impl Default for WhereClause {
    fn default() -> Self {
        // Note that an empty where clause is perfectly valid syntax
        Self {
            _where: Ident::new("where", Span::call_site()),
            items: Punctuated::new(),
        }
    }
}
