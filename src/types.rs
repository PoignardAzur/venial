#![allow(missing_docs)]

use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt as _};

pub use crate::Punctuated;

/// The declaration of a Rust item.
///
/// See also <https://doc.rust-lang.org/reference/items.html>.
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
/// union MyUnion {
///     // ...
///     # x: i32,
/// }
/// fn foobar() {}
/// ```
#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum Item {
    /// `struct` declaration.
    Struct(Struct),

    /// `enum` declaration.
    Enum(Enum),

    /// `union` declaration.
    Union(Union),

    /// `mod` declaration (with a `{}` block, or to refer to separate files as in `mod file;`).
    Module(Module),

    /// `trait` declaration.
    Trait(Trait),

    /// `impl` block (inherent or trait implementation).
    Impl(Impl),

    /// `type` declaration.
    TypeAlias(TypeAlias),

    /// `fn` declaration.
    Function(Function),

    /// `const` or `static` declaration.
    Constant(Constant),

    /// `use` statement.
    UseDeclaration(UseDeclaration),

    /// Macro invocation.
    Macro(Macro),

    /// Extern block.
    ExternBlock(ExternBlock),

    /// Extern crate declaration.
    ExternCrate(ExternCrate),
}

/// Declaration of a struct.
///
/// See also <https://doc.rust-lang.org/reference/items/structs.html>.
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
    pub tk_struct: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParamList>,
    pub where_clause: Option<WhereClause>,
    pub fields: Fields,
    pub tk_semicolon: Option<Punct>,
}

/// Fields of a [`Struct`] or an [`EnumVariant`].
#[derive(Clone, Debug)]
pub enum Fields {
    Unit,
    Tuple(TupleFields),
    Named(NamedFields),
}

/// Fields in a tuple-style [`Struct`].
#[derive(Clone)]
pub struct TupleFields {
    pub fields: Punctuated<TupleField>,
    pub tk_parens: GroupSpan,
}

/// Fields in a nominal [`Struct`].
#[derive(Clone)]
pub struct NamedFields {
    pub fields: Punctuated<NamedField>,
    pub tk_braces: GroupSpan,
}

/// Declaration of an `enum`.
///
/// See also <https://doc.rust-lang.org/reference/items/enumerations.html>.
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
    pub tk_enum: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParamList>,
    pub where_clause: Option<WhereClause>,
    pub tk_braces: GroupSpan,
    pub variants: Punctuated<EnumVariant>,
}

/// The individual variant of an [`Enum`].
///
/// The variant can either be a c-like variant, hold one or multiple types,
/// or hold named fields depending on the value of `fields`.
#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: Ident,
    pub fields: Fields,

    /// The value of the variant, normally for c-like enums.
    pub value: Option<EnumVariantValue>,
}

/// Declaration of a module.
///
/// **Example input:**
///
/// ```no_run
/// mod module {
///     // ...
/// }
/// ```
///
/// See also: <https://doc.rust-lang.org/reference/items/modules.html>
#[derive(Clone, Debug)]
pub struct Module {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    /// `unsafe` keyword before mod, syntactically valid though unusual.
    pub tk_unsafe: Option<Ident>,
    /// `mod` keyword.
    pub tk_mod: Ident,
    /// Module name.
    pub name: Ident,
    /// `;` token, only present for foreign mods, such as `mod my_module;`.
    /// This is mutually exclusive with `tk_braces` and all remaining fields.
    pub tk_semicolon: Option<Punct>,
    /// `{ }` group, only available for mods with a local block, such as `mod my_module { ... }`.
    /// This is mutually exclusive with `tk_semicolon`.
    pub tk_braces: Option<GroupSpan>,
    /// List of all `#![inner]` attributes.
    pub inner_attributes: Vec<Attribute>,
    /// Everything declared inside the module.
    pub members: Vec<Item>,
}

/// Declaration of a union.
///
/// See also <https://doc.rust-lang.org/reference/items/unions.html>.
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
    pub tk_union: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParamList>,
    pub where_clause: Option<WhereClause>,
    pub fields: NamedFields,
}

/// Declaration of a trait.
///
/// See also <https://doc.rust-lang.org/reference/items/traits.html>.
///
/// **Example input:**
///
/// ```no_run
/// trait MyTrait {
///     const N: i32;
///     fn some_fn() -> i32;
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Trait {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub tk_unsafe: Option<Ident>,
    pub tk_trait: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParamList>,
    pub bound: Option<GenericBound>,
    pub where_clause: Option<WhereClause>,
    pub tk_braces: GroupSpan,
    pub inner_attributes: Vec<Attribute>,
    pub body_items: Vec<TraitMember>,
}

/// Associated items in a trait.
///
/// See also <https://doc.rust-lang.org/reference/items/associated-items.html>.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum TraitMember {
    AssocFunction(Function),
    AssocConstant(Constant),
    AssocType(TypeAlias),
    Macro(Macro),
}

/// Declaration of an `impl` block.
///
/// See also <https://doc.rust-lang.org/reference/items/implementations.html>.
///
/// **Example input:**
///
/// ```no_run
/// # #[cfg(FALSE)]
/// impl MyType {
///     // ...
/// }
/// ```
/// or:
/// ```no_run
/// # #[cfg(FALSE)]
/// impl MyTrait for MyType {
///     // ...
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Impl {
    pub attributes: Vec<Attribute>,
    pub tk_unsafe: Option<Ident>,
    pub tk_impl: Ident,
    pub impl_generic_params: Option<GenericParamList>,
    pub trait_ty: Option<TypeExpr>,
    pub tk_for: Option<Ident>,
    pub self_ty: TypeExpr,
    pub where_clause: Option<WhereClause>,
    pub tk_braces: GroupSpan,
    pub inner_attributes: Vec<Attribute>,
    pub body_items: Vec<ImplMember>,
}

/// The group representing an `impl` block.
///
/// See also <https://doc.rust-lang.org/reference/items/associated-items.html>.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ImplMember {
    AssocFunction(Function),
    AssocConstant(Constant),
    AssocType(TypeAlias),
    Macro(Macro),
}

/// Constant or static declaration.
///
/// See also <https://doc.rust-lang.org/reference/items/constant-items.html>
/// and <https://doc.rust-lang.org/reference/items/static-items.html>.
///
/// **Example inputs:**
///
/// ```no_run
/// pub const CONSTANT: i32 = 783;
/// # trait Trait {
/// const CONSTANT: i32;
/// # }
/// static MUTEX: std::sync::Mutex<i32> = std::sync::Mutex::new(0);
/// pub(crate) static mut STATIC: i32 = 7;
/// ```
#[derive(Clone, Debug)]
pub struct Constant {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    /// Either `const` or `static` keyword.
    pub tk_const_or_static: Ident,
    /// `mut` keyword in `static mut`, absent for `const` or immutable `static`.
    pub tk_mut: Option<Ident>,
    pub name: Ident,
    pub tk_colon: Punct,
    pub ty: TypeExpr,
    /// The `=` sign is optional; constants without initializer are syntactically valid.
    pub tk_equals: Option<Punct>,
    /// The initializer value is optional; constants without initializer are syntactically valid.
    pub initializer: Option<ValueExpr>,
    pub tk_semicolon: Punct,
}

impl Constant {
    /// True if this is a `static`, false if `const`.
    pub fn is_static(&self) -> bool {
        self.tk_const_or_static == "static"
    }
}

/// Type definition.
/// Handles both associated types (in `impl` blocks) or type aliases (globally).
///
/// See also <https://doc.rust-lang.org/reference/items/type-aliases.html>.
///
/// **Example input:**
///
/// ```no_run
/// type MyType = i32;
/// ```
#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub tk_type: Ident,
    pub name: Ident,
    pub bound: Option<GenericBound>,
    pub tk_equals: Option<Punct>,
    pub initializer_ty: Option<TypeExpr>,
    pub tk_semicolon: Punct,
}

/// Declaration of a function.
///
/// See also <https://doc.rust-lang.org/reference/items/functions.html>.
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
    pub qualifiers: FnQualifiers,
    pub tk_fn_keyword: Ident,
    pub name: Ident,
    pub generic_params: Option<GenericParamList>,
    pub tk_params_parens: GroupSpan,
    pub params: Punctuated<FnParam>,
    pub where_clause: Option<WhereClause>,
    pub tk_return_arrow: Option<[Punct; 2]>,
    pub return_ty: Option<TypeExpr>,
    pub tk_semicolon: Option<Punct>,
    pub body: Option<Group>,
}

/// Keywords giving special information on a function.
///
/// Possible qualifiers are `default`, `const`, `async`, `unsafe` and `extern`,
/// always in that order.
#[derive(Clone, Debug, Default)]
pub struct FnQualifiers {
    pub tk_default: Option<Ident>,
    pub tk_const: Option<Ident>,
    pub tk_async: Option<Ident>,
    pub tk_unsafe: Option<Ident>,
    pub tk_extern: Option<Ident>,
    pub extern_abi: Option<Literal>,
}

/// A parameter of a [`Function`].
///
/// Function parameters can either be receivers (`self` variations) or typed parameters (`name: type` form).
///
/// In the following code, the parameters captured are `&self`, `a: i32` and `b: f32`:
/// ```no_run
/// # struct S; impl S {
/// pub fn hello_world(&self, a: i32, b: f32) {}
/// # }
/// ```
#[derive(Clone, Debug)]
pub enum FnParam {
    Receiver(FnReceiverParam),
    Typed(FnTypedParam),
}

/// [`Function`] parameter which refers to `self` in some way.
///
/// Possible parameters captures by this are `self`, `mut self`, `&self` or `&mut self`.
/// Reference lifetimes are not yet supported.
///
/// Parameters of the form `self: Pin<&mut Self>` are recognized as [`FnTypedParam`].
#[derive(Clone, Debug)]
pub struct FnReceiverParam {
    pub attributes: Vec<Attribute>,
    /// `&` token.
    pub tk_ref: Option<Punct>,
    /// `'lifetime` tokens.
    pub lifetime: Option<Lifetime>,
    /// `mut` keyword.
    pub tk_mut: Option<Ident>,
    /// `self` keyword; required.
    pub tk_self: Ident,
}

/// [`Function`] parameter with explicit type.
///
/// In the following code, the function parameters captured are `a: i32` and `mut b: f32`
///
/// ```no_run
/// pub fn hello_world(a: i32, mut b: f32) {}
/// ```
#[derive(Clone, Debug)]
pub struct FnTypedParam {
    pub attributes: Vec<Attribute>,
    pub tk_mut: Option<Ident>,
    pub name: Ident,
    pub tk_colon: Punct,
    pub ty: TypeExpr,
}

/// A field of a tuple [`Struct`] or tuple-like [`EnumVariant`].
///
/// For instance, in `struct MyTuple(A, B, C);` A, B, C are each a tuple field.
#[derive(Clone, Debug)]
pub struct TupleField {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub ty: TypeExpr,
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
    pub tk_colon: Punct,
    pub ty: TypeExpr,
}

// --- Token groups ---

/// An outer or inner attribute.
///
/// **Example input:**
///
/// ```no_run
/// # #[cfg(FALSE)]
/// #[hello(world)]
/// # struct Foo;
/// ```
///
/// See also: <https://doc.rust-lang.org/reference/attributes.html>
#[derive(Clone)]
pub struct Attribute {
    /// `#`, always present
    pub tk_hash: Punct,
    /// `!`, present only for inner attributes
    pub tk_bang: Option<Punct>,
    /// `[ ]`
    pub tk_brackets: GroupSpan,
    /// The `hello` in `#[hello(world)]`. May be an arbitrary path, eg `a::b::c`.
    pub path: Vec<TokenTree>,
    /// Everything that comes after the path
    pub value: AttributeValue,
}

/// The value of an [`Attribute`].
///
/// In `#[hello(world)]`, this is the `(world)` part.
#[derive(Clone)]
pub enum AttributeValue {
    /// Example: `#[hello(world)]`
    Group(GroupSpan, Vec<TokenTree>),
    /// Example: `#[hello = world]`
    Equals(Punct, Vec<TokenTree>),
    /// Example: `#[hello]`
    Empty,
}

/// Visibility marker, eg `pub`, `pub(crate)`, `pub(super)`, etc.
#[derive(Clone)]
pub struct VisMarker {
    /// `pub` token.
    pub tk_token1: TokenTree,
    /// `(...)` token, if any.
    pub tk_token2: Option<TokenTree>,
}

/// The generic parameters declared right after your type's name.
///
/// **Example input:**
///
/// ```no_run
/// struct MyUnitStruct<A, B, C>(A, B, C);
/// ```
#[derive(Clone)]
pub struct GenericParamList {
    pub tk_l_bracket: Punct,
    pub params: Punctuated<GenericParam>,
    pub tk_r_bracket: Punct,
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
    pub tk_prefix: Option<TokenTree>,
    pub name: Ident,
    pub bound: Option<GenericBound>,
}

/// A parameter bound in a type's generic list.
///
/// For instance, this is the `: Clone` in `struct MyStruct <T: Clone>(T);`
#[derive(Clone)]
pub struct GenericBound {
    pub tk_colon: Punct,
    pub tokens: Vec<TokenTree>,
}

/// List of generic arguments, as in `Vec<i32, Alloc>`.
#[derive(Clone)]
pub struct GenericArgList {
    /// If the list is in expression position, e.g. `Vec::<i32, Alloc>`, then this captures the `::`.
    pub tk_turbofish_colons: Option<[Punct; 2]>,
    /// Opening bracket of the generic argument list.
    pub tk_l_bracket: Punct,
    /// The arguments, separated by comma. Nested `<` `>` tokens will be part of this.
    pub args: Punctuated<GenericArg>,
    /// Opening bracket of the generic argument list.
    pub tk_r_bracket: Punct,
}

/// A parameter in a type's generic list.
///
/// **Example inputs:** (arguments are _inside_ the angle brackets)
///
/// ```no_run
/// # use std::borrow::Cow;
/// # mod path { pub mod to { pub type Type = i32; }}
/// # struct MyArray<const N: i32> {}
/// # #[allow(clippy::needless_lifetimes)] fn f<'a>(tuple: (
/// Cow<'a, path::to::Type>,
/// impl Iterator<Item = path::to::Type>,
/// MyArray<17>,
/// # )) {}
/// ```
#[derive(Clone, Debug)]
pub enum GenericArg {
    /// E.g. `Ref<'a>`.
    Lifetime { lifetime: Lifetime },
    /// E.g. `Iterator<Item = path::to::Type>`.
    Binding {
        /// For the above example, this would be `Item`.
        ident: Ident,
        tk_equals: Punct,
        /// For the above example, this would be `path::to::Type`.
        /// Note that it may also capture constants, e.g. `MyArray<32>` this would be `32`.
        ty: TypeExpr,
    },
    /// E.g. `Rc<path::to::Type>` or `MyArray<32>`.  
    /// Since expressions are not parsed, the two cannot be differentiated.
    TypeOrConst { expr: TypeExpr },
}

/// Generic arguments deduced from a type's [GenericParamList].
///
/// For instance, `<'a: 'static, T, U: Clone, const N: usize>` becomes `<'a, T, U, N>`.
/// This is useful when creating wrapper types in derive macros.
///
/// Note: this is a thin reference type. Creating this type doesn't inherently modify
/// the underlying [GenericParamList]; this is just a wrapper that is processed
/// differently when passed to quote macros.
pub struct InlineGenericArgs<'a>(pub(crate) &'a GenericParamList);

/// Lifetime declaration.
///
/// Handles the tokens `'lifetime` in various contexts.
#[derive(Clone, Debug)]
pub struct Lifetime {
    pub tk_apostrophe: Punct,
    pub name: Ident,
}

/// All the stuff that comes after the `where` keyword.
#[derive(Clone)]
pub struct WhereClause {
    pub tk_where: Ident,
    pub items: Punctuated<WhereClausePredicate>,
}

/// Item in a where clause.
///
/// This is the `T: Clone` in the following code:
///
/// ```
/// struct MyStruct<T>(T) where T: Clone;
/// ```
#[derive(Clone)]
pub struct WhereClausePredicate {
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
/// # };
/// ```
#[derive(Clone)]
pub struct TypeExpr {
    pub tokens: Vec<TokenTree>,
}

/// Value expression, e.g. as the initializer of a [`Constant`].
///
/// **Example input:**
///
/// ```no_run
/// const CONSTANT: i32 = 5 * 302;
/// //                    ^^^^^^^
/// ```
#[derive(Clone)]
pub struct ValueExpr {
    pub tokens: Vec<TokenTree>,
}

/// Implements a path expression according to the [Rust documentation](https://doc.rust-lang.org/reference/paths.html).
///
/// Examples: `bool`, `path::to::Type`, `module::Type::<i32, Item=()>`, etc.
#[derive(Clone, Debug)]
pub struct Path {
    // Note: could use Punctuated once that supports other tokens than ','
    pub segments: Vec<PathSegment>,
}

/// A segment of a [`Path`], e.g. `Type::<i32>`
#[derive(Clone)]
pub struct PathSegment {
    /// `::` separator in front (can be omitted for leading segment)
    pub tk_separator_colons: Option<[Punct; 2]>,
    /// Main identifier of the path
    pub ident: Ident,
    /// Generic argument list, if available
    pub generic_args: Option<GenericArgList>,
}

/// A `use` declaration for a path.
///
/// See also <https://doc.rust-lang.org/reference/items/use-declarations.html>.
///
/// **Example input:**
///
/// ```no_run
/// use std::collections::hash_map::{self, HashMap};
/// ```
#[derive(Clone, Debug)]
pub struct UseDeclaration {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    /// The `use` keyword
    pub tk_use: Ident,
    /// Un-tokenized import paths between `use` and the finishing `;`
    pub import_tree: TypeExpr,
    /// Semicolon ending the use statement
    pub tk_semicolon: Punct,
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
#[derive(Clone, Debug)]
pub struct EnumVariantValue {
    pub tk_equal: Punct,
    pub value: TokenTree,
}

/// A macro invocation or `macro_rules!` declaration.
///
/// See also <https://doc.rust-lang.org/reference/macros.html> and
/// <https://doc.rust-lang.org/reference/macros-by-example.html>.
///
/// **Example input:**
///
/// ```no_run
/// # macro_rules! lazy_static { ($($tt:tt)*) => {  } };
/// lazy_static! {
///     /// Some doc comment.
///     static ref EXAMPLE: u8 = 42;
/// }
/// ```
///
/// Declarations of declarative macros are also supported; their only syntactical difference is an identifier (here `my_macro`):
/// ```no_run
/// macro_rules! my_macro {
///    ($($tt:tt)*) => { $($tt)* };
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Macro {
    /// Any attributes, such as `#[macro_export]`.
    pub attributes: Vec<Attribute>,
    /// Name of the invoked macro. In case of a macro declaration, this is `macro_rules`.
    pub name: Ident,
    /// The `!` token.
    pub tk_bang: Punct,
    /// Only set for `macro_rules!` declarations.
    ///
    /// In `macro_rules! my_macro { ... }`, this is `my_macro`.
    pub tk_declared_name: Option<Ident>,
    /// The `{}` or `()` group around the macro invocation.
    pub tk_braces_or_parens: GroupSpan,
    /// Unparsed tokens in the macro invocation.
    pub inner_tokens: Vec<TokenTree>,
    /// The `;` token, in case `()` is used.
    pub tk_semicolon: Option<Punct>,
}

/// An `extern` block.
///
/// See also <https://doc.rust-lang.org/reference/items/external-blocks.html>.
///
/// **Example input:**
///
/// ```no_run
/// # macro_rules! some_macro { () => {} };
/// extern "cdecl" {
///    #![allow(unused_variables)]
///
///    some_macro!();
///    fn f();
///    static S: i32;
/// }
#[derive(Clone, Debug)]
pub struct ExternBlock {
    /// Any attributes before the `extern` declaration.
    pub attributes: Vec<Attribute>,
    /// Visibility such as `pub`, `pub(crate)` etc.
    pub vis_marker: Option<VisMarker>,
    /// Optional `unsafe` keyword.
    pub tk_unsafe: Option<Ident>,
    /// `extern` keyword.
    pub tk_extern: Ident,
    /// Optional ABI string, e.g. `"stdcall"`.
    pub extern_abi: Option<Literal>,
    /// The `{}` marking the block.
    pub tk_braces: GroupSpan,
    /// Any `#![...]` attributes inside the block.
    pub inner_attributes: Vec<Attribute>,
    /// Members inside the extern block.
    ///
    /// Note that Rust syntax only allows macros, static items or functions. venial is slightly less strict here.
    /// Since a subset of [`ImplMember`] is valid in Rust, that type is used.
    pub body_items: Vec<ImplMember>,
}

/// Extern crate declaration.
///
/// See also <https://doc.rust-lang.org/reference/items/extern-crates.html>.
///
/// **Example input:**
///
/// ```no_run
/// extern crate std;
/// extern crate std as ruststd;
/// extern crate self as _;
/// ```
#[derive(Clone, Debug)]
pub struct ExternCrate {
    /// Any attributes before the `extern crate` declaration.
    pub attributes: Vec<Attribute>,
    /// Visibility such as `pub`, `pub(crate)` etc.
    pub vis_marker: Option<VisMarker>,
    /// `extern` keyword.
    pub tk_extern: Ident,
    /// `crate` keyword.
    pub tk_crate: Ident,
    /// Name of the crate. Can also be `self`.
    pub name: Ident,
    /// Optional `as` keyword.
    pub tk_as: Option<Ident>,
    /// Crate alias (only present if `as` is present).
    pub alias: Option<Ident>,
    /// `_` token (as an alternative to `alias`, only if `as` is present).
    pub tk_underscore: Option<Ident>,
    /// `;` token.
    pub tk_semicolon: Punct,
}

/// Span information about a [`Group`].
///
/// This can be used to recreate the group from its inner token sequence, or to create
/// a new group with a modified token sequence but the original group's span information.
#[derive(Clone)]
pub struct GroupSpan {
    pub delimiter: Delimiter,
    pub span: Span,
}

// --- Debug impls ---

struct TokenRef<'a>(&'a TokenTree);

pub(crate) fn format_debug_tokens(
    f: &mut std::fmt::Formatter<'_>,
    tokens: &[TokenTree],
) -> std::fmt::Result {
    let mut list = f.debug_list();
    for token in tokens {
        list.entry(&TokenRef(token));
    }
    list.finish()
}

impl std::fmt::Debug for TokenRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            TokenTree::Group(_group) => self.0.fmt(f),
            TokenTree::Ident(_ident) => f.write_str(&self.0.to_string()),
            TokenTree::Punct(_punct) => write!(f, "\"{}\"", &self.0.to_string()),
            TokenTree::Literal(_literal) => f.write_str(&self.0.to_string()),
        }
    }
}

impl std::fmt::Debug for TupleFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fields.fmt(f)
    }
}

impl std::fmt::Debug for NamedFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fields.fmt(f)
    }
}

impl std::fmt::Debug for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Enum")
            .field("attributes", &self.attributes)
            .field("vis_marker", &self.vis_marker)
            .field("tk_enum", &self.tk_enum)
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
            .field("tk_union", &self.tk_union)
            .field("name", &self.name)
            .field("generic_params", &self.generic_params)
            .field("where_clauses", &self.where_clause)
            .field("fields", &self.fields)
            .finish()
    }
}

impl std::fmt::Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("Attribute");

        f.field("tk_hash", &self.tk_hash);
        if let Some(tk_bang) = self.tk_bang.as_ref() {
            f.field("tk_bang", tk_bang);
        }
        f.field("tk_brackets", &self.tk_brackets);

        let path_token_refs: Vec<_> = self.path.iter().map(TokenRef).collect();
        f.field("path", &path_token_refs);

        f.field("value", &self.value);

        f.finish()
    }
}

impl std::fmt::Debug for AttributeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeValue::Group(tk_group, value) => {
                let mut f = f.debug_tuple("Group");
                let value_token_refs: Vec<_> = value.iter().map(TokenRef).collect();
                f.field(&value_token_refs);
                f.field(&tk_group);
                f.finish()
            }
            AttributeValue::Equals(tk_equals, value) => {
                let mut f = f.debug_tuple("Equals");
                let value_token_refs: Vec<_> = value.iter().map(TokenRef).collect();
                f.field(&value_token_refs);
                f.field(&tk_equals);
                f.finish()
            }
            AttributeValue::Empty => f.write_str("Empty"),
        }
    }
}

impl std::fmt::Debug for VisMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.tk_token2 {
            None => f.write_str(&self.tk_token1.to_string()),
            Some(TokenTree::Group(group)) => {
                let mut list = f.debug_tuple(&self.tk_token1.to_string());
                for token in group.stream() {
                    list.field(&TokenRef(&token));
                }
                list.finish()
            }
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Debug for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("PathSegment");
        if self.tk_separator_colons.is_some() {
            f.field("tk_separator_colons", &"::");
        }
        f.field("ident", &self.ident);
        if let Some(args) = self.generic_args.as_ref() {
            f.field("generic_args", args);
        }
        f.finish()
    }
}

impl std::fmt::Debug for GenericParamList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.params.fmt(f)
    }
}

impl std::fmt::Debug for GenericParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("GenericParam");
        if let Some(prefix) = self.tk_prefix.as_ref() {
            f.field("tk_prefix", &prefix.to_string());
        }
        f.field("name", &self.name.to_string());
        f.field("bound", &self.bound);
        f.finish()
    }
}

impl std::fmt::Debug for GenericBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_debug_tokens(f, &self.tokens)
    }
}

impl std::fmt::Debug for GenericArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("GenericArgList");
        if self.tk_turbofish_colons.is_some() {
            f.field("tk_turbofish_colons", &"::");
        }
        f.field("args", &self.args);
        f.finish()
    }
}

impl std::fmt::Debug for WhereClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.items.fmt(f)
    }
}

impl std::fmt::Debug for WhereClausePredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in quote::quote!(#self) {
            list.entry(&TokenRef(&token));
        }
        list.finish()
    }
}

impl std::fmt::Debug for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_debug_tokens(f, &self.tokens)
    }
}

impl std::fmt::Debug for ValueExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_debug_tokens(f, &self.tokens)
    }
}

impl std::fmt::Debug for GroupSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.delimiter {
            Delimiter::Parenthesis => f.write_str("()"),
            Delimiter::Brace => f.write_str("{}"),
            Delimiter::Bracket => f.write_str("[]"),
            Delimiter::None => f.write_str("Ø"),
        }
    }
}

// --- ToTokens impls ---

impl GroupSpan {
    fn quote_with(&self, tokens: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        let mut inner = TokenStream::new();
        f(&mut inner);
        let mut g = Group::new(self.delimiter, inner);
        g.set_span(self.span);
        tokens.append(g);
    }
}

impl ToTokens for Item {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Item::Struct(struct_decl) => struct_decl.to_tokens(tokens),
            Item::Enum(enum_decl) => enum_decl.to_tokens(tokens),
            Item::Union(union_decl) => union_decl.to_tokens(tokens),
            Item::Module(mod_decl) => mod_decl.to_tokens(tokens),
            Item::Trait(trait_decl) => trait_decl.to_tokens(tokens),
            Item::Impl(impl_decl) => impl_decl.to_tokens(tokens),
            Item::TypeAlias(ty_decl) => ty_decl.to_tokens(tokens),
            Item::Function(function_decl) => function_decl.to_tokens(tokens),
            Item::Constant(const_decl) => const_decl.to_tokens(tokens),
            Item::UseDeclaration(use_decl) => use_decl.to_tokens(tokens),
            Item::Macro(macro_decl) => macro_decl.to_tokens(tokens),
            Item::ExternBlock(block_decl) => block_decl.to_tokens(tokens),
            Item::ExternCrate(crate_decl) => crate_decl.to_tokens(tokens),
        }
    }
}

impl ToTokens for Struct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_struct.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);

        if matches!(&self.fields, Fields::Named(_)) {
            self.where_clause.to_tokens(tokens);
            self.fields.to_tokens(tokens);
        } else {
            self.fields.to_tokens(tokens);
            self.where_clause.to_tokens(tokens);
            self.tk_semicolon.to_tokens(tokens);
        }
    }
}

impl ToTokens for Fields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Fields::Unit => (),
            Fields::Tuple(fields) => fields.to_tokens(tokens),
            Fields::Named(fields) => fields.to_tokens(tokens),
        }
    }
}

impl ToTokens for TupleFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_parens.quote_with(tokens, |tokens| {
            self.fields.to_tokens(tokens);
        });
    }
}

impl ToTokens for NamedFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_braces.quote_with(tokens, |tokens| {
            self.fields.to_tokens(tokens);
        });
    }
}

impl ToTokens for Enum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_enum.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.tk_braces.quote_with(tokens, |tokens| {
            self.variants.to_tokens(tokens);
        });
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.fields.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

impl ToTokens for Constant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_const_or_static.to_tokens(tokens);
        self.tk_mut.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.tk_colon.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self.tk_equals.to_tokens(tokens);
        self.initializer.to_tokens(tokens);
        self.tk_semicolon.to_tokens(tokens);
    }
}

impl ToTokens for TypeAlias {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_type.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.bound.to_tokens(tokens);
        self.tk_equals.to_tokens(tokens);
        self.initializer_ty.to_tokens(tokens);
        self.tk_semicolon.to_tokens(tokens);
    }
}

impl ToTokens for Union {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_union.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.fields.to_tokens(tokens);
    }
}

impl ToTokens for Module {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_unsafe.to_tokens(tokens);
        self.tk_mod.to_tokens(tokens);
        self.name.to_tokens(tokens);

        if let Some(tk_semicolon) = self.tk_semicolon.as_ref() {
            tk_semicolon.to_tokens(tokens);
        } else if let Some(tk_braces) = self.tk_braces.as_ref() {
            tk_braces.quote_with(tokens, |tokens| {
                for attribute in &self.inner_attributes {
                    attribute.to_tokens(tokens);
                }
                for token in &self.members {
                    token.to_tokens(tokens);
                }
            });
        }
    }
}

impl ToTokens for Trait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_unsafe.to_tokens(tokens);
        self.tk_trait.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.bound.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.tk_braces.quote_with(tokens, |tokens| {
            for attribute in &self.inner_attributes {
                attribute.to_tokens(tokens);
            }

            for item in self.body_items.iter() {
                item.to_tokens(tokens)
            }
        });
    }
}

impl ToTokens for Impl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.tk_unsafe.to_tokens(tokens);
        self.tk_impl.to_tokens(tokens);
        self.impl_generic_params.to_tokens(tokens);
        self.trait_ty.to_tokens(tokens);
        self.tk_for.to_tokens(tokens);
        self.self_ty.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);
        self.tk_braces.quote_with(tokens, |tokens| {
            for attribute in &self.inner_attributes {
                attribute.to_tokens(tokens);
            }

            for item in self.body_items.iter() {
                item.to_tokens(tokens)
            }
        });
    }
}

impl ToTokens for ImplMember {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ImplMember::AssocFunction(function) => function.to_tokens(tokens),
            ImplMember::AssocConstant(constant) => constant.to_tokens(tokens),
            ImplMember::AssocType(assoc_ty) => assoc_ty.to_tokens(tokens),
            ImplMember::Macro(macro_) => macro_.to_tokens(tokens),
        }
    }
}

impl ToTokens for TraitMember {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TraitMember::AssocFunction(function) => function.to_tokens(tokens),
            TraitMember::AssocConstant(constant) => constant.to_tokens(tokens),
            TraitMember::AssocType(assoc_ty) => assoc_ty.to_tokens(tokens),
            TraitMember::Macro(macro_) => macro_.to_tokens(tokens),
        }
    }
}

impl ToTokens for Function {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.qualifiers.to_tokens(tokens);
        self.tk_fn_keyword.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generic_params.to_tokens(tokens);
        self.tk_params_parens.quote_with(tokens, |tokens| {
            self.params.to_tokens(tokens);
        });
        if let Some([dash, tip]) = self.tk_return_arrow.as_ref() {
            dash.to_tokens(tokens);
            tip.to_tokens(tokens);
        }
        self.return_ty.to_tokens(tokens);
        self.where_clause.to_tokens(tokens);

        // Note: branches are deliberately not exclusive, if user changes layout when passing around signatures
        if let Some(body) = self.body.as_ref() {
            body.to_tokens(tokens);
        }
        if let Some(semicolon) = self.tk_semicolon.as_ref() {
            semicolon.to_tokens(tokens);
        }
    }
}

impl ToTokens for FnQualifiers {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_default.to_tokens(tokens);
        self.tk_const.to_tokens(tokens);
        self.tk_async.to_tokens(tokens);
        self.tk_unsafe.to_tokens(tokens);
        self.tk_extern.to_tokens(tokens);
        self.extern_abi.to_tokens(tokens);
    }
}

impl ToTokens for FnParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FnParam::Receiver(param) => param.to_tokens(tokens),
            FnParam::Typed(param) => param.to_tokens(tokens),
        }
    }
}

impl ToTokens for FnReceiverParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.tk_ref.to_tokens(tokens);
        self.lifetime.to_tokens(tokens);
        self.tk_mut.to_tokens(tokens);
        self.tk_self.to_tokens(tokens);
    }
}

impl ToTokens for FnTypedParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.tk_mut.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.tk_colon.to_tokens(tokens);
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
        self.tk_colon.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for Attribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_hash.to_tokens(tokens);
        if let Some(tk_bang) = self.tk_bang.as_ref() {
            tk_bang.to_tokens(tokens);
        }

        self.tk_brackets.quote_with(tokens, |tokens| {
            for token in &self.path {
                token.to_tokens(tokens);
            }

            self.value.to_tokens(tokens);
        });
    }
}

impl ToTokens for AttributeValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            AttributeValue::Group(tk_group, value) => {
                tk_group.quote_with(tokens, |tokens| {
                    for token in value {
                        token.to_tokens(tokens);
                    }
                });
            }
            AttributeValue::Equals(tk_equals, value) => {
                tk_equals.to_tokens(tokens);
                for token in value {
                    token.to_tokens(tokens);
                }
            }
            AttributeValue::Empty => (),
        }
    }
}

impl ToTokens for VisMarker {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_token1.to_tokens(tokens);
        self.tk_token2.to_tokens(tokens);
    }
}

impl ToTokens for Path {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for segment in &self.segments {
            segment.to_tokens(tokens);
        }
    }
}

impl ToTokens for PathSegment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(colons) = &self.tk_separator_colons {
            tokens.append(colons[0].clone());
            tokens.append(colons[1].clone());
        }

        self.ident.to_tokens(tokens);
        self.generic_args.to_tokens(tokens);
    }
}

impl ToTokens for GenericParamList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.tk_l_bracket.clone());
        self.params.to_tokens(tokens);
        tokens.append(self.tk_r_bracket.clone());
    }
}

impl ToTokens for GenericParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_prefix.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.bound.to_tokens(tokens);
    }
}

impl ToTokens for GenericBound {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_colon.to_tokens(tokens);
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for GenericArgList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(colons) = &self.tk_turbofish_colons {
            tokens.append(colons[0].clone());
            tokens.append(colons[1].clone());
        }
        tokens.append(self.tk_l_bracket.clone());
        self.args.to_tokens(tokens);
        tokens.append(self.tk_r_bracket.clone());
    }
}

impl ToTokens for GenericArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            GenericArg::Lifetime { lifetime } => {
                lifetime.to_tokens(tokens);
            }
            GenericArg::Binding {
                ident,
                tk_equals,
                ty,
            } => {
                ident.to_tokens(tokens);
                tk_equals.to_tokens(tokens);
                ty.to_tokens(tokens);
            }
            GenericArg::TypeOrConst { expr } => {
                expr.to_tokens(tokens);
            }
        }
    }
}

impl ToTokens for InlineGenericArgs<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(Punct::new('<', Spacing::Alone));

        for param in &self.0.params.inner {
            if param.0.is_lifetime() {
                param.0.tk_prefix.to_tokens(tokens);
            }
            tokens.append(param.0.name.clone());
            tokens.append(Punct::new(',', Spacing::Alone));
        }

        tokens.append(Punct::new('>', Spacing::Alone));
    }
}

impl ToTokens for Lifetime {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.tk_apostrophe.clone());
        tokens.append(self.name.clone());
    }
}

impl ToTokens for WhereClause {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.tk_where.clone());
        self.items.to_tokens(tokens);
    }
}

impl ToTokens for WhereClausePredicate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.left_side {
            tokens.append(token.clone());
        }
        self.bound.to_tokens(tokens);
    }
}

impl ToTokens for TypeExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for ValueExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for token in &self.tokens {
            tokens.append(token.clone());
        }
    }
}

impl ToTokens for UseDeclaration {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_use.to_tokens(tokens);
        self.import_tree.to_tokens(tokens);
        self.tk_semicolon.to_tokens(tokens);
    }
}

impl ToTokens for EnumVariantValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tk_equal.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

impl ToTokens for Macro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.name.to_tokens(tokens);
        self.tk_bang.to_tokens(tokens);
        self.tk_declared_name.to_tokens(tokens);
        self.tk_braces_or_parens.quote_with(tokens, |tokens| {
            tokens.extend(self.inner_tokens.iter().cloned());
        });
        self.tk_semicolon.to_tokens(tokens);
    }
}

impl ToTokens for ExternBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_unsafe.to_tokens(tokens);
        self.tk_extern.to_tokens(tokens);
        self.extern_abi.to_tokens(tokens);
        self.tk_braces.quote_with(tokens, |tokens| {
            for attribute in &self.inner_attributes {
                attribute.to_tokens(tokens);
            }
            for item in &self.body_items {
                item.to_tokens(tokens);
            }
        });
    }
}

impl ToTokens for ExternCrate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attribute in &self.attributes {
            attribute.to_tokens(tokens);
        }
        self.vis_marker.to_tokens(tokens);
        self.tk_extern.to_tokens(tokens);
        self.tk_crate.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.tk_as.to_tokens(tokens);
        self.alias.to_tokens(tokens);
        self.tk_underscore.to_tokens(tokens);
        self.tk_semicolon.to_tokens(tokens);
    }
}

// --- Default impls ---

impl Default for GenericParamList {
    fn default() -> Self {
        Self {
            tk_l_bracket: Punct::new('<', Spacing::Alone),
            params: Punctuated::new(),
            tk_r_bracket: Punct::new('>', Spacing::Alone),
        }
    }
}

impl Default for WhereClause {
    fn default() -> Self {
        // Note that an empty where clause is perfectly valid syntax
        Self {
            tk_where: Ident::new("where", Span::call_site()),
            items: Punctuated::new(),
        }
    }
}
