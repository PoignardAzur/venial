use crate::{parse_declaration, Declaration, GenericParam, Struct, TyExpr, WhereClauseItem};

use crate::parse_type::consume_generic_args;
use crate::types::GenericArgList;
use insta::assert_debug_snapshot;
use proc_macro2::TokenStream;
use quote::quote;

// TODO - check test coverage

macro_rules! assert_quote_snapshot {
    ($item:expr) => {{
        use quote::ToTokens;
        let tokens = ($item).to_token_stream();
        ::insta::assert_display_snapshot!(tokens);
    }};
}

fn parse_declaration_checked(tokens: TokenStream) -> Declaration {
    let initial_tokens = tokens.clone();
    let declaration = parse_declaration(tokens).unwrap();

    similar_asserts::assert_str_eq!(quote!(#declaration), initial_tokens);

    declaration
}

fn parse_generic_args_checked(tokens: TokenStream) -> GenericArgList {
    let initial_tokens = tokens.clone();

    let mut token_iter = tokens.into_iter().peekable();
    let generic_args = consume_generic_args(&mut token_iter).unwrap();

    similar_asserts::assert_str_eq!(quote!(#generic_args), initial_tokens);

    generic_args
}

// =============
// BASIC PARSING
// =============

#[test]
fn parse_unit_struct() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

#[test]
fn parse_union() {
    let union_type = parse_declaration_checked(quote!(
        union Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(union_type);
}

#[test]
fn parse_empty_tuple() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello();
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_struct() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello {}
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_enum() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello {}
    ));

    assert_debug_snapshot!(enum_type);
}

// ==========
// VISIBILITY
// ==========

#[test]
fn parse_unit_struct_vis() {
    let struct_type_pub = parse_declaration_checked(quote!(
        pub struct Hello;
    ));
    let struct_type_pub_crate = parse_declaration_checked(quote!(
        pub(crate) struct Hello;
    ));
    let struct_type_crate = parse_declaration_checked(quote!(
        crate struct Hello;
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_tuple_struct_vis() {
    let struct_type_pub = parse_declaration_checked(quote!(
        pub struct Hello(A, B);
    ));
    let struct_type_pub_crate = parse_declaration_checked(quote!(
        pub(crate) struct Hello(A, B);
    ));
    let struct_type_crate = parse_declaration_checked(quote!(
        crate struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_normal_struct_vis() {
    let struct_type_pub = parse_declaration_checked(quote!(
        pub struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_pub_crate = parse_declaration_checked(quote!(
        pub(crate) struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_crate = parse_declaration_checked(quote!(
        crate struct Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_enum_vis() {
    let enum_type_pub = parse_declaration_checked(quote!(
        pub enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_pub_crate = parse_declaration_checked(quote!(
        pub(crate) enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_crate = parse_declaration_checked(quote!(
        crate enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type_pub);
    assert_debug_snapshot!(enum_type_pub_crate);
    assert_debug_snapshot!(enum_type_crate);
}

#[test]
fn parse_struct_fields_vis() {
    let struct_type = parse_declaration_checked(quote!(
        pub struct Hello {
            pub a: A,
            pub(super) b: B,
            crate c: C,
            d: D,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_fields_vis() {
    let struct_type = parse_declaration_checked(quote!(
        pub struct Hello(pub A, pub(super) B, crate C, D);
    ));

    assert_debug_snapshot!(struct_type);
}

// ==========
// ATTRIBUTES
// ==========

#[test]
fn parse_unit_struct_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        #[hello]
        pub struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        #[hello]
        pub struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        #[hello]
        pub struct Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum_attributes() {
    let enum_type = parse_declaration_checked(quote!(
        #[hello]
        pub enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

#[test]
fn parse_struct_fields_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        pub struct Hello {
            #[hello]
            a: A,
            b: B,
            #[hello]
            #[hello]
            #[hello]
            c: C,
            d: D,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_fields_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        pub struct Hello(
            #[hello] A,
            B,
            #[hello]
            #[hello]
            #[hello]
            C,
            D,
        );
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_complex_attributes() {
    let struct_type = parse_declaration_checked(quote!(
        #[hello::world]
        #[hello = a + b]
        #[hello::world = a + b]
        #[hello(a b c)]
        #[hello::world(a b c)]
        pub struct Hello;
    ));

    assert_debug_snapshot!(struct_type);

    assert_debug_snapshot!(struct_type.attributes()[0].get_single_path_segment());
    assert_debug_snapshot!(struct_type.attributes()[1].get_single_path_segment());
}

// =============
// WHERE CLAUSES
// =============

#[test]
fn parse_unit_struct_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(A, B)
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello
        where
            A: B,
            C<D>: E<F>,
        {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum_where_clause() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello
        where
            A: B,
            C<D>: E<F>,
        {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

#[rustfmt::skip]
#[test]
fn parse_unit_struct_empty_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello where;
    ));

    assert_debug_snapshot!(struct_type);
}

#[rustfmt::skip]
#[test]
fn parse_tuple_struct_empty_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(A, B) where;
    ));

    assert_debug_snapshot!(struct_type);
}

#[rustfmt::skip]
#[test]
fn parse_normal_struct_empty_where_clause() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello
        where
        {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[rustfmt::skip]
#[test]
fn parse_enum_empty_where_clause() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello
        where
        {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

// ==============
// GENERIC PARAMS
// ==============

#[test]
fn parse_unit_struct_generic_params() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello<X, Y: Z<A>, Z>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_generic_params() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello<X, Y: Z<A>, Z>(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_generic_params() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello<X, Y: Z<A>, Z> {
            a: A,
            b: B<X, Y, Z>,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum_generic_params() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello<X, Y: Z<A>, Z> {
            A,
            B(Foo, Bar<X, Y, Z>),
            C { foo: Foo<X, Y, Z>, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

#[rustfmt::skip]
#[test]
fn parse_enum_empty_generic_params() {
    let enum_type = parse_declaration_checked(quote!(
        enum Hello<>
        where
            A<>: B<>,
        {
            A,
            B(Foo, Bar<>),
            C { foo: Foo<>, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

// ============
// GENERIC ARGS
// ============

#[test]
fn parse_generic_args() {
    let generic_args = parse_generic_args_checked(quote!(
        <'a, path::to::Type, 15, Item = i32, module::NestedType<another::Type>>
    ));

    assert_debug_snapshot!(generic_args);
}

#[test]
fn parse_generic_args_turbofish() {
    let generic_args = parse_generic_args_checked(quote!(
        ::<>
    ));

    assert_debug_snapshot!(generic_args);
}

#[test]
fn parse_inline_generic_args() {
    let struct_decl = parse_declaration_checked(quote!(
        struct Hello<'a: 'static, T, U: Clone, const N: usize> {}
    ));

    let params = struct_decl.generic_params().unwrap();
    assert_debug_snapshot!(params);

    let inline_args = params.as_inline_args();
    let owned_args = inline_args.to_owned_args();
    assert_debug_snapshot!(owned_args);
}

// ==================
// ENUM VARIANT VALUE
// ==================

#[test]
fn parse_complex_enum_variant() {
    let enum_type_1 = parse_declaration(quote!(
        enum Hello {
            A = 1,
            B(Foo, Bar) = 1 + 2 + 3,
        }
    ));
    let enum_type_2 = parse_declaration(quote!(
        enum Hello {
            A = 1,
            B { foo: Foo, bar: Bar } = foo(bar),
        }
    ));
    let enum_type_3 = parse_declaration(quote!(
        enum Hello {
            A = 1,
            B(Foo, Bar) = [1 + 2 + 3],
            C { foo: Foo, bar: Bar } = (foo(bar)),
        }
    ));

    assert_debug_snapshot!(enum_type_1);
    assert_debug_snapshot!(enum_type_2);
    assert_debug_snapshot!(enum_type_3);
}

// =================
// TYPE CORNER CASES
// =================

#[test]
fn parse_const_generics() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(Array<A, { 123 + (1, 2, 3) }>, B<{ 1 }, { 2 }>);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_fn_traits() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(
            fn(A, B) -> C,
            A<FnOnce(A, B, C<D>) -> E,
        );
    ));

    assert_debug_snapshot!(struct_type);
}

#[rustfmt::skip]
#[test]
fn parse_multiple_brackets() {
    let struct_type = parse_declaration_checked(quote!(
        struct Hello(
            A<B<C>>,
            A<B<C, D>>,
            <<D as Trait>::X as Trait>::Y,
        );
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_bounded_type_param_no_comma() {
    let struct_type = parse_declaration_checked(quote!(
        struct Example<A: Clone> {
            _test: A,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

// FIXME
#[test]
#[should_panic]
fn parse_macro_in_where_clause() {
    // venial thinks the content of the macro is the content of the enum
    // so it tries to parse `:` as a variant
    let enum_type = parse_declaration_checked(quote!(
        enum A
        where
            b: c! { : }, {}
    ));

    assert_debug_snapshot!(enum_type);
}

// =========
// FUNCTIONS
// =========

#[test]
fn parse_fn() {
    let func = parse_declaration(quote!(
        fn hello(a: i32, b: f32) -> String {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_empty_fn() {
    let func = parse_declaration(quote!(
        fn test_me() {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_generic_fn() {
    let func = parse_declaration(quote!(
        fn generic<T, B>(a: T) -> B {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_where_fn() {
    let func = parse_declaration(quote!(
        fn where_clause<T>() -> T
        where
            T: Debug,
        {
        }
    ))
    .unwrap();
    let func_2 = parse_declaration(quote!(
        fn where_clause<T>()
        where
            T: Debug,
        {
        }
    ))
    .unwrap();

    assert_debug_snapshot!(func);
    assert_debug_snapshot!(func_2);
}

#[test]
fn parse_attr_fn() {
    let func = parse_declaration(quote!(
        #[my_attr]
        fn my_attr_fn(a: i32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_visi_fn() {
    let func = parse_declaration(quote!(
        pub fn visibility(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_default_fn() {
    let func = parse_declaration(quote!(
        pub default fn default_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_const_fn() {
    let func = parse_declaration(quote!(
        pub const fn const_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_async_fn() {
    let func = parse_declaration(quote!(
        pub async fn async_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_unsafe_fn() {
    let func = parse_declaration(quote!(
        pub unsafe fn unsafe_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_extern_abi_fn() {
    let func = parse_declaration(quote!(
        pub extern "C" fn extern_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_extern_fn() {
    #[rustfmt::skip] // would add "C"
    let func = parse_declaration(quote!(
        pub extern fn extern_fn(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_all_kw_fn() {
    let func = parse_declaration(quote!(
        pub default const async unsafe extern "C" fn all_kw(b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_param_attr_fn() {
    let func = parse_declaration(quote!(
        pub async fn visibility(#[my_attr] b: f32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_body() {
    let func = parse_declaration(quote!(
        fn hello_world(a: i32, b: f32) -> String {
            println!("hello world")
        }
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_prototype() {
    let func = parse_declaration(quote!(
        fn prototype(a: i32, b: f32) -> String;
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_mut_param() {
    let func = parse_declaration(quote!(
        fn prototype(a: i32, mut b: f32) -> String;
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_lifetimes() {
    let func = parse_declaration(quote!(
        fn prototype<'a>(a: &'a mut i32) -> &'a String;
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

// FIXME
#[test]
#[should_panic]
fn parse_fn_pattern_arg() {
    let func = parse_declaration(quote!(
        fn foobar((a, b): (i32, i32)) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

// FIXME
#[test]
#[should_panic]
fn parse_fn_c_variadics() {
    let func = parse_declaration(quote!(
        fn foobar(a: i32, ...) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

// FIXME
#[test]
#[should_panic]
fn parse_fn_no_pattern() {
    let func = parse_declaration(quote!(
        fn foobar(i32) {}
    ))
    .unwrap();

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_self_param() {
    let func_self = parse_declaration(quote!(
        fn foobar(self) {}
    ));
    let func_ref_self = parse_declaration(quote!(
        fn foobar(&self) {}
    ));
    let func_mut_self = parse_declaration(quote!(
        fn foobar(mut self) {}
    ));
    let func_ref_mut_self = parse_declaration(quote!(
        fn foobar(&mut self) {}
    ));

    assert_debug_snapshot!(func_self);
    assert_debug_snapshot!(func_ref_self);
    assert_debug_snapshot!(func_mut_self);
    assert_debug_snapshot!(func_ref_mut_self);
}

// ============
// TYPE EDITING
// ============

fn parse_struct_declaration(tokens: TokenStream) -> Struct {
    match parse_declaration(tokens).unwrap() {
        Declaration::Struct(struct_decl) => struct_decl,
        _ => panic!("not a struct"),
    }
}

#[test]
fn add_lifetime() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));
    let type_with_args = parse_struct_declaration(quote!(
        struct Hello<A, B> {
            a: A,
            b: B,
        }
    ));
    let type_with_lifetime = parse_struct_declaration(quote!(
        struct Hello<'a, A, B> {
            a: &'a A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_param(GenericParam::lifetime("b"));
    let type_with_args = type_with_args.with_param(GenericParam::lifetime("b"));
    let type_with_lifetime = type_with_lifetime.with_param(GenericParam::lifetime("b"));

    assert_quote_snapshot!(basic_type);
    assert_quote_snapshot!(type_with_args);
    assert_quote_snapshot!(type_with_lifetime);
}

#[test]
fn add_bounded_lifetime() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_param(GenericParam::bounded_lifetime(
        "b",
        quote!(c + d).into_iter().collect(),
    ));

    assert_quote_snapshot!(basic_type);
}

#[test]
fn add_ty_param() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));
    let type_with_args = parse_struct_declaration(quote!(
        struct Hello<A, B> {
            a: A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_param(GenericParam::ty("T"));
    let type_with_args = type_with_args.with_param(GenericParam::ty("T"));

    assert_quote_snapshot!(basic_type);
    assert_quote_snapshot!(type_with_args);
}

#[test]
fn add_bounded_ty_param() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_param(GenericParam::bounded_ty(
        "T",
        quote!(Clone).into_iter().collect(),
    ));

    assert_quote_snapshot!(basic_type);
}

#[test]
fn add_const_param() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));
    let type_with_args = parse_struct_declaration(quote!(
        struct Hello<A, B> {
            a: A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_param(GenericParam::const_param(
        "N",
        quote!(u32).into_iter().collect(),
    ));
    let type_with_args = type_with_args.with_param(GenericParam::const_param(
        "N",
        quote!(u32).into_iter().collect(),
    ));

    assert_quote_snapshot!(basic_type);
    assert_quote_snapshot!(type_with_args);
}

#[test]
fn add_where_item() {
    let basic_type = parse_struct_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));
    let type_with_args = parse_struct_declaration(quote!(
        struct Hello<A, B>
        where
            A: Clone,
            B: Clone,
        {
            a: A,
            b: B,
        }
    ));

    let basic_type = basic_type.with_where_item(WhereClauseItem::parse(quote!(Self: Sized)));
    let type_with_args =
        type_with_args.with_where_item(WhereClauseItem::parse(quote!(Self: Sized)));

    assert_quote_snapshot!(basic_type);
    assert_quote_snapshot!(type_with_args);
}

// =================
// IMPL DECLARATIONS
// =================

#[test]
fn parse_impl_inherent() {
    let expr = quote!(
        #[outer]
        impl MyStruct {
            #![inner]

            fn new(i: i32, b: bool) -> Self {
                Self {}
            }

            #[attr]
            pub(crate) fn set_value(&mut self, s: String) {}

            pub const CONSTANT: i8 = 24 + 7;
        }
    );

    let impl_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(impl_decl);
}

#[test]
fn parse_impl_inherent_generic() {
    let expr = quote!(
        impl<'a, T: Clone, const N: i8> structs::MyStruct<'a, T, N> {}
    );

    let impl_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(impl_decl);
}

#[test]
fn parse_impl_trait() {
    let expr = quote!(
        #[outer]
        impl MyTrait for MyStruct {
            #![inner]
            #![inner2]

            #[attr]
            #[attr2]
            pub type MyType = std::string::String;

            fn new(i: i32, b: bool) -> Self {
                Self {}
            }

            #[attr]
            const fn set_value(&mut self, s: String) {}

            const CONSTANT: i8 = 24 + 7;
        }
    );

    let impl_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(impl_decl);
}

#[test]
fn parse_impl_trait_generic() {
    let expr = quote!(
        unsafe impl<'a, T, const N: i8> traits::MyTrait<T, N> for structs::MyStruct<'a, T>
        where
            T: Clone,
        {
            #[attr]
            pub(crate) const CONSTANT: i8 = N;
        }
    );

    let impl_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(impl_decl);
}

// =================
// TYPE DECLARATIONS
// =================

#[test]
fn parse_type_simple() {
    let expr = quote!(
        type MyType = std::string::String;
    );

    let ty_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(ty_decl);
}

#[test]
fn parse_type_complex() {
    let expr = quote!(
        #[attribute]
        pub(crate) type MyType = some::module::OtherType<i32, 10>::AssocType<Item = char>;
    );

    let ty_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(ty_decl);
}

// =====================
// CONSTANT DECLARATIONS
// =====================

#[test]
fn parse_constant_simple() {
    let expr = quote!(
        const CONSTANT: i32 = 20;
    );

    let const_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(const_decl);
}

#[test]
fn parse_constant_complex() {
    let expr = quote!(
        #[attribute]
        pub(crate) const CONSTANT: some::complex::Type<'static, bool> = some::complex::Expr + 77;
    );

    let const_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(const_decl);
}

// Syntactically valid in venial
#[test]
fn parse_constant_expressionless() {
    let expr = quote!(
        const CONSTANT: some::Type;
    );

    let const_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(const_decl);
}

// =====================
// TYPE PATH EXPRESSIONS
// =====================

#[test]
fn interpret_ty_expr_simple_as_path() {
    let tokens = quote!(::path::to::Type);
    let ty_expr = TyExpr {
        tokens: tokens.clone().into_iter().collect(),
    };

    let path = ty_expr.as_path().expect("as_path()");
    assert_debug_snapshot!(path);
    similar_asserts::assert_str_eq!(quote!(#ty_expr), tokens);
}

#[test]
fn interpret_ty_expr_generic_as_path() {
    #[rustfmt::skip] // would remove trailing ::<>
    let tokens = quote!(path::to::Type<'a, other::Arg, 36>::Turbofish::<>);
    let ty_expr = TyExpr {
        tokens: tokens.clone().into_iter().collect(),
    };

    let path = ty_expr.as_path().expect("as_path()");
    assert_debug_snapshot!(path);
    similar_asserts::assert_str_eq!(quote!(#ty_expr), tokens);
}

#[test]
fn interpret_ty_expr_invalid_as_path() {
    let tokens = quote!(());
    let ty_expr = TyExpr {
        tokens: tokens.into_iter().collect(),
    };

    let invalid = ty_expr.as_path();
    assert!(invalid.is_none())
}

// ================
// MOD DECLARATIONS
// ================

#[test]
fn parse_mod() {
    let expr = quote! {
        #[path = "some/module"]
        mod one_module {
            #![allow(clippy::iter_with_drain)]

            use std::clone::Clone as Clown;
            use std::cell::{Cell, RefCell};

            pub struct MyStruct {
                field: i32,
            }
            impl MyStruct {}
            impl MyTrait for MyStruct {}

            #[derive(Copy, Clone)]
            enum Enum {
                Variant,
            }

            fn f() -> bool { true }

            pub use crate::{A, self, b::c};

            const C: i32 = -8 * 2;

            pub static MUTEX: std::sync::Mutex<i32> = std::sync::Mutex::new(0);

            type MyType = Rc<RefCell<MyStruct>>;

            mod nested_mod {
                fn g() {}
            }

            mod empty_mod {}
            pub(crate) mod foreign_mod;

            #[contain_it]
            unsafe mod hazard_mod {}
        }
    };

    let mod_decl = parse_declaration_checked(expr);
    assert_debug_snapshot!(mod_decl);
}
