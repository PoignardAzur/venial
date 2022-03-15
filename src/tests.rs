use crate::{parse_declaration, Declaration, GenericParam, Struct, WhereClauseItem};

use insta::assert_debug_snapshot;
use proc_macro2::TokenStream;
use quote::quote;

// TODO - check that ToTokens implementations are correct

// =============
// BASIC PARSING
// =============

#[test]
fn parse_unit_struct() {
    let struct_type = parse_declaration(quote!(
        struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct() {
    let struct_type = parse_declaration(quote!(
        struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct() {
    let struct_type = parse_declaration(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum() {
    let enum_type = parse_declaration(quote!(
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
    let union_type = parse_declaration(quote!(
        union Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(union_type);
}

#[test]
fn parse_empty_tuple() {
    let struct_type = parse_declaration(quote!(
        struct Hello();
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_struct() {
    let struct_type = parse_declaration(quote!(
        struct Hello {}
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_enum() {
    let enum_type = parse_declaration(quote!(
        enum Hello {}
    ));

    assert_debug_snapshot!(enum_type);
}

// ==========
// VISIBILITY
// ==========

#[test]
fn parse_unit_struct_vis() {
    let struct_type_pub = parse_declaration(quote!(
        pub struct Hello;
    ));
    let struct_type_pub_crate = parse_declaration(quote!(
        pub(crate) struct Hello;
    ));
    let struct_type_crate = parse_declaration(quote!(
        crate struct Hello;
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_tuple_struct_vis() {
    let struct_type_pub = parse_declaration(quote!(
        pub struct Hello(A, B);
    ));
    let struct_type_pub_crate = parse_declaration(quote!(
        pub(crate) struct Hello(A, B);
    ));
    let struct_type_crate = parse_declaration(quote!(
        crate struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_normal_struct_vis() {
    let struct_type_pub = parse_declaration(quote!(
        pub struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_pub_crate = parse_declaration(quote!(
        pub(crate) struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_crate = parse_declaration(quote!(
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
    let enum_type_pub = parse_declaration(quote!(
        pub enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_pub_crate = parse_declaration(quote!(
        pub(crate) enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_crate = parse_declaration(quote!(
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
    let struct_type = parse_declaration(quote!(
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
    let struct_type = parse_declaration(quote!(
        pub struct Hello(pub A, pub(super) B, crate C, D);
    ));

    assert_debug_snapshot!(struct_type);
}

// ==========
// ATTRIBUTES
// ==========

#[test]
fn parse_unit_struct_attributes() {
    let struct_type = parse_declaration(quote!(
        #[hello]
        pub struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_attributes() {
    let struct_type = parse_declaration(quote!(
        #[hello]
        pub struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_attributes() {
    let struct_type = parse_declaration(quote!(
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
    let enum_type = parse_declaration(quote!(
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
    let struct_type = parse_declaration(quote!(
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
    let struct_type = parse_declaration(quote!(
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

// =============
// WHERE CLAUSES
// =============

#[test]
fn parse_unit_struct_where_clause() {
    let struct_type = parse_declaration(quote!(
        struct Hello
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_where_clause() {
    let struct_type = parse_declaration(quote!(
        struct Hello(A, B)
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_where_clause() {
    let struct_type = parse_declaration(quote!(
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
    let enum_type = parse_declaration(quote!(
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

// TODO - empty where clause

// ==============
// GENERIC PARAMS
// ==============

#[test]
fn parse_unit_struct_generic_params() {
    let struct_type = parse_declaration(quote!(
        struct Hello<X, Y: Z<A>, Z>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_generic_params() {
    let struct_type = parse_declaration(quote!(
        struct Hello<X, Y: Z<A>, Z>(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_generic_params() {
    let struct_type = parse_declaration(quote!(
        struct Hello<X, Y: Z<A>, Z> {
            a: A,
            b: B<X, Y, Z>,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum_generic_params() {
    let enum_type = parse_declaration(quote!(
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
    let enum_type = parse_declaration(quote!(
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

// =================
// ENUM DISCRIMINANT
// =================

#[test]
fn parse_enum_discriminant() {
    let enum_type = parse_declaration(quote!(
        enum Hello {
            A = 1,
            B(Foo, Bar) = call::some::function(1, 2, { 3 }),
            C { foo: Foo, bar: Bar } = A<B>(c) + { D },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

// =====================
// GENERICS CORNER CASES
// =====================

#[test]
fn parse_const_generics() {
    let struct_type = parse_declaration(quote!(
        struct Hello(Array<A, { 123 + (1, 2, 3) }>, B<{ 1 }, { 2 }>);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_fn_traits() {
    let struct_type = parse_declaration(quote!(
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
    let struct_type = parse_declaration(quote!(
        struct Hello(
            A<B<C>>,
            A<B<C, D>>,
            <<D as Trait>::X as Trait>::Y,
        );
    ));

    assert_debug_snapshot!(struct_type);
}

// =========
// FUNCTIONS
// =========

#[test]
fn parse_fn() {
    let func = parse_declaration(quote! {
        fn hello(a: i32, b: f32) -> String {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_empty_fn() {
    let func = parse_declaration(quote! {
        fn test_me() {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_generic_fn() {
    let func = parse_declaration(quote! {
        fn generic<T, B>(a: T) -> B {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_where_fn() {
    let func = parse_declaration(quote! {
        fn where_clause<T>() -> T
        where
            T: Debug
        {}
    });
    let func_2 = parse_declaration(quote! {
        fn where_clause<T>()
        where
            T: Debug
        {}
    });

    assert_debug_snapshot!(func);
    assert_debug_snapshot!(func_2);
}

#[test]
fn parse_attr_fn() {
    let func = parse_declaration(quote! {
        #[my_attr]
        fn my_attr_fn(a: i32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_visi_fn() {
    let func = parse_declaration(quote! {
        pub fn visibility(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_default_fn() {
    let func = parse_declaration(quote! {
        pub default fn default_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_const_fn() {
    let func = parse_declaration(quote! {
        pub const fn const_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_async_fn() {
    let func = parse_declaration(quote! {
        pub async fn async_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_unsafe_fn() {
    let func = parse_declaration(quote! {
        pub unsafe fn unsafe_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_extern_abi_fn() {
    let func = parse_declaration(quote! {
        pub extern "C" fn extern_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_extern_fn() {
    let func = parse_declaration(quote! {
        pub extern fn extern_fn(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_all_kw_fn() {
    let func = parse_declaration(quote! {
        pub default const async unsafe extern "C" fn all_kw(b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_param_attr_fn() {
    let func = parse_declaration(quote! {
        pub async fn visibility(#[my_attr] b: f32) {}
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_body() {
    let func = parse_declaration(quote! {
        fn hello_world(a: i32, b: f32) -> String {
            println!("hello world")
        }
    });

    assert_debug_snapshot!(func);
}

#[test]
fn parse_fn_prototype() {
    let func = parse_declaration(quote! {
        fn prototype(a: i32, b: f32) -> String;
    });

    assert_debug_snapshot!(func);
}

// ============
// TYPE EDITING
// ============

fn parse_struct_declaration(tokens: TokenStream) -> Struct {
    match parse_declaration(tokens) {
        Declaration::Struct(struct_decl) => struct_decl,
        _ => panic!("not a struct"),
    }
}

// TODO - assert_quote_snapshot

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

    assert_debug_snapshot!(basic_type);
    assert_debug_snapshot!(type_with_args);
    assert_debug_snapshot!(type_with_lifetime);
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
        quote!("c + d").into_iter().collect(),
    ));

    assert_debug_snapshot!(basic_type);
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

    assert_debug_snapshot!(basic_type);
    assert_debug_snapshot!(type_with_args);
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
        quote!("Clone").into_iter().collect(),
    ));

    assert_debug_snapshot!(basic_type);
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
        quote!("u32").into_iter().collect(),
    ));
    let type_with_args = type_with_args.with_param(GenericParam::const_param(
        "N",
        quote!("u32").into_iter().collect(),
    ));

    assert_debug_snapshot!(basic_type);
    assert_debug_snapshot!(type_with_args);
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

    assert_debug_snapshot!(basic_type);
    assert_debug_snapshot!(type_with_args);
}
