use crate::parse_type;

use insta::assert_debug_snapshot;
use quote::quote;

// TODO - test const generics {}
// TODO - test Fn()

// =============
// BASIC PARSING
// =============

#[test]
fn parse_unit_struct() {
    let struct_type = parse_type(quote!(
        struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct() {
    let struct_type = parse_type(quote!(
        struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct() {
    let struct_type = parse_type(quote!(
        struct Hello {
            a: A,
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum() {
    let enum_type = parse_type(quote!(
        enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

#[test]
fn parse_empty_tuple() {
    let struct_type = parse_type(quote!(
        struct Hello();
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_struct() {
    let struct_type = parse_type(quote!(
        struct Hello {}
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_empty_enum() {
    let enum_type = parse_type(quote!(
        enum Hello {}
    ));

    assert_debug_snapshot!(enum_type);
}

// ==========
// VISIBILITY
// ==========

#[test]
fn parse_unit_struct_vis() {
    let struct_type_pub = parse_type(quote!(
        pub struct Hello;
    ));
    let struct_type_pub_crate = parse_type(quote!(
        pub(crate) struct Hello;
    ));
    let struct_type_crate = parse_type(quote!(
        crate struct Hello;
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_tuple_struct_vis() {
    let struct_type_pub = parse_type(quote!(
        pub struct Hello(A, B);
    ));
    let struct_type_pub_crate = parse_type(quote!(
        pub(crate) struct Hello(A, B);
    ));
    let struct_type_crate = parse_type(quote!(
        crate struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type_pub);
    assert_debug_snapshot!(struct_type_pub_crate);
    assert_debug_snapshot!(struct_type_crate);
}

#[test]
fn parse_normal_struct_vis() {
    let struct_type_pub = parse_type(quote!(
        pub struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_pub_crate = parse_type(quote!(
        pub(crate) struct Hello {
            a: A,
            b: B,
        }
    ));
    let struct_type_crate = parse_type(quote!(
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
    let enum_type_pub = parse_type(quote!(
        pub enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_pub_crate = parse_type(quote!(
        pub(crate) enum Hello {
            A,
            B(Foo, Bar),
            C { foo: Foo, bar: Bar },
        }
    ));
    let enum_type_crate = parse_type(quote!(
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
    let struct_type = parse_type(quote!(
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
    let struct_type = parse_type(quote!(
        pub struct Hello(pub A, pub(super) B, crate C, D);
    ));

    assert_debug_snapshot!(struct_type);
}

// ==========
// ATTRIBUTES
// ==========

#[test]
fn parse_unit_struct_attributes() {
    let struct_type = parse_type(quote!(
        #[hello]
        pub struct Hello;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_attributes() {
    let struct_type = parse_type(quote!(
        #[hello]
        pub struct Hello(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_attributes() {
    let struct_type = parse_type(quote!(
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
    let enum_type = parse_type(quote!(
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
    let struct_type = parse_type(quote!(
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
    let struct_type = parse_type(quote!(
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
    let struct_type = parse_type(quote!(
        struct Hello
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_where_clause() {
    let struct_type = parse_type(quote!(
        struct Hello(A, B)
        where
            A: B,
            C<D>: E<F>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_where_clause() {
    let struct_type = parse_type(quote!(
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
    let enum_type = parse_type(quote!(
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

// ==============
// GENERIC PARAMS
// ==============

#[test]
fn parse_unit_struct_generic_params() {
    let struct_type = parse_type(quote!(
        struct Hello<X, Y: Z<A>, Z>;
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_tuple_struct_generic_params() {
    let struct_type = parse_type(quote!(
        struct Hello<X, Y: Z<A>, Z>(A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct_generic_params() {
    let struct_type = parse_type(quote!(
        struct Hello<X, Y: Z<A>, Z> {
            a: A,
            b: B<X, Y, Z>,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum_generic_params() {
    let enum_type = parse_type(quote!(
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
    let enum_type = parse_type(quote!(
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
    let enum_type = parse_type(quote!(
        enum Hello {
            A = 1,
            B(Foo, Bar) = call::some::function(1, 2, { 3 }),
            C { foo: Foo, bar: Bar } = A<B>(c) + { D },
        }
    ));

    assert_debug_snapshot!(enum_type);
}

// ==============
// CONST GENERICS
// ==============

#[test]
fn parse_const_generics() {
    let struct_type = parse_type(quote!(
        struct Hello(Array<A, { 123 + (1, 2, 3) }>, B<{ 1 }, { 2 }>);
    ));

    assert_debug_snapshot!(struct_type);
}

// =========
// FN TRAITS
// =========

#[test]
fn parse_fn_traits() {
    let struct_type = parse_type(quote!(
        struct Hello(
            fn(A, B) -> C,
            A<FnOnce(A, B, C<D>) -> E,
        );
    ));

    assert_debug_snapshot!(struct_type);
}

// TODO - >>
// TODO - <<
