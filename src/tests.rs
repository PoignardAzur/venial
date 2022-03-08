use crate::parse_type;

use insta::assert_debug_snapshot;
use quote::quote;

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
        struct Hello(#[abc] A, B);
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_normal_struct() {
    let struct_type = parse_type(quote!(
        struct Hello {
            a: A,
            #[abc]
            b: B,
        }
    ));

    assert_debug_snapshot!(struct_type);
}

#[test]
fn parse_enum() {
    let enum_type = parse_type(quote!(
        enum Hello {
            #[abc]
            A,
            B(Foo, Bar),
            C {
                #[abc]
                foo: Foo,
                bar: Bar,
            },
        }
    ));

    assert_debug_snapshot!(enum_type);
}
