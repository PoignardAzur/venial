## Lightweight parsing for Rust proc macros

Venial is a WIP parser for Rust [proc macros](https://doc.rust-lang.org/reference/procedural-macros.html).

When writing proc macros that need to parse Rust code (such as attribute and derive macros), the most common solution is to use the [syn](https://docs.rs/syn/latest/syn/index.html) crate. Syn can parse arbitrary valid Rust code, and even Rust-based DSLs, and return versatile data structures that can inspected and mutated in powerful ways.

**It's also extremely heavy**. In [one analysis](https://hackmd.io/mxdn4U58Su-UQXwzOHpHag?view#round-13-cargo-timing-opt-j8) of [lqd's early 2022 benchmark collection](https://github.com/lqd/rustc-benchmarking-data), the author estimates that syn is reponsible for 8% of compile times of the benchmark, which accounts for Rust's most popular crates. There are subtleties (eg this isn't necessarily critical path time, but syn is often in the critical path anyway), but the overall takeaway is clear: syn is expensive.

And yet, a lot of the power of syn is often unneeded. If we look at the [crates that depend on syn](https://crates.io/crates/syn/reverse_dependencies), we can see that the 5 most downloaded are:

- serde_derive
- proc-macro-hack
- pin-project-internal
- anyhow
- thiserror-impl

Of these, proc-macro-hack is deprecated, and the other four only need to parse basic information on a type.

Other popular reverse-dependencies of syn (such as futures-macro, tokios-macros, async-trait, etc) *do* use syn's more advanced features, but there's still room for a lightweight parser in proc-macros.

Venial is that parser.


## Design

Venial is extremely simple. Most of its implementation is in the `parse.rs` file, which is about 350 lines at the time I'm writing this README. This is because the Rust language has a very clean syntax, especially for type declarations.

Venial has no dependency besides proc-macro2 and quote.

To achieve this simplicity, venial makes several trade-offs:

- It can only parse declarations (eg `struct MyStruct {}`). It can't parse expressions or statements. For now, only types and functions are supported.
- It doesn't try to parse inside type expressions. For instance, if your struct includes a field like `foo_bar: &mut Foo<Bar, dyn Foobariser>`, venial will dutifully give you this type as a sequence of tokens and let you interpret it.
- It doesn't attempt to recover gracefully from errors. Venial assumes you're running inside a derive macro, and thus that your input is statically guaranteed to be a valid type declaration. If it isn't, venial will summarily panic.

Note though that venial will accept any syntactically valid declaration, even if it isn't semantically valid. The rule of thumb is "if it compiles under a `#[cfg(FALSE)]`, venial will parse it without panicking".

(Note: The above sentence is a lie; venial currently panics on unsupported declarations, eg traits, aliases, etc.)


## Example

```rust
use venial::{parse_declaration, TypeDeclaration};
use quote::quote;

let enum_type = parse_declaration(quote!(
    enum Shape {
        Square(Square),
        Circle(Circle),
        Triangle(Triangle),
    }
));

let enum_type = match enum_type {
    TypeDeclaration::Enum(enum_type) => enum_type,
    _ => unreachable!(),
};

assert_eq!(enum_type.variants[0].name, "Square");
assert_eq!(enum_type.variants[1].name, "Circle");
assert_eq!(enum_type.variants[2].name, "Triangle");
```

## Benchmarks

TODO.


## Contributions

Pull requests are welcome.

My current roadmap is:

- Find any bugs there might be and fix them.
- Port other projects from syn to venial and compare compile times.
- Add Github Actions

On the long term, I'd also like to add parsing for more use cases, while keeping the crate lightweight:

- Parsing traits.
- Parsing comma-separated expression lists.

With those, I believe venial would cover virtually all use cases that popular libraries use syn for.
