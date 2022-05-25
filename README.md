[![crates.io](https://img.shields.io/crates/v/venial)](https://crates.io/crates/venial)
[![docs.rs](https://docs.rs/venial/badge.svg)](https://docs.rs/venial/)
[![license](https://img.shields.io/crates/l/venial)](https://github.com/PoignardAzur/venial/blob/main/LICENSE)

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
- It doesn't attempt to recover gracefully from errors. Venial assumes you're running inside a derive or attribute macro, and thus that your input is statically guaranteed to be a valid type declaration. If it isn't, venial will summarily panic.

Note though that venial will accept any syntactically valid declaration, even if it isn't semantically valid. The rule of thumb is "if it compiles under a `#[cfg(FALSE)]`, venial will parse it without panicking".

The only exception is enum discriminants. Venial only supports enum discriminants with a single token, or a token-group. Eg:

```rust
enum MyEnum {
    A = 42,           // Ok
    B = "hello",      // Ok
    C = CONSTANT,     // Ok
    D = FOO + BAR,    // MACRO ERROR
    E = (FOO + BAR),  // Ok
}
```

This is because parsing complex discriminants requires arbitrary expression parsing, which is beyond the scope of this crate.

(Note: venial currently panics on unsupported declarations, eg traits, aliases, etc. Also, function support is incomplete.)


## Example

```rust
use venial::{parse_declaration, Declaration};
use quote::quote;

let enum_type = parse_declaration(quote!(
    enum Shape {
        Square(Square),
        Circle(Circle),
        Triangle(Triangle),
    }
));

let enum_type = match enum_type {
    Declaration::Enum(enum_type) => enum_type,
    _ => unreachable!(),
};

assert_eq!(enum_type.variants[0].0.name, "Square");
assert_eq!(enum_type.variants[1].0.name, "Circle");
assert_eq!(enum_type.variants[2].0.name, "Triangle");
```

## Performance

I haven't performed any kind of formal benchmark yet. That said, I compared [this fork of miniserde using venial](https://github.com/PoignardAzur/miniserde/tree/098bbbc3bac5812dc6613e334281d649fcbf88dc) to the [equivalent miniserde commit](https://github.com/dtolnay/miniserde/tree/4951a04384a69a3261e1a817ac4d146b119e953b), and got the following results:

```sh
$ cargo check -j1 # miniserde-venial, clean build
    Finished dev [unoptimized + debuginfo] target(s) in 6.30s
$ cargo check -j1 # miniserde, clean build
    Finished dev [unoptimized + debuginfo] target(s) in 9.52s

$ cargo check -j4 # miniserde-venial, clean build
    Finished dev [unoptimized + debuginfo] target(s) in 3.17s
$ cargo check -j4 # miniserde, clean build
    Finished dev [unoptimized + debuginfo] target(s) in 4.79s
```

My machine is desktop computer with an AMD Ryzen 7 1800x (8 cores, 16 threads), I have 32GB of RAM and a 2.5TB SSD.

As we can see, using venial instead of syn shaves about 3.2s off total build times in single-threaded builds, and 1.6s in 4-threaded builds.

Most of the difference comes from syn and venial themselves: `cargo check --timings` shows that syn takes 2.11s to compile and venial takes 0.58s in 4-threaded builds.

I'm not showing codegen builds, release mode builds, 16-threads builds and the like, but the trend stays roughly the same: for the miniserde project, switching to venial removes ~30% of the build time.

### So... Is it worth it?

That's a fairly complicated to answer. At the time I'm writing this section my answer is "Probably, but I'm less enthusiastic than when I started the project".

If you take the most optimistic interpretation, this is great! On a single-threaded machine, switching shaves three seconds off, a whole third of the build time!

In reality, there are a lot of complicating factors:

- Venial never improves incremental build times at all (since dependencies are cached, even when incremental compilation is off).
- The gap between syn and venial is shorter with any amount of multithreading.
- I have a fairly powerful computer. Laptops might get more of a benefit from venial.
- In projects bigger than miniserde, syn is usually one of many libraries being compiled at the same time. In some cases that means the build time of syn doesn't matter that much since it's compiled in parallel with other libraries. In other cases syn is on the critical path.
- In practice, most clean build are run by CI servers. To measure the usefulness of venial, you'd need to analyze the specs of the servers used in Github Actions / Gitlab CI / whatever [crater](https://github.com/rust-lang/crater) uses.

All in all, it's questionable whether the benefits are worth porting your derive crate from syn to venial (though my experience so far has been that porting isn't that hard).

Another thing to keep in mind is that this is a very young library. There has been very little effort to optimize it or profile it so far, and further versions may give a better build time reduction.

**tl;dr:** You can probably shave off a few seconds off your clean builds with venial. Incremental builds see no benefits.

## Contributions

Pull requests are welcome.

I have no intention to work on venial in the near future myself, but I will still merge PRs.

Some possible improvements:

- Fixing the function declaration parser.
- Finding and fixing any eventual bugs.
- Porting other projects from syn to venial and comparing compile times.
- Parsing traits.
- Parsing all possible declarations.
