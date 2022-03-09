## Lightweight parsing for Rust proc macros

Venial is a WIP parser for Rust [proc macros](https://doc.rust-lang.org/reference/procedural-macros.html).

When writing proc macros that need to parse Rust code (such as attribute and derive macros), the most common solution is to use the [syn](https://docs.rs/syn/latest/syn/index.html) crate. Syn can parse arbitrary valid Rust code, and return data versatile structures that can inspected and mutated in powerful ways.

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

Venial is extremely simple. Most of its implementation is in the `parse.rs` file, which is about 350 lines at the time I'm writing this README. This is because the Rust language has a very clean syntax, especially for type declarations, as long as you don't try to do anything fancy like parsing inside generic types.

Venial has no dependency besides proc-macro2 and quote.


## Example

TODO.


## Benchmarks

TODO.


## Contributions

Pull requests are welcome.

My current roadmap is:

- Find any bugs there might be and fix it.
- Finish the README and documentation.
- Publish on crates.io.
- Port other projects from syn to venial and compare compile times.

On the long term, I'd also like to add parsing for more use cases, while keeping the crate lightweight:

- Parsing unions.
- Parsing functions.
- Parsing traits.
- Parsing comma-separated expression lists.

With those, I believe venial would cover virtually all use cases that popular libraries use syn for.
