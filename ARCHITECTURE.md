# Project architecture

The composition root of the project is the `parse_declaration` function.

This project tries to have a low MSRV. It has minimal dependencies, and doesn't use any advanced Rust features.

The code tends to use using `match` rather `if let`, and `loop` rather than `while`. This is more a matter of style than necessity.


## Files

- **`parse.rs` -** composition root.
    - **`parse_fn.rs` -** parsing function declarations.
    - **`parse_impl.rs` -** parsing `impl` blocks.
    - **`parse_mod.rs` -** parsing modules and `use` statements.
    - **`parse_type.rs` -** parsing chunks of type declarations (generic args, where clauses, enum variants, struct fields, etc).
    - **`parse_utils.rs` -** parsing attributes, paths, common token, `consume_stuff_until`.
- **`types.rs` -** the types of all data structures returned by the parser; includes implementations of `std::fmt::Debug` and`quote::ToTokens`.
    - **`types_edition.rs` -** method for editing types and getting useful information from them.
    - **`punctuated.rs` -** generic data structure representing a list of objects separated by punctuation. Includes optional trailing punctuation.
- **`tests.rs` -** all the unit tests of the crate.
    - **`snapshots/` -** The values of snapshot tests. See [the documentation of `insta`](https://docs.rs/insta/latest/insta/) for details.
- **`error.rs` -** convenient error type for displaying errors in your macros to users.


## Naming schemes

Functions named `parse_xxx` take a token stream or another token container *by value* and either return a successfully parsed value, return an error, or panic.

Functions named `consume_xxx` take a token stream *by mutable reference* and either return a successfully parsed value, or nothing (None or empty array). If they return nothing, no token is consumed.

Therefore parse_xxx methods are better for cases where you know what you expect and not getting be would be incorrect Rust syntax (eg enum variants after an `enum` keyword). And consume_xxx mehods are better for cases where you're expecting optional syntax (eg `pub` keywords).


## `consume_stuff_until`

Rust has some syntax bits that are too complicated from Venial to parse.

Instead, venial uses its knowledge of when that syntax bit is guaranteed to end (eg: a `where` clause is guaranteed to end with `;` or `{`) and just captures and stores the whole token stream.

`consume_stuff_until` is the function that handles that logic. There's also some logic to handle generic braces.


## Unit tests

The `tests.rs` includes a few utility macros/functions:

- **`assert_quote_snapshot` -** A wrapper around `insta`'s `assert_display_snapshot`. Serializes the given object with `quote!` before snapshotting it.
- **`parse_xxx_checked` -** Parses XXX, then serializes the parsed object with `quote!`; checks that the token stream is preserved by the round-trip.