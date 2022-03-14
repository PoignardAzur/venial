#![warn(missing_docs)]
#![warn(clippy::missing_errors_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::cargo)]
#![allow(clippy::match_like_matches_macro)]

//! ## Lightweight parsing for Rust proc macros
//!
//! Venial is a WIP parser for Rust [proc macros](https://doc.rust-lang.org/reference/procedural-macros.html).
//!
//! When writing proc macros that need to parse Rust code (such as attribute and derive macros), the most common solution is to use the [syn](https://docs.rs/syn/latest/syn/index.html) crate. Syn can parse arbitrary valid Rust code, and even Rust-based DSLs, and return versatile data structures that can inspected and mutated in powerful ways.
//!
//! **It's also extremely heavy**. In [one analysis](https://hackmd.io/mxdn4U58Su-UQXwzOHpHag?view#round-13-cargo-timing-opt-j8) of [lqd's early 2022 benchmark collection](https://github.com/lqd/rustc-benchmarking-data), the author estimates that syn is reponsible for 8% of compile times of the benchmark, which accounts for Rust's most popular crates. There are subtleties (eg this isn't necessarily critical path time, but syn is often in the critical path anyway), but the overall takeaway is clear: syn is expensive.
//!
//! And yet, a lot of the power of syn is often unneeded. If we look at the [crates that depend on syn](https://crates.io/crates/syn/reverse_dependencies), we can see that the 5 most downloaded are:
//!
//! - serde_derive
//! - proc-macro-hack
//! - pin-project-internal
//! - anyhow
//! - thiserror-impl
//!
//! Of these, proc-macro-hack is deprecated, and the other four only need to parse basic information on a type.
//!
//! Other popular reverse-dependencies of syn (such as futures-macro, tokios-macros, async-trait, etc) *do* use syn's more advanced features, but there's still room for a lightweight parser in proc-macros.
//!
//! Venial is that parser.
//!
//! ## Example
//!
//! ```rust
//! use venial::{parse_declaration, Declaration};
//! use quote::quote;
//!
//! let enum_type = parse_declaration(quote!(
//!     enum Shape {
//!         Square(Square),
//!         Circle(Circle),
//!         Triangle(Triangle),
//!     }
//! ));
//!
//! let enum_type = match enum_type {
//!     Declaration::Enum(enum_type) => enum_type,
//!     _ => unreachable!(),
//! };
//!
//! assert_eq!(enum_type.variants[0].name, "Square");
//! assert_eq!(enum_type.variants[1].name, "Circle");
//! assert_eq!(enum_type.variants[2].name, "Triangle");
//! ```

mod parse;
mod types;

#[cfg(test)]
mod tests;

pub use parse::parse_declaration;

pub use types::{
    Attribute, Declaration, Enum, EnumDiscriminant, EnumVariant, GenericParam, GenericParams,
    NamedField, Struct, StructFields, TupleField, TyExpr, VisMarker, WhereClause, WhereClauseItem,
};
