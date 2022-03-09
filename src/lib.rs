// TODO - Add clippy lints

mod parse;
mod types;

#[cfg(test)]
mod tests;

pub use parse::parse_type;

pub use types::{
    Attribute, Enum, EnumDiscriminant, EnumVariant, GenericParams, NamedField, Struct,
    StructFields, TupleField, TyExpr, TypeDeclaration, VisMarker, WhereClauses,
};
