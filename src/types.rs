use proc_macro2::TokenTree;

#[derive(Clone, Debug)]
pub enum TypeDeclaration {
    Struct(Struct),
    Enum(Enum),
}

// TODO - fn TypeDeclaration::name()

#[derive(Clone, Debug)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    // TODO - attributes
    pub name: String,
    pub contents: StructFields,
    // TODO "Variant = xxx"
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: StructFields,
}

#[derive(Clone, Debug)]
pub enum StructFields {
    Unit,
    Tuple(Vec<TupleField>),
    Named(Vec<NamedField>),
}

#[derive(Clone, Debug)]
pub struct TupleField {
    // TODO - attributes
    pub ty: TyExpr,
}

#[derive(Clone, Debug)]
pub struct NamedField {
    // TODO - attributes
    pub name: String,
    pub ty: TyExpr,
}

#[derive(Clone, Debug)]
pub struct TyExpr {
    pub tokens: Vec<TokenTree>,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub tokens: Vec<TokenTree>,
}
