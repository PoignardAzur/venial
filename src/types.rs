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
    pub attributes: Vec<Attribute>,
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
    pub attributes: Vec<Attribute>,
    pub ty: TyExpr,
}

#[derive(Clone, Debug)]
pub struct NamedField {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub ty: TyExpr,
}

#[derive(Clone)]
pub struct TyExpr {
    pub tokens: Vec<TokenTree>,
}

#[derive(Clone)]
pub struct Attribute {
    pub _hashbang: TokenTree,
    pub child_tokens: Vec<TokenTree>,
}

// ---

impl std::fmt::Debug for TyExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in &self.tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}

impl std::fmt::Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("#")?;
        let mut list = f.debug_list();
        for token in &self.child_tokens {
            match token {
                TokenTree::Group(_group) => list.entry(token),
                TokenTree::Ident(_ident) => list.entry(&token.to_string()),
                TokenTree::Punct(_punct) => list.entry(&token.to_string()),
                TokenTree::Literal(_literal) => list.entry(&token.to_string()),
            };
        }
        list.finish()
    }
}
