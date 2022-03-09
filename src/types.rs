use proc_macro2::TokenTree;

#[derive(Clone, Debug)]
pub enum TypeDeclaration {
    Struct(Struct),
    Enum(Enum),
}

// TODO - fn TypeDeclaration::name()

#[derive(Clone, Debug)]
pub struct Struct {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: String,
    pub generic_params: Option<GenericParams>,
    pub where_clauses: Option<WhereClauses>,
    pub fields: StructFields,
}

#[derive(Clone, Debug)]
pub enum StructFields {
    Unit,
    Tuple(Vec<TupleField>),
    Named(Vec<NamedField>),
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: String,
    pub generic_params: Option<GenericParams>,
    pub where_clauses: Option<WhereClauses>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: String,
    pub contents: StructFields,
    pub discriminant: Option<EnumDiscriminant>,
}

#[derive(Clone, Debug)]
pub struct TupleField {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub ty: TyExpr,
}

#[derive(Clone, Debug)]
pub struct NamedField {
    pub attributes: Vec<Attribute>,
    pub vis_marker: Option<VisMarker>,
    pub name: String,
    pub ty: TyExpr,
}

// --- Token groups ---

#[derive(Clone)]
pub struct Attribute {
    pub _hashbang: TokenTree,
    pub child_tokens: Vec<TokenTree>,
}

#[derive(Clone)]
pub struct VisMarker {
    pub _token1: TokenTree,
    pub _token2: Option<TokenTree>,
}

#[derive(Clone)]
pub struct GenericParams {
    pub tokens: Vec<TokenTree>,
}

#[derive(Clone)]
pub struct WhereClauses {
    pub tokens: Vec<TokenTree>,
}

#[derive(Clone)]
pub struct TyExpr {
    pub tokens: Vec<TokenTree>,
}

#[derive(Clone)]
pub struct EnumDiscriminant {
    pub tokens: Vec<TokenTree>,
}

// ---

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

impl std::fmt::Debug for VisMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self._token2 {
            None => f.write_str(&self._token1.to_string()),
            Some(TokenTree::Group(group)) => {
                let mut list = f.debug_tuple(&self._token1.to_string());
                for token in group.stream().into_iter() {
                    match &token {
                        TokenTree::Group(_group) => list.field(&token),
                        TokenTree::Ident(_ident) => list.field(&token.to_string()),
                        TokenTree::Punct(_punct) => list.field(&token.to_string()),
                        TokenTree::Literal(_literal) => list.field(&token.to_string()),
                    };
                }
                list.finish()
            }
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Debug for GenericParams {
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

impl std::fmt::Debug for WhereClauses {
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

impl std::fmt::Debug for EnumDiscriminant {
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
