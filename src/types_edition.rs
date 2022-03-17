#![allow(missing_docs)]
#![allow(unused)]

pub use crate::types::{
    Attribute, Declaration, Enum, EnumDiscriminant, EnumVariant, GenericBound, GenericParam,
    GenericParams, NamedField, Struct, StructFields, TupleField, TyExpr, Union, VisMarker,
    WhereClause, WhereClauseItem,
};
use proc_macro2::{Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

impl Declaration {
    pub fn generic_params(&self) -> Option<&GenericParams> {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.generic_params.as_ref(),
            Declaration::Enum(enum_decl) => enum_decl.generic_params.as_ref(),
            Declaration::Union(union_decl) => union_decl.generic_params.as_ref(),
            Declaration::Function(function_decl) => function_decl.generic_params.as_ref(),
        }
    }

    pub fn generic_params_mut(&mut self) -> Option<&mut GenericParams> {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.generic_params.as_mut(),
            Declaration::Enum(enum_decl) => enum_decl.generic_params.as_mut(),
            Declaration::Union(union_decl) => union_decl.generic_params.as_mut(),
            Declaration::Function(function_decl) => function_decl.generic_params.as_mut(),
        }
    }
}

impl Struct {
    pub fn with_param(mut self, param: GenericParam) -> Self {
        let params = self.generic_params.take().unwrap_or_default();
        let params = params.with_param(param);
        self.generic_params = Some(params);
        self
    }

    pub fn with_where_item(mut self, item: WhereClauseItem) -> Self {
        if let Some(where_clause) = self.where_clause {
            self.where_clause = Some(where_clause.with_item(item));
        } else {
            self.where_clause = Some(WhereClause::from_item(item));
        }
        self
    }
}

impl Enum {
    pub fn with_param(mut self, param: GenericParam) -> Self {
        let params = self.generic_params.take().unwrap_or_default();
        let params = params.with_param(param);
        self.generic_params = Some(params);
        self
    }

    pub fn with_where_item(mut self, item: WhereClauseItem) -> Self {
        if let Some(where_clause) = self.where_clauses {
            self.where_clauses = Some(where_clause.with_item(item));
        } else {
            self.where_clauses = Some(WhereClause::from_item(item));
        }
        self
    }
}

impl Union {
    pub fn with_param(mut self, param: GenericParam) -> Self {
        let params = self.generic_params.take().unwrap_or_default();
        let params = params.with_param(param);
        self.generic_params = Some(params);
        self
    }

    pub fn with_where_item(mut self, item: WhereClauseItem) -> Self {
        if let Some(where_clause) = self.where_clauses {
            self.where_clauses = Some(where_clause.with_item(item));
        } else {
            self.where_clauses = Some(WhereClause::from_item(item));
        }
        self
    }
}

impl GenericParams {
    pub fn with_param(mut self, param: GenericParam) -> Self {
        if param.is_lifetime() {
            self.params.insert(0, param, None);
        } else {
            self.params.push(param, None);
        }
        self
    }
}

impl GenericParam {
    pub fn lifetime(name: &str) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            _prefix: Some(Punct::new('\'', Spacing::Joint).into()),
            name: lifetime_ident,
            bound: None,
        }
    }

    pub fn bounded_lifetime(name: &str, bound: Vec<TokenTree>) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            _prefix: Some(Punct::new('\'', Spacing::Alone).into()),
            name: lifetime_ident,
            bound: Some(GenericBound {
                _colon: Punct::new(':', Spacing::Alone),
                tokens: bound,
            }),
        }
    }

    pub fn ty(name: &str) -> Self {
        let ty_ident = Ident::new(name, Span::call_site());
        GenericParam {
            _prefix: None,
            name: ty_ident,
            bound: None,
        }
    }

    pub fn bounded_ty(name: &str, bound: Vec<TokenTree>) -> Self {
        let ty_ident = Ident::new(name, Span::call_site());
        GenericParam {
            _prefix: None,
            name: ty_ident,
            bound: Some(GenericBound {
                _colon: Punct::new(':', Spacing::Alone),
                tokens: bound,
            }),
        }
    }

    pub fn const_param(name: &str, ty: Vec<TokenTree>) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            _prefix: Some(Ident::new("const", Span::call_site()).into()),
            name: lifetime_ident,
            bound: Some(GenericBound {
                _colon: Punct::new(':', Spacing::Alone),
                tokens: ty,
            }),
        }
    }

    pub fn is_lifetime(&self) -> bool {
        match &self._prefix {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '\'' => true,
            _ => false,
        }
    }

    pub fn is_ty(&self) -> bool {
        match &self._prefix {
            Some(_) => false,
            None => true,
        }
    }

    pub fn is_const(&self) -> bool {
        match &self._prefix {
            Some(TokenTree::Ident(ident)) if ident == "const" => true,
            _ => false,
        }
    }
}

impl WhereClause {
    pub fn from_item(item: WhereClauseItem) -> Self {
        Self::default().with_item(item)
    }

    pub fn with_item(mut self, item: WhereClauseItem) -> Self {
        self.items.push(item, None);
        self
    }
}

impl WhereClauseItem {
    pub fn parse(tokens: TokenStream) -> Self {
        let mut tokens = tokens.into_iter().peekable();

        let left_side = crate::parse::consume_stuff_until(&mut tokens, |token| match token {
            TokenTree::Punct(punct) if punct.as_char() == ':' => true,
            _ => false,
        });

        let colon = match tokens.next().unwrap() {
            TokenTree::Punct(punct) if punct.as_char() == ':' => punct.clone(),
            _ => panic!("cannot parse type"),
        };

        let bound_tokens = tokens.collect();

        WhereClauseItem {
            left_side,
            bound: GenericBound {
                _colon: colon,
                tokens: bound_tokens,
            },
        }
    }
}

// TODO - as_inline_args -> InlineGenerics
