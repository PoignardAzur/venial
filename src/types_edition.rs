#![allow(missing_docs)]
// TODO - document

pub use crate::types::{
    Attribute, Declaration, Enum, EnumDiscriminant, EnumVariant, Function, GenericBound,
    GenericParam, GenericParams, GroupSpan, InlineGenericArgs, NamedField, Struct, StructFields,
    TupleField, TyExpr, Union, VisMarker, WhereClause, WhereClauseItem,
};
use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

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

    pub fn name(&self) -> Ident {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.name.clone(),
            Declaration::Enum(enum_decl) => enum_decl.name.clone(),
            Declaration::Union(union_decl) => union_decl.name.clone(),
            Declaration::Function(function_decl) => function_decl.name.clone(),
        }
    }

    pub fn as_struct(&self) -> Option<&Struct> {
        match self {
            Declaration::Struct(struct_decl) => Some(&struct_decl),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Enum> {
        match self {
            Declaration::Enum(enum_decl) => Some(&enum_decl),
            _ => None,
        }
    }

    pub fn as_union(&self) -> Option<&Union> {
        match self {
            Declaration::Union(union_decl) => Some(&union_decl),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Function> {
        match self {
            Declaration::Function(function_decl) => Some(&function_decl),
            _ => None,
        }
    }
}

// TODO - use macros to remove copy-pasting

impl Struct {
    pub fn field_names(&self) -> impl IntoIterator<Item = String> {
        match &self.fields {
            StructFields::Unit => Vec::new(),
            StructFields::Tuple(tuple_fields) => {
                let range = 0..tuple_fields.fields.len();
                range.map(|i| i.to_string()).collect()
            }
            StructFields::Named(named_fields) => named_fields
                .fields
                .items()
                .map(|field| field.name.to_string())
                .collect(),
        }
    }

    pub fn field_tokens(&self) -> impl IntoIterator<Item = TokenTree> {
        match &self.fields {
            StructFields::Unit => Vec::new(),
            StructFields::Tuple(tuple_fields) => {
                let range = 0..tuple_fields.fields.len();
                range.map(|i| Literal::usize_unsuffixed(i).into()).collect()
            }
            StructFields::Named(named_fields) => named_fields
                .fields
                .items()
                .map(|field| field.name.clone().into())
                .collect(),
        }
    }

    pub fn field_types(&self) -> impl IntoIterator<Item = &TyExpr> {
        match &self.fields {
            StructFields::Unit => Vec::new(),
            StructFields::Tuple(tuple_fields) => {
                tuple_fields.fields.items().map(|field| &field.ty).collect()
            }
            StructFields::Named(named_fields) => {
                named_fields.fields.items().map(|field| &field.ty).collect()
            }
        }
    }

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

    pub fn get_lifetime_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_lifetime(param))
    }

    pub fn get_type_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_ty(param))
    }

    pub fn get_const_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_const(param))
    }

    pub fn get_inline_generic_args(&self) -> Option<InlineGenericArgs<'_>> {
        Some(self.generic_params.as_ref()?.as_inline_args())
    }

    pub fn create_derive_where_clause(&self, derived_trait: TokenStream) -> WhereClause {
        let mut where_clause = self.where_clause.clone().unwrap_or_default();

        for param in self.get_type_params() {
            let item = WhereClauseItem {
                left_side: vec![param.name.clone().into()],
                bound: GenericBound {
                    _colon: Punct::new(':', Spacing::Alone),
                    tokens: derived_trait.clone().into_iter().collect(),
                },
            };

            where_clause = where_clause.with_item(item);
        }

        where_clause
    }
}

impl Enum {
    pub fn is_c_enum(&self) -> bool {
        for variant in self.variants.items() {
            if !variant.is_empty_variant() {
                return false;
            }
        }
        true
    }

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

    pub fn get_lifetime_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_lifetime(param))
    }

    pub fn get_type_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_ty(param))
    }

    pub fn get_const_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_const(param))
    }

    pub fn get_inline_generic_args(&self) -> Option<InlineGenericArgs<'_>> {
        Some(self.generic_params.as_ref()?.as_inline_args())
    }

    pub fn create_derive_where_clause(&self, derived_trait: TokenStream) -> WhereClause {
        let mut where_clause = self.where_clause.clone().unwrap_or_default();

        for param in self.get_type_params() {
            let item = WhereClauseItem {
                left_side: vec![param.name.clone().into()],
                bound: GenericBound {
                    _colon: Punct::new(':', Spacing::Alone),
                    tokens: derived_trait.clone().into_iter().collect(),
                },
            };

            where_clause = where_clause.with_item(item);
        }

        where_clause
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
        if let Some(where_clause) = self.where_clause {
            self.where_clause = Some(where_clause.with_item(item));
        } else {
            self.where_clause = Some(WhereClause::from_item(item));
        }
        self
    }

    pub fn get_lifetime_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_lifetime(param))
    }

    pub fn get_type_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_ty(param))
    }

    pub fn get_const_params(&self) -> impl Iterator<Item = &GenericParam> {
        let params: &[_] = if let Some(params) = self.generic_params.as_ref() {
            &params.params
        } else {
            &[]
        };

        params
            .iter()
            .map(|(param, _punct)| param)
            .filter(|param| GenericParam::is_const(param))
    }

    pub fn get_inline_generic_args(&self) -> Option<InlineGenericArgs<'_>> {
        Some(self.generic_params.as_ref()?.as_inline_args())
    }

    pub fn create_derive_where_clause(&self, derived_trait: TokenStream) -> WhereClause {
        let mut where_clause = self.where_clause.clone().unwrap_or_default();

        for param in self.get_type_params() {
            let item = WhereClauseItem {
                left_side: vec![param.name.clone().into()],
                bound: GenericBound {
                    _colon: Punct::new(':', Spacing::Alone),
                    tokens: derived_trait.clone().into_iter().collect(),
                },
            };

            where_clause = where_clause.with_item(item);
        }

        where_clause
    }
}

impl EnumVariant {
    pub fn is_empty_variant(&self) -> bool {
        matches!(self.contents, StructFields::Unit)
    }

    pub fn get_single_type(&self) -> Option<&TupleField> {
        match &self.contents {
            StructFields::Tuple(fields) if fields.fields.len() == 1 => Some(&fields.fields[0].0),
            StructFields::Tuple(_fields) => None,
            StructFields::Unit => None,
            StructFields::Named(_) => None,
        }
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

    pub fn as_inline_args(&self) -> InlineGenericArgs<'_> {
        InlineGenericArgs(&self)
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

impl GroupSpan {
    /// Create from proc_macro2 Group.
    pub fn new(group: &Group) -> Self {
        Self {
            span: group.span(),
            delimiter: group.delimiter(),
        }
    }
}
