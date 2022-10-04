use crate::parse_utils::{consume_path, tokens_from_slice};
pub use crate::types::{
    Attribute, AttributeValue, Declaration, Enum, EnumVariant, EnumVariantValue, Function,
    GenericBound, GenericParam, GenericParamList, GroupSpan, InlineGenericArgs, NamedField, Struct,
    StructFields, TupleField, TyExpr, Union, VisMarker, WhereClause, WhereClauseItem,
};
use crate::types::{FnQualifiers, GenericArg, GenericArgList, Impl, Module, Path};
use crate::{Constant, Punctuated, TyDefinition};
use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

impl Declaration {
    /// Returns the [`Vec<Attribute>`] of the declaration.
    pub fn attributes(&self) -> &Vec<Attribute> {
        match self {
            Declaration::Struct(struct_decl) => &struct_decl.attributes,
            Declaration::Enum(enum_decl) => &enum_decl.attributes,
            Declaration::Union(union_decl) => &union_decl.attributes,
            Declaration::Module(mod_decl) => &mod_decl.attributes,
            Declaration::Impl(impl_decl) => &impl_decl.attributes,
            Declaration::TyDefinition(ty_decl) => &ty_decl.attributes,
            Declaration::Function(function_decl) => &function_decl.attributes,
            Declaration::Constant(const_decl) => &const_decl.attributes,
            Declaration::Use(use_decl) => &use_decl.attributes,
        }
    }

    /// Returns the [`Vec<Attribute>`] of the declaration.
    pub fn attributes_mut(&mut self) -> &mut Vec<Attribute> {
        match self {
            Declaration::Struct(struct_decl) => &mut struct_decl.attributes,
            Declaration::Enum(enum_decl) => &mut enum_decl.attributes,
            Declaration::Union(union_decl) => &mut union_decl.attributes,
            Declaration::Module(mod_decl) => &mut mod_decl.attributes,
            Declaration::Impl(impl_decl) => &mut impl_decl.attributes,
            Declaration::TyDefinition(ty_decl) => &mut ty_decl.attributes,
            Declaration::Function(function_decl) => &mut function_decl.attributes,
            Declaration::Constant(const_decl) => &mut const_decl.attributes,
            Declaration::Use(use_decl) => &mut use_decl.attributes,
        }
    }

    /// Returns the [`GenericParamList`], if any, of the declaration.
    ///
    /// For instance, this will return Some for `struct MyStruct<A, B, C> { ... }`,
    /// Some for `impl<A> MyTrait for MyType<A>` and None for `enum MyEnum { ... }`.
    ///
    /// `TyDefinition` and `Constant` variants never have a generic parameter list.
    pub fn generic_params(&self) -> Option<&GenericParamList> {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.generic_params.as_ref(),
            Declaration::Enum(enum_decl) => enum_decl.generic_params.as_ref(),
            Declaration::Union(union_decl) => union_decl.generic_params.as_ref(),
            Declaration::Module(_) => None,
            Declaration::Impl(impl_decl) => impl_decl.impl_generic_params.as_ref(),
            Declaration::TyDefinition(_) => None,
            Declaration::Function(function_decl) => function_decl.generic_params.as_ref(),
            Declaration::Constant(_) => None,
            Declaration::Use(_) => None,
        }
    }

    /// Returns the [`GenericParamList`], if any, of the declaration.
    ///
    /// For instance, this will return Some for `struct MyStruct<A, B, C> { ... }`,
    /// Some for `impl<A> MyTrait for MyType<A>` and None for `enum MyEnum { ... }`.
    ///
    /// `TyDefinition` and `Constant` variants never have a generic parameter list.
    pub fn generic_params_mut(&mut self) -> Option<&mut GenericParamList> {
        match self {
            Declaration::Struct(struct_decl) => struct_decl.generic_params.as_mut(),
            Declaration::Enum(enum_decl) => enum_decl.generic_params.as_mut(),
            Declaration::Union(union_decl) => union_decl.generic_params.as_mut(),
            Declaration::Module(_) => None,
            Declaration::Impl(impl_decl) => impl_decl.impl_generic_params.as_mut(),
            Declaration::TyDefinition(_) => None,
            Declaration::Function(function_decl) => function_decl.generic_params.as_mut(),
            Declaration::Constant(_) => None,
            Declaration::Use(_) => None,
        }
    }

    /// Returns the [`Ident`] of the declaration, if available.
    ///
    /// Certain declarations (currently `impl` blocks) do not have a name, as they refer to other (possibly qualified) types.
    /// In that case, `None` is returned.
    ///
    /// ```
    /// # use venial::parse_declaration;
    /// # use quote::quote;
    /// let struct_type = parse_declaration(quote!(
    ///     struct Hello(A, B);
    /// )).unwrap();
    /// assert_eq!(struct_type.name().unwrap().to_string(), "Hello");
    /// ```
    pub fn name(&self) -> Option<Ident> {
        match self {
            Declaration::Struct(struct_decl) => Some(struct_decl.name.clone()),
            Declaration::Enum(enum_decl) => Some(enum_decl.name.clone()),
            Declaration::Union(union_decl) => Some(union_decl.name.clone()),
            Declaration::Module(mod_decl) => Some(mod_decl.name.clone()),
            Declaration::Impl(_) => None,
            Declaration::TyDefinition(ty_decl) => Some(ty_decl.name.clone()),
            Declaration::Function(function_decl) => Some(function_decl.name.clone()),
            Declaration::Constant(const_decl) => Some(const_decl.name.clone()),
            Declaration::Use(_) => None,
        }
    }

    /// Returns the [`Struct`] variant of the enum if possible.
    pub fn as_struct(&self) -> Option<&Struct> {
        match self {
            Declaration::Struct(struct_decl) => Some(struct_decl),
            _ => None,
        }
    }

    /// Returns the [`Enum`] variant of the enum if possible.
    pub fn as_enum(&self) -> Option<&Enum> {
        match self {
            Declaration::Enum(enum_decl) => Some(enum_decl),
            _ => None,
        }
    }

    /// Returns the [`Union`] variant of the enum if possible.
    pub fn as_union(&self) -> Option<&Union> {
        match self {
            Declaration::Union(union_decl) => Some(union_decl),
            _ => None,
        }
    }

    /// Returns the [`Mod`] variant of the enum if possible.
    pub fn as_module(&self) -> Option<&Module> {
        match self {
            Declaration::Module(mod_decl) => Some(mod_decl),
            _ => None,
        }
    }

    /// Returns the [`Impl`] variant of the enum if possible.
    pub fn as_impl(&self) -> Option<&Impl> {
        match self {
            Declaration::Impl(impl_decl) => Some(impl_decl),
            _ => None,
        }
    }

    /// Returns the [`TyDefinition`] variant of the enum if possible.
    pub fn as_ty_definition(&self) -> Option<&TyDefinition> {
        match self {
            Declaration::TyDefinition(ty_decl) => Some(ty_decl),
            _ => None,
        }
    }

    /// Returns the [`Function`] variant of the enum if possible.
    pub fn as_function(&self) -> Option<&Function> {
        match self {
            Declaration::Function(function_decl) => Some(function_decl),
            _ => None,
        }
    }

    /// Returns the [`Constant`] variant of the enum if possible.
    pub fn as_constant(&self) -> Option<&Constant> {
        match self {
            Declaration::Constant(const_decl) => Some(const_decl),
            _ => None,
        }
    }
}

impl Struct {
    /// Returns a collection of strings that can be used to exhaustively
    /// access the struct's fields.
    ///
    /// If the struct is a tuple struct, integer strings will be returned.
    ///
    /// ```
    /// # use venial::parse_declaration;
    /// # use quote::quote;
    /// let struct_type = parse_declaration(quote!(
    ///     struct Hello {
    ///         a: Foo,
    ///         b: Bar,
    ///     }
    /// )).unwrap();
    /// let struct_type = struct_type.as_struct().unwrap();
    /// let field_names: Vec<_> = struct_type.field_names().into_iter().collect();
    /// assert_eq!(field_names, ["a", "b"]);
    /// ```
    ///
    /// ```
    /// # use venial::parse_declaration;
    /// # use quote::quote;
    /// let tuple_type = parse_declaration(quote!(
    ///     struct Hello(Foo, Bar);
    /// )).unwrap();
    /// let tuple_type = tuple_type.as_struct().unwrap();
    /// let field_names: Vec<_> = tuple_type.field_names().into_iter().collect();
    /// assert_eq!(field_names, ["0", "1"]);
    /// ```
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

    /// Returns a collection of tokens that can be used to exhaustively
    /// access the struct's fields.
    ///
    /// If the struct is a tuple struct, span-less integer literals will be returned.
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

    /// Returns a collection of references to the struct's field types.
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
}

impl Enum {
    /// Returns true if every single variant is empty.
    ///
    /// ```
    /// # use venial::parse_declaration;
    /// # use quote::quote;
    /// let enum_type = parse_declaration(quote!(
    ///     enum MyEnum { A, B, C, D }
    /// )).unwrap();
    /// let enum_type = enum_type.as_enum().unwrap();
    /// assert!(enum_type.is_c_enum());
    /// ```
    pub fn is_c_enum(&self) -> bool {
        for variant in self.variants.items() {
            if !variant.is_empty_variant() {
                return false;
            }
        }
        true
    }
}

macro_rules! implement_common_methods {
    ($Kind:ident) => {
        impl $Kind {
            /// Builder method, add a [`GenericParam`] to `self.generic_params`.
            ///
            /// Creates a default [`GenericParamList`] if `self.generic_params` is None.
            pub fn with_param(mut self, param: GenericParam) -> Self {
                let params = self.generic_params.take().unwrap_or_default();
                let params = params.with_param(param);
                self.generic_params = Some(params);
                self
            }

            /// Builder method, add a [`WhereClauseItem`] to `self.where_clause`.
            ///
            /// Creates a default [`WhereClause`] if `self.where_clause` is None.
            pub fn with_where_item(mut self, item: WhereClauseItem) -> Self {
                if let Some(where_clause) = self.where_clause {
                    self.where_clause = Some(where_clause.with_item(item));
                } else {
                    self.where_clause = Some(WhereClause::from_item(item));
                }
                self
            }

            /// Returns a collection of references to declared lifetime params, if any.
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

            /// Returns a collection of references to declared type params, if any.
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

            /// Returns a collection of references to declared const generic params, if any.
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

            /// See [`InlineGenericArgs`] for details.
            pub fn get_inline_generic_args(&self) -> Option<InlineGenericArgs<'_>> {
                Some(self.generic_params.as_ref()?.as_inline_args())
            }

            /// Returns a where clause that can be quoted to form
            /// a `impl TRAIT for TYPE where ... { ... }` trait implementation.
            ///
            /// This takes the bounds of the current declaration and adds one bound
            /// to `derived_trait` for every generic argument. For instance:
            ///
            /// ```no_run
            /// struct MyStruct<T, U> where T: Clone {
            ///     t: T,
            ///     u: U
            /// }
            ///
            /// // my_struct_decl.create_derive_where_clause(quote!(SomeTrait))
            ///
            /// # trait SomeTrait {}
            /// impl<T, U> SomeTrait for MyStruct<T, U>
            ///     // GENERATED WHERE CLAUSE
            ///     where T: SomeTrait, U: SomeTrait, T: Clone
            /// {
            ///     // ...
            /// }
            /// ```
            pub fn create_derive_where_clause(&self, derived_trait: TokenStream) -> WhereClause {
                let mut where_clause = self.where_clause.clone().unwrap_or_default();

                for param in self.get_type_params() {
                    let item = WhereClauseItem {
                        left_side: vec![param.name.clone().into()],
                        bound: GenericBound {
                            tk_colon: Punct::new(':', Spacing::Alone),
                            tokens: derived_trait.clone().into_iter().collect(),
                        },
                    };

                    where_clause = where_clause.with_item(item);
                }

                where_clause
            }
        }
    };
}

implement_common_methods! { Struct }
implement_common_methods! { Enum }
implement_common_methods! { Union }

impl Attribute {
    /// Returns Some if the attribute has a single path segment, eg `#[hello(...)]`.
    /// Returns None if the attribute has multiple segments, eg `#[hello::world(...)]`.
    pub fn get_single_path_segment(&self) -> Option<&Ident> {
        let mut segments: Vec<_> = self
            .path
            .iter()
            .filter_map(|segment| match segment {
                TokenTree::Ident(ident) => Some(ident),
                _ => None,
            })
            .collect();
        if segments.len() == 1 {
            segments.pop()
        } else {
            None
        }
    }

    /// Returns `foo + bar` for `#[hello = foo + bar]` and `#[hello(foo + bar)]`.
    /// Returns an empty slice for `#[hello]`.
    pub fn get_value_tokens(&self) -> &[TokenTree] {
        self.value.get_value_tokens()
    }
}

impl AttributeValue {
    /// Returns `foo + bar` for `#[hello = foo + bar]` and `#[hello(foo + bar)]`.
    /// Returns an empty slice for `#[hello]`.
    pub fn get_value_tokens(&self) -> &[TokenTree] {
        match self {
            AttributeValue::Group(_, tokens) => tokens,
            AttributeValue::Equals(_, tokens) => tokens,
            AttributeValue::Empty => &[],
        }
    }
}

impl EnumVariant {
    /// Returns true if the variant doesn't store a type.
    pub fn is_empty_variant(&self) -> bool {
        matches!(self.contents, StructFields::Unit)
    }

    /// Returns Some if the variant is a wrapper around a single type.
    /// Returns None otherwise.
    pub fn get_single_type(&self) -> Option<&TupleField> {
        match &self.contents {
            StructFields::Tuple(fields) if fields.fields.len() == 1 => Some(&fields.fields[0].0),
            StructFields::Tuple(_fields) => None,
            StructFields::Unit => None,
            StructFields::Named(_) => None,
        }
    }
}

impl FnQualifiers {
    /// Whether exactly either `const` or `unsafe` attribute is set, and no other one
    /// (so the tokens could be the start of a constant or impl declaration)
    pub(crate) fn has_only_const_xor_unsafe(&self) -> bool {
        (self.tk_const.is_some() ^ self.tk_unsafe.is_some())
            && self.tk_default.is_none()
            && self.tk_async.is_none()
            && self.tk_extern.is_none()
            && self.extern_abi.is_none()
    }
}

impl GenericParamList {
    /// Builder method, add a [`GenericParam`].
    ///
    /// Add lifetime params to the beginning of the list.
    pub fn with_param(mut self, param: GenericParam) -> Self {
        if param.is_lifetime() {
            self.params.insert(0, param, None);
        } else {
            self.params.push(param, None);
        }
        self
    }

    /// See [`InlineGenericArgs`] for details.
    pub fn as_inline_args(&self) -> InlineGenericArgs<'_> {
        InlineGenericArgs(self)
    }
}

impl GenericParam {
    /// Create new lifetime param from name.
    ///
    /// ```
    /// # use venial::GenericParam;
    /// GenericParam::lifetime("a")
    /// # ;
    /// ```
    pub fn lifetime(name: &str) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            tk_prefix: Some(Punct::new('\'', Spacing::Joint).into()),
            name: lifetime_ident,
            bound: None,
        }
    }

    /// Create new lifetime param from name and bound.
    ///
    /// ```
    /// # use venial::GenericParam;
    /// # use quote::quote;
    /// GenericParam::bounded_lifetime("a", quote!(b + c).into_iter().collect())
    /// # ;
    /// ```
    pub fn bounded_lifetime(name: &str, bound: Vec<TokenTree>) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            tk_prefix: Some(Punct::new('\'', Spacing::Alone).into()),
            name: lifetime_ident,
            bound: Some(GenericBound {
                tk_colon: Punct::new(':', Spacing::Alone),
                tokens: bound,
            }),
        }
    }

    /// Create new type param from name.
    ///
    /// ```
    /// # use venial::GenericParam;
    /// GenericParam::ty("T")
    /// # ;
    /// ```
    pub fn ty(name: &str) -> Self {
        let ty_ident = Ident::new(name, Span::call_site());
        GenericParam {
            tk_prefix: None,
            name: ty_ident,
            bound: None,
        }
    }

    /// Create new type param from name and bound.
    ///
    /// ```
    /// # use venial::GenericParam;
    /// # use quote::quote;
    /// GenericParam::bounded_ty("T", quote!(Debug + Eq).into_iter().collect())
    /// # ;
    /// ```
    pub fn bounded_ty(name: &str, bound: Vec<TokenTree>) -> Self {
        let ty_ident = Ident::new(name, Span::call_site());
        GenericParam {
            tk_prefix: None,
            name: ty_ident,
            bound: Some(GenericBound {
                tk_colon: Punct::new(':', Spacing::Alone),
                tokens: bound,
            }),
        }
    }

    /// Create new const param from name and type.
    ///
    /// ```
    /// # use venial::GenericParam;
    /// # use quote::quote;
    /// GenericParam::const_param("N", quote!(i32).into_iter().collect())
    /// # ;
    /// ```
    pub fn const_param(name: &str, ty: Vec<TokenTree>) -> Self {
        let lifetime_ident = Ident::new(name, Span::call_site());
        GenericParam {
            tk_prefix: Some(Ident::new("const", Span::call_site()).into()),
            name: lifetime_ident,
            bound: Some(GenericBound {
                tk_colon: Punct::new(':', Spacing::Alone),
                tokens: ty,
            }),
        }
    }

    /// Returns true if the generic param is a lifetime param.
    pub fn is_lifetime(&self) -> bool {
        matches!(
            &self.tk_prefix,
            Some(TokenTree::Punct(punct)) if punct.as_char() == '\''
        )
    }

    /// Returns true if the generic param is a type param.
    pub fn is_ty(&self) -> bool {
        self.tk_prefix.is_none()
    }

    /// Returns true if the generic param is a const param.
    pub fn is_const(&self) -> bool {
        matches!(
            &self.tk_prefix,
            Some(TokenTree::Ident(ident)) if ident == "const"
        )
    }
}

impl<'a> InlineGenericArgs<'a> {
    /// Returns an owned argument list from this.
    ///
    /// # Panics
    /// If `self` is a mal-formed generic argument list.
    pub fn to_owned_args(&self) -> GenericArgList {
        let GenericParamList {
            tk_l_bracket,
            params,
            tk_r_bracket,
        } = self.0;

        GenericArgList {
            tk_turbofish_colons: None, // TODO add if GenericParamList supports this, too
            tk_l_bracket: tk_l_bracket.clone(),
            args: Punctuated {
                inner: params
                    .inner
                    .iter()
                    .map(|(param, punctuated_punct)| {
                        let name = param.name.clone();
                        let arg = match &param.tk_prefix {
                            Some(TokenTree::Punct(punct)) if punct.as_char() == '\'' => {
                                GenericArg::Lifetime {
                                    tk_lifetime: punct.clone(),
                                    ident: name,
                                }
                            }
                            Some(TokenTree::Ident(ident)) if ident == "const" => {
                                GenericArg::TyOrConst {
                                    expr: TyExpr {
                                        tokens: vec![TokenTree::Ident(name)],
                                    },
                                }
                            }
                            Some(_) => panic!("unexpected tk_prefix, must be ' or const"),
                            None => GenericArg::TyOrConst {
                                expr: TyExpr {
                                    tokens: vec![TokenTree::Ident(name)],
                                },
                            },
                        };

                        (arg, punctuated_punct.clone())
                    })
                    .collect(),
                skip_last: params.skip_last,
            },
            tk_r_bracket: tk_r_bracket.clone(),
        }
    }
}

impl WhereClause {
    /// Create where-clause with a single item.
    pub fn from_item(item: WhereClauseItem) -> Self {
        Self::default().with_item(item)
    }

    /// Builder method, add an item to the where-clause.
    pub fn with_item(mut self, item: WhereClauseItem) -> Self {
        self.items.push(item, None);
        self
    }
}

impl WhereClauseItem {
    /// Helper method to create a WhereClauseItem from a quote.
    ///
    /// # Panics
    ///
    /// Panics if given a token stream that isn't a valid where-clause item.
    pub fn parse(tokens: TokenStream) -> Self {
        let mut tokens = tokens.into_iter().peekable();

        let left_side = crate::parse_utils::consume_stuff_until(
            &mut tokens,
            |token| match token {
                TokenTree::Punct(punct) if punct.as_char() == ':' => true,
                _ => false,
            },
            false,
        );

        let colon = match tokens.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ':' => punct,
            Some(token) => panic!(
                "cannot parse where-clause item: expected ':', found token {:?}",
                token
            ),
            None => {
                panic!("cannot parse where-clause item: expected colon, found end of stream")
            }
        };

        let bound_tokens = tokens.collect();

        WhereClauseItem {
            left_side,
            bound: GenericBound {
                tk_colon: colon,
                tokens: bound_tokens,
            },
        }
    }
}

impl TyExpr {
    /// Tries to parse this type as a [`Path`] such as `path::to::Type<'a, other::Type>`.
    ///
    /// If it does not match a path, `None` is returned.
    pub fn as_path(&self) -> Option<Path> {
        let tokens = tokens_from_slice(&self.tokens);

        consume_path(tokens)
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
