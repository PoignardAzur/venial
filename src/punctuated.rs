#![allow(missing_docs)]

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt as _};

/// Inspired by syn's `Punctuated` type.
#[derive(Clone)]
pub struct Punctuated<T> {
    inner: Vec<(T, Punct)>,
    last: Option<Box<T>>,
}

impl<T> Punctuated<T> {
    pub fn new() -> Self {
        Punctuated {
            inner: Vec::new(),
            last: None,
        }
    }

    pub fn push(&mut self, value: T) {
        self.push_with_period(value, None);
    }

    pub fn push_with_period(&mut self, value: T, period: Option<Punct>) {
        if let Some(item) = self.last.take() {
            self.inner.push((*item, Punct::new(',', Spacing::Alone)))
        }
        let period = period.unwrap_or(Punct::new(',', Spacing::Alone));
        self.inner.push((value, period))
    }

    /// Inserts an element at position `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is greater than the number of elements previously in
    /// this punctuated sequence.
    pub fn insert(&mut self, index: usize, value: T) {
        assert!(index <= self.len());

        if index == self.len() {
            self.push(value);
        } else {
            self.inner
                .insert(index, (value, Punct::new(',', Spacing::Alone)));
        }
    }

    pub fn len(&self) -> usize {
        if self.last.is_some() {
            self.inner.len() + 1
        } else {
            self.inner.len()
        }
    }
}

// --- Trait impls ---

impl<T> Default for Punctuated<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Punctuated<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for (item, _) in &self.inner {
            list.entry(item);
        }
        if let Some(item) = &self.last {
            list.entry(item);
        }
        list.finish()
    }
}

impl<T: ToTokens> ToTokens for Punctuated<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for (item, punct) in &self.inner {
            item.to_tokens(tokens);
            tokens.append(punct.clone());
        }
        if let Some(item) = &self.last {
            item.to_tokens(tokens);
        }
    }
}
