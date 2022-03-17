#![allow(missing_docs)]

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt as _};

/// Inspired by syn's `Punctuated` type.
#[derive(Clone)]
pub struct Punctuated<T> {
    pub inner: Vec<(T, Punct)>,
    pub skip_last: bool,
}

impl<T> Punctuated<T> {
    pub fn new() -> Self {
        Punctuated {
            inner: Vec::new(),
            skip_last: false,
        }
    }

    pub fn push(&mut self, value: T, period: Option<Punct>) {
        self.skip_last = period.is_none();
        let period = period.unwrap_or(Punct::new(',', Spacing::Alone));
        self.inner.push((value, period))
    }

    /// Inserts an element at position `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is greater than the number of elements previously in
    /// this punctuated sequence.
    pub fn insert(&mut self, index: usize, value: T, period: Option<Punct>) {
        assert!(index <= self.len());

        if index == self.len() {
            self.push(value, period);
        } else {
            self.inner
                .insert(index, (value, Punct::new(',', Spacing::Alone)));
        }
    }

    pub fn items(&self) -> impl Iterator<Item = &T> {
        self.inner.iter().map(|(item, _punct)| item)
    }

    pub fn punct(&self) -> impl Iterator<Item = &Punct> {
        let len = self.inner.len();
        let slice = if self.skip_last && len > 0 {
            &self.inner[..len - 1]
        } else {
            &self.inner[..]
        };
        slice.iter().map(|(_item, punct)| punct)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
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
        list.finish()
    }
}

impl<T: ToTokens> ToTokens for Punctuated<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.inner.is_empty() {
            return;
        }

        for (item, punct) in &self.inner[..self.inner.len() - 1] {
            item.to_tokens(tokens);
            tokens.append(punct.clone());
        }
        self.inner.last().unwrap().0.to_tokens(tokens);
        if !self.skip_last {
            tokens.append(self.inner.last().unwrap().1.clone());
        }
    }
}

impl<T> std::ops::Deref for Punctuated<T> {
    type Target = [(T, Punct)];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
