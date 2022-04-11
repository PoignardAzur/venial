use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt as _};

// Inspired by syn's `Punctuated` type.

/// Comma-separated list of items.
#[derive(Clone)]
pub struct Punctuated<T> {
    /// Vec of items and commas.
    pub inner: Vec<(T, Punct)>,
    /// If true, the last comma shouldn't be printed or considered part of the list.
    pub skip_last: bool,
}

impl<T> Punctuated<T> {
    /// Create a new list.
    pub fn new() -> Self {
        Punctuated {
            inner: Vec::new(),
            skip_last: false,
        }
    }

    /// Add an items at the end of the list.
    ///
    /// If `comma` is None, sets `self.skip_last` to true, and adds a comma
    /// to the array with default values. That comma will be considered as the
    /// item's comma if **push** is called again.
    pub fn push(&mut self, value: T, comma: Option<Punct>) {
        self.skip_last = comma.is_none();
        let comma = comma.unwrap_or_else(|| Punct::new(',', Spacing::Alone));
        self.inner.push((value, comma))
    }

    /// Inserts an element at position `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is greater than the number of elements previously in
    /// this punctuated sequence.
    pub fn insert(&mut self, index: usize, value: T, comma: Option<Punct>) {
        assert!(index <= self.len());

        if index == self.len() {
            self.push(value, comma);
        } else {
            self.inner
                .insert(index, (value, Punct::new(',', Spacing::Alone)));
        }
    }

    /// Return an interator that reads items.
    pub fn items(&self) -> impl Iterator<Item = &T> {
        self.inner.iter().map(|(item, _punct)| item)
    }

    /// Return an interator that reads commas.
    pub fn punct(&self) -> impl Iterator<Item = &Punct> {
        let len = self.inner.len();
        let slice = if self.skip_last && len > 0 {
            &self.inner[..len - 1]
        } else {
            &self.inner[..]
        };
        slice.iter().map(|(_item, punct)| punct)
    }

    /// Return number of items.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Return true if collection has no items.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
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

impl<T> std::ops::DerefMut for Punctuated<T> {
    fn deref_mut(&mut self) -> &mut <Self as std::ops::Deref>::Target {
        &mut self.inner
    }
}
