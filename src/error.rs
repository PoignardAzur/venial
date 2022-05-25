// Most of this code is copy-pasted from syn roughly as-is.

use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::ToTokens;
use std::fmt::{Debug, Display};

/// Convenient error type for displaying errors in your macros to users.
///
/// # Error reporting in proc macros
///
/// The correct way to report errors back to the compiler from a procedural
/// macro is by emitting an appropriately spanned invocation of
/// [`compile_error!`] in the generated code. This produces a better diagnostic
/// message than simply panicking the macro.
///
/// This type provides a convenient [`.to_compile_error()`] method that returns
/// `compile_error!("Your error message")` in TokenStream form.
///
/// [`compile_error!`]: std::compile_error!
///
/// ```
/// # extern crate proc_macro;
/// #
/// # use proc_macro2::TokenStream;
/// # use venial::{parse_declaration, Declaration, Struct, Error};
/// #
/// # const IGNORE: &str = stringify! {
/// #[proc_macro_derive(MyDerive)]
/// # };
/// pub fn my_derive(input: TokenStream) -> TokenStream {
///     let input = parse_declaration(input);
///
///     let parse_res = match input {
///         Err(error) => Err(error),
///         Ok(Declaration::Struct(struct_decl)) => parse_my_struct(struct_decl),
///         Ok(_) => Err(Error::new("Error in my_derive macro: only structs are accepted")),
///     };
///
///     parse_res
///         .unwrap_or_else(|err| err.to_compile_error())
///         .into()
/// }
///
/// pub fn parse_my_struct(input: Struct) -> Result<TokenStream, Error> {
///     // ...
///     # unimplemented!()
/// }
/// ```

#[derive(Clone)]
pub struct Error {
    messages: Vec<ErrorMessage>,
}

#[derive(Clone)]
struct ErrorMessage {
    start_span: Span,
    end_span: Span,
    message: String,
}

impl Error {
    /// Create a new error message with the given text.
    ///
    /// The span will be located at the macro's call site.
    pub fn new<T: Display>(message: T) -> Self {
        Error {
            messages: vec![ErrorMessage {
                start_span: Span::call_site(),
                end_span: Span::call_site(),
                message: message.to_string(),
            }],
        }
    }

    /// Create a new error message with the given text and span.
    pub fn new_at_span<T: Display>(span: Span, message: T) -> Self {
        Error {
            messages: vec![ErrorMessage {
                start_span: span,
                end_span: span,
                message: message.to_string(),
            }],
        }
    }

    /// Create a new error message with the given text.
    ///
    /// Venial will do its best to locate the message at a span encompassing
    /// all the given tokens.
    pub fn new_at_tokens<T: ToTokens, U: Display>(tokens: T, message: U) -> Self {
        let mut iter = tokens.into_token_stream().into_iter();
        let start = iter.next().map_or_else(Span::call_site, |t| t.span());
        let end = iter.last().map_or(start, |t| t.span());
        Error {
            messages: vec![ErrorMessage {
                start_span: start,
                end_span: end,
                message: message.to_string(),
            }],
        }
    }

    /// Returns a span in the first error.
    ///
    /// Note that, if span merging isn't enabled (currently nightly-only), the
    /// returned span will the smalled than the span that `self.to_compile_error()`
    /// inhabits.
    pub fn span(&self) -> Span {
        let start = self.messages[0].start_span;
        let end = self.messages[0].end_span;
        start.join(end).unwrap_or(start)
    }

    /// Render the error as an invocation of [`compile_error!`].
    ///
    /// [`compile_error!`]: std::compile_error!
    pub fn to_compile_error(&self) -> TokenStream {
        self.messages
            .iter()
            .map(ErrorMessage::to_compile_error)
            .collect()
    }

    /// Add another error message to self such that when `to_compile_error()` is
    /// called, both errors will be emitted together.
    pub fn combine(&mut self, another: Error) {
        self.messages.extend(another.messages);
    }
}

impl ErrorMessage {
    fn to_compile_error(&self) -> TokenStream {
        // compile_error!($message)
        TokenStream::from_iter(vec![
            TokenTree::Ident(Ident::new("compile_error", self.start_span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(self.start_span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(self.end_span);
                        string
                    })])
                });
                group.set_span(self.end_span);
                group
            }),
        ])
    }
}

// -- Trait impls --

impl std::error::Error for Error {}

impl Debug for Error {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.messages.len() == 1 {
            formatter
                .debug_tuple("Error")
                .field(&self.messages[0])
                .finish()
        } else {
            formatter
                .debug_tuple("Error")
                .field(&self.messages)
                .finish()
        }
    }
}

impl Debug for ErrorMessage {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        Debug::fmt(&self.message, formatter)
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str(&self.messages[0].message)
    }
}
