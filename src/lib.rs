pub mod parse;
pub mod types;

use proc_macro2::{TokenStream, TokenTree};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeclType {
    Struct,
    Enum,
}

pub fn get_declaration_type(tokens: TokenStream) -> DeclType {
    let first_token = tokens.into_iter().next().unwrap();

    if let TokenTree::Ident(ident) = first_token {
        dbg!(&ident);

        if ident == "struct" {
            DeclType::Struct
        } else if ident == "enum" {
            DeclType::Enum
        } else {
            panic!("can't recognize type")
        }
    } else {
        panic!("first token isn't a type");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn get_struct_type() {
        let struct_type = get_declaration_type(quote!(
            struct Hello;
        ));

        assert_eq!(struct_type, DeclType::Struct);
    }
}
