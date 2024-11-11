#![warn(clippy::pedantic)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Ident, ItemStruct, LitStr};

#[proc_macro_attribute]
pub fn node_location(args: TokenStream, input: TokenStream) -> TokenStream {
    // Parse the input struct
    let input = parse_macro_input!(input as ItemStruct);
    let struct_name = &input.ident;
    let mut inner_field_name: Ident = Ident::new("inner_item", proc_macro2::Span::call_site());

    let name_parser = syn::meta::parser(|meta| -> Result<(), syn::Error> {
        if meta.path.is_ident("inner") {
            let name: LitStr = meta.value()?.parse()?;
            inner_field_name = Ident::new(name.value().as_str(), inner_field_name.span());
            Ok(())
        } else {
            Ok(())
        }
    });

    syn::parse::Parser::parse(name_parser, args).unwrap();

    // Generate the methods for the Node trait implementation
    let expanded = quote! {
        #input

        impl NodeLocation for #struct_name {
            fn source_code(&self) -> Option<String> {
                self.#inner_field_name.ident.span().source_text()
            }

            fn start_line(&self) -> usize {
                self.#inner_field_name.ident.span().start().line as usize
            }

            fn start_col(&self) -> usize {
                self.#inner_field_name.ident.span().start().column as usize
            }

            fn end_line(&self) -> usize {
                self.#inner_field_name.ident.span().end().line as usize
            }

            fn end_col(&self) -> usize {
                self.#inner_field_name.ident.span().end().column as usize
            }
        }
    };

    TokenStream::from(expanded)
}
