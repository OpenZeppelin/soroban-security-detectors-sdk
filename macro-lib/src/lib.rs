#![warn(clippy::pedantic)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Ident, ItemStruct, LitStr};

/// This macro generates the implementation of the `NodeLocation` trait for a struct.
/// The struct must have a field that is a `syn::Item`, which is the item that the struct represents.
///
/// # Panics
/// This macro will panic if the struct does not have a field that is a `syn::Item`.
#[proc_macro_attribute]
pub fn node_location(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let struct_name = &input.ident;
    let mut inner_field_name: Ident = Ident::new("location", proc_macro2::Span::call_site());

    let name_parser = syn::meta::parser(|meta| -> Result<(), syn::Error> {
        if meta.path.is_ident("location") {
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

        impl TLocation for #struct_name {
            fn source_code(&self) -> Option<String> {
                match &self.#inner_field_name.source_code {
                    Some(source_code) => Some(source_code.clone()),
                    None => None,
                }
            }

            fn start_line(&self) -> usize {
                self.#inner_field_name.start_line
            }

            fn start_col(&self) -> usize {
                self.#inner_field_name.start_col
            }

            fn end_line(&self) -> usize {
                self.#inner_field_name.end_line
            }

            fn end_col(&self) -> usize {
                self.#inner_field_name.end_col
            }
        }

    };

    TokenStream::from(expanded)
}
