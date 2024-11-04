#![warn(clippy::pedantic)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Field, FieldMutability, Fields, Ident, ItemStruct, Type, Visibility};

#[proc_macro_attribute]
pub fn ast_node(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(item as ItemStruct);
    let fields_to_add = vec![
        ("start_line", "u32"),
        ("start_col", "u32"),
        ("end_line", "u32"),
        ("end_col", "u32"),
    ];

    match &mut ast.fields {
        Fields::Named(fields_named) => {
            for (field_name, field_type) in fields_to_add {
                let field_ident = Ident::new(field_name, proc_macro2::Span::call_site());
                let field_type: Type = syn::parse_str(field_type).unwrap();

                let new_field = Field {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: Some(field_ident),
                    colon_token: Some(Default::default()),
                    ty: field_type,
                };
                fields_named.named.push(new_field);
            }
        }
        _ => panic!("Only named fields are supported"),
    }

    let result = quote! {
        #ast
    };

    result.into()
}
