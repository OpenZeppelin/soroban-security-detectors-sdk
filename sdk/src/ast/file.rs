#![warn(clippy::pedantic)]
use std::cell::RefCell;

use super::node::Node;
use super::node_type::FileChildType;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct File {
    pub id: u128,
    pub children: RefCell<Vec<FileChildType>>,
    pub name: String,
    pub path: String,
    pub attributes: Vec<String>,
    pub source_code: String,
}

impl Node for File {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FileChildType> {
        self.children.borrow().clone().into_iter()
    }
}

impl File {
    #[must_use]
    pub fn has_no_std(&self) -> bool {
        self.attributes.contains(&"no_std".to_string())
    }

    #[must_use]
    pub fn attributes_from_file_item(file: &syn::File) -> Vec<String> {
        file.attrs
            .iter()
            .map(|attr| attr.path().segments[0].ident.to_string())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::test::{create_mock_file, create_mock_file_with_inner_struct};

    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_children() {
        let file = create_mock_file();
        let mut children = file.children();
        assert!(
            children.next().is_none(),
            "File node should have no children"
        );
    }

    #[test]
    fn test_file_has_no_std() {
        let source = "#![no_std]\nfn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(&parsed_file);
        assert!(file.has_no_std(), "File should have no_std attribute");

        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(&parsed_file);
        assert!(!file.has_no_std(), "File should not have no_std attribute");
    }
}
