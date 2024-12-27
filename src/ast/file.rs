#![warn(clippy::pedantic)]
use std::rc::Rc;

use super::node::Node;
use super::node_type::{FileChildType, NodeType};

pub struct File {
    pub id: usize,
    pub(crate) inner_struct: Rc<syn::File>,
    pub children: Vec<Rc<FileChildType>>,

    pub name: String,
    pub path: String,
}

impl Node for File {
    fn parent(&self) -> Option<Rc<NodeType>> {
        None
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = Rc<FileChildType>> {
        self.children.clone().into_iter()
    }
}

impl File {
    #[must_use]
    pub fn name(&self) -> String {
        self.name.clone()
    }

    #[must_use]
    pub fn has_no_std(&self) -> bool {
        self.inner_struct
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("no_std"))
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::test::{create_mock_file, create_mock_file_with_inner_struct};

    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_parent() {
        let file = create_mock_file();
        assert!(file.parent().is_none(), "File node should have no parent");
    }

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
        let file = create_mock_file_with_inner_struct(parsed_file.clone());
        assert!(file.has_no_std(), "File should have no_std attribute");

        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(parsed_file.clone());
        assert!(!file.has_no_std(), "File should not have no_std attribute");
    }
}
