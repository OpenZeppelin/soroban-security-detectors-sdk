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
    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_parent() {
        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = File {
            id: 0,
            inner_struct: Rc::new(parsed_file),
            children: Vec::new(),
            name: "main.rs".to_string(),
            path: "./main.rs".to_string(),
        };

        assert!(file.parent().is_none(), "File node should have no parent");
    }

    #[test]
    fn test_file_as_node_children() {
        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = File {
            id: 0,
            inner_struct: Rc::new(parsed_file),
            children: Vec::new(),
            name: "main.rs".to_string(),
            path: "./main.rs".to_string(),
        };

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
        let file = File {
            id: 0,
            inner_struct: Rc::new(parsed_file),
            children: Vec::new(),
            name: "main.rs".to_string(),
            path: "./main.rs".to_string(),
        };
        assert!(file.has_no_std(), "File should have no_std attribute");

        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = File {
            id: 0,
            inner_struct: Rc::new(parsed_file),
            children: Vec::new(),
            name: "main.rs".to_string(),
            path: "./main.rs".to_string(),
        };
        assert!(!file.has_no_std(), "File should not have no_std attribute");
    }
}
