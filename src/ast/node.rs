#![warn(clippy::pedantic)]
use std::{iter, rc::Rc};

use syn::File;

use super::node_type::{FileChildType, NodeType};

#[allow(dead_code)]
pub trait Location {
    fn source_code(&self) -> Option<String>;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
}

pub trait InnerStructIdentifier {
    fn identifier(&self) -> syn::Ident;
}

pub trait Node {
    fn parent(&self) -> Option<Rc<NodeType>>;
    fn children(&self) -> impl Iterator;
}

impl Node for File {
    fn parent(&self) -> Option<Rc<NodeType>> {
        None
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = Rc<FileChildType>> {
        iter::empty::<Rc<FileChildType>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_parent() {
        let source = "fn main() {}";
        let parsed_file: File = parse_file(source).expect("Failed to parse file");

        assert!(
            parsed_file.parent().is_none(),
            "File node should have no parent"
        );
    }

    #[test]
    fn test_file_as_node_children() {
        let source = "fn main() {}";
        let parsed_file: File = parse_file(source).expect("Failed to parse file");

        let mut children = parsed_file.children();
        assert!(
            children.next().is_none(),
            "File node should have no children"
        );
    }

    struct MockLocation {
        source: Option<String>,
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    }

    impl Location for MockLocation {
        fn source_code(&self) -> Option<String> {
            self.source.clone()
        }

        fn start_line(&self) -> usize {
            self.start_line
        }

        fn start_col(&self) -> usize {
            self.start_col
        }

        fn end_line(&self) -> usize {
            self.end_line
        }

        fn end_col(&self) -> usize {
            self.end_col
        }
    }

    #[test]
    fn test_mock_location() {
        let location = MockLocation {
            source: Some("fn main() {}".to_string()),
            start_line: 1,
            start_col: 1,
            end_line: 1,
            end_col: 14,
        };

        assert_eq!(location.source_code(), Some("fn main() {}".to_string()));
        assert_eq!(location.start_line(), 1);
        assert_eq!(location.start_col(), 1);
        assert_eq!(location.end_line(), 1);
        assert_eq!(location.end_col(), 14);
    }
}
