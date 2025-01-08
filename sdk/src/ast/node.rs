#![warn(clippy::pedantic)]
use std::rc::Rc;

use super::node_type::NodeType;

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

#[cfg(test)]
mod tests {
    use crate::utils::test::create_mock_location;

    use super::*;

    #[test]
    fn test_mock_location() {
        let location = create_mock_location();

        assert_eq!(location.source_code(), Some("fn main() {}".to_string()));
        assert_eq!(location.start_line(), 1);
        assert_eq!(location.start_col(), 1);
        assert_eq!(location.end_line(), 1);
        assert_eq!(location.end_col(), 14);
    }
}
