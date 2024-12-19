#![warn(clippy::pedantic)]
use std::rc::Rc;

use syn::File;

use super::node_type::NodeType;

#[allow(dead_code)]
pub trait Location {
    fn source_code(&self) -> Option<String>;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
}

pub trait Node {
    fn parent(&self) -> Option<Rc<NodeType>>;
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<NodeType>> + 'a>;
}

impl Node for File {
    fn parent(&self) -> Option<Rc<NodeType>> {
        None
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<NodeType>> + 'a> {
        Box::new(Vec::new().into_iter())
    }
}
