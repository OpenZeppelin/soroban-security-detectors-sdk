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
    fn children(&self) -> Box<impl Iterator<Item = Rc<NodeType>>>;
}

impl Node for File {
    fn parent(&self) -> Option<Rc<NodeType>> {
        None
    }

    fn children(&self) -> Box<impl Iterator<Item = Rc<NodeType>>> {
        Box::new(Vec::new().into_iter())
    }
}
