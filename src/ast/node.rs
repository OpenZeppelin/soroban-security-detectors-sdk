#![warn(clippy::pedantic)]
use std::rc::Rc;
use syn::File;
pub enum Type {
    File,
    Contract,
    Function,
    Struct,
    Enum,
}

#[allow(dead_code)]
pub trait Location {
    fn source_code(&self) -> Option<String>;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
}

pub trait Node {
    fn parent(&self) -> Option<Rc<dyn Node>>;
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'a>;
    fn node_type(&self) -> Type;
}

impl Node for File {
    fn parent(&self) -> Option<Rc<dyn Node>> {
        None
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'a> {
        Box::new(Vec::new().into_iter())
    }

    fn node_type(&self) -> Type {
        Type::File
    }
}
