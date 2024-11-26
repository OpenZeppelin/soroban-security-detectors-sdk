#![warn(clippy::pedantic)]
use std::rc::Rc;

use macro_lib::node_location;

use super::node::Type as NodeType;
use super::node::{Location, Node};
use syn::{File, ItemStruct};

#[node_location(inner = "inner_struct")]
pub struct Contract {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemStruct>,
    pub parent: Rc<File>,
    pub children: Vec<Rc<dyn Node>>,
}

impl Node for Contract {
    fn parent(&self) -> Option<Rc<dyn Node>> {
        Some(self.parent.clone())
    }

    fn children<'n>(&'n self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'n> {
        Box::new(self.children.iter().cloned())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Contract
    }
}
