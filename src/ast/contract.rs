#![warn(clippy::pedantic)]
use std::rc::Rc;

use macro_lib::node_location;

use super::node::{Location, Node};
use super::node_type::NodeType;
use syn::ItemStruct;

#[node_location(inner = "inner_struct")]
pub struct Contract {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemStruct>,
    pub parent: Rc<NodeType>,
    pub children: Vec<Rc<NodeType>>,
}

impl Node for Contract {
    fn parent(&self) -> Option<Rc<NodeType>> {
        Some(self.parent.clone())
    }

    fn children(&self) -> impl Iterator<Item = Rc<NodeType>> {
        self.children.iter().cloned()
    }
}
