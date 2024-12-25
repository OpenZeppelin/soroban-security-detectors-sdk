#![warn(clippy::pedantic)]
use std::rc::Rc;

use macro_lib::node_location;
use syn::ItemFn;

use super::node::{InnerStructIdentifier, Location, Node};
use super::node_type::{FunctionChildType, FunctionParentType, NodeType};

#[node_location(inner = "inner_struct")]
pub struct Function {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemFn>,
    pub parent: Rc<FunctionParentType>,
    pub children: Vec<Rc<FunctionChildType>>,
}

impl InnerStructIdentifier for ItemFn {
    fn identifier(&self) -> syn::Ident {
        self.sig.ident.clone()
    }
}

impl Node for Function {
    fn parent(&self) -> Option<Rc<NodeType>> {
        match self.parent.as_ref() {
            FunctionParentType::File(file) => Some(Rc::new(NodeType::File(file.clone()))),
            FunctionParentType::Contract(contract) => {
                Some(Rc::new(NodeType::Contract(contract.clone())))
            }
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = Rc<FunctionChildType>> {
        self.children.clone().into_iter()
    }
}

impl Function {
    #[must_use]
    pub fn name(&self) -> String {
        self.inner_struct.sig.ident.to_string()
    }
}
