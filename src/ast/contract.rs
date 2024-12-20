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

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_contract_parent() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let parent_node = Rc::new(NodeType::Struct);

        let contract = Contract {
            id: 1,
            inner_struct: Rc::new(item_struct),
            parent: parent_node.clone(),
            children: vec![],
        };

        let parent = contract.parent();
        assert!(parent.is_some(), "Contract should have a parent node");
        assert_eq!(Rc::as_ptr(&parent.unwrap()), Rc::as_ptr(&parent_node));
    }

    #[test]
    fn test_contract_children_empty() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let parent_node = Rc::new(NodeType::Struct);

        let contract = Contract {
            id: 1,
            inner_struct: Rc::new(item_struct),
            parent: parent_node,
            children: vec![],
        };

        let mut children = contract.children();
        assert!(
            children.next().is_none(),
            "Contract should have no children initially"
        );
    }

    #[test]
    fn test_contract_children_non_empty() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let parent_node = Rc::new(NodeType::Struct);
        let child_node1 = Rc::new(NodeType::Function);
        let child_node2 = Rc::new(NodeType::Enum);

        let contract = Contract {
            id: 1,
            inner_struct: Rc::new(item_struct),
            parent: parent_node,
            children: vec![child_node1.clone(), child_node2.clone()],
        };

        let children: Vec<_> = contract.children().collect();
        assert_eq!(children.len(), 2, "Contract should have two children");
        assert_eq!(Rc::as_ptr(&children[0]), Rc::as_ptr(&child_node1));
        assert_eq!(Rc::as_ptr(&children[1]), Rc::as_ptr(&child_node2));
    }
}
