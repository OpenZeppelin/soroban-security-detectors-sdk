#![warn(clippy::pedantic)]
use std::cell::RefCell;
use std::rc::Rc;

use macro_lib::node_location;

use super::function::Function;
use super::node::{InnerStructIdentifier, Location, Node};
use super::node_type::{ContractChildType, ContractParentType, NodeType};
use syn::ItemStruct;

#[node_location(inner = "inner_struct")]
pub struct Contract {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemStruct>,
    pub parent: Rc<ContractParentType>,
    pub children: RefCell<Vec<Rc<ContractChildType>>>,
}

impl InnerStructIdentifier for ItemStruct {
    fn identifier(&self) -> syn::Ident {
        self.ident.clone()
    }
}

impl Node for Contract {
    fn parent(&self) -> Option<Rc<NodeType>> {
        match self.parent.as_ref() {
            ContractParentType::File(file) => Some(Rc::new(NodeType::File(file.clone()))),
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = Rc<ContractChildType>> {
        self.children.borrow().clone().into_iter()
    }
}

impl Contract {
    #[must_use]
    pub fn name(&self) -> String {
        self.inner_struct.ident.to_string()
    }

    pub fn functions(&self) -> impl Iterator<Item = Rc<Function>> {
        let mut res = Vec::new();
        for child in self.children.borrow().iter() {
            if let ContractChildType::Function(function) = child.as_ref() {
                res.push(function.clone());
            }
        }
        res.into_iter()
    }

    pub fn add_function(&self, function: Rc<Function>) {
        self.children
            .borrow_mut()
            .push(Rc::new(ContractChildType::Function(function)));
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::test::{
        create_mock_contract, create_mock_contract_with_inner_struct,
        create_mock_contract_with_parent, create_mock_file, create_mock_function,
    };

    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_contract_parent() {
        let parent_file = Rc::new(create_mock_file());
        let parent_node = Rc::new(ContractParentType::File(parent_file.clone()));
        let contract = create_mock_contract_with_parent(1, parent_node);
        let parent = contract.parent();
        assert!(parent.is_some(), "Contract should have a parent node");
        match parent.unwrap().as_ref() {
            NodeType::File(file) => {
                assert_eq!(Rc::as_ptr(&parent_file), Rc::as_ptr(file));
            }
            _ => panic!("Contract parent should be a file"),
        }
    }

    #[test]
    fn test_contract_children_empty() {
        let contract = create_mock_contract(1);
        let mut children = contract.children();
        assert!(
            children.next().is_none(),
            "Contract should have no children initially"
        );
    }

    #[test]
    fn test_contract_children_non_empty() {
        let child_node1 = Rc::new(ContractChildType::Function(Rc::new(create_mock_function(
            1,
        ))));
        let child_node2 = Rc::new(ContractChildType::Function(Rc::new(create_mock_function(
            2,
        ))));

        let contract = create_mock_contract(1);
        contract.children.borrow_mut().push(child_node1.clone());
        contract.children.borrow_mut().push(child_node2.clone());

        let children: Vec<_> = contract.children().collect();
        assert_eq!(children.len(), 2, "Contract should have two children");
        assert_eq!(Rc::as_ptr(&children[0]), Rc::as_ptr(&child_node1));
        assert_eq!(Rc::as_ptr(&children[1]), Rc::as_ptr(&child_node2));
    }

    #[test]
    fn test_contract_name() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let contract = create_mock_contract_with_inner_struct(1, item_struct);
        assert_eq!(contract.name(), "TestStruct");
    }

    #[test]
    fn test_contract_identifier() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let contract = create_mock_contract_with_inner_struct(1, item_struct);
        assert_eq!(contract.inner_struct.identifier().to_string(), "TestStruct");
    }
}
