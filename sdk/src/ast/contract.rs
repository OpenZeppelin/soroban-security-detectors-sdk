#![warn(clippy::pedantic)]
use super::function::Function;
use super::node::{Location, Node};
use super::node_type::{ContractChildType, ContractParentType, NodeType};
use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::rc::Rc;
use syn::spanned::Spanned;
use syn::ItemStruct;

#[node_location(inner = "inner_struct")]
pub struct Contract {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemStruct>,
    pub parent: ContractParentType,
    pub children: RefCell<Vec<ContractChildType>>,
}

impl Node for Contract {
    fn parent(&self) -> Option<NodeType> {
        match &self.parent {
            ContractParentType::File(file) => Some(NodeType::File(file.clone())),
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = ContractChildType> {
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
            if let ContractChildType::Function(function) = child {
                res.push(function.clone());
            }
        }
        res.into_iter()
    }

    pub fn add_function(&self, function: Rc<Function>) {
        self.children
            .borrow_mut()
            .push(ContractChildType::Function(function));
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
        let parent_node = ContractParentType::File(parent_file.clone());
        let contract = create_mock_contract_with_parent(1, parent_node);
        let parent = contract.parent();
        assert!(parent.is_some(), "Contract should have a parent node");
        match parent.unwrap() {
            NodeType::File(file) => {
                assert_eq!(Rc::as_ptr(&parent_file), Rc::as_ptr(&file));
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
        let child_node1 = ContractChildType::Function(Rc::new(create_mock_function(1)));
        let child_node2 = ContractChildType::Function(Rc::new(create_mock_function(2)));

        let contract = create_mock_contract(1);
        contract.children.borrow_mut().push(child_node1.clone());
        contract.children.borrow_mut().push(child_node2.clone());

        let children: Vec<_> = contract.children().collect();
        assert_eq!(children.len(), 2, "Contract should have two children");
        if let ContractChildType::Function(ref func) = children[0] {
            if let ContractChildType::Function(ref func1) = child_node1 {
                assert_eq!(Rc::as_ptr(func), Rc::as_ptr(func1));
            } else {
                panic!("Expected a function child");
            }
        } else {
            panic!("Expected a function child");
        }
        if let ContractChildType::Function(ref func) = children[1] {
            if let ContractChildType::Function(ref func2) = child_node2 {
                assert_eq!(Rc::as_ptr(func), Rc::as_ptr(func2));
            } else {
                panic!("Expected a function child");
            }
        } else {
            panic!("Expected a function child");
        }
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
}
