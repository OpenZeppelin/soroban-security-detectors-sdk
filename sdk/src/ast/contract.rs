#![warn(clippy::pedantic)]
use super::custom_type::Type;
use super::function::Function;
use super::node::{Location, Node, TLocation};
use super::node_type::{ContractChildType, RcFunction};

use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::rc::Rc;

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Struct {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub methods: RefCell<Vec<RcFunction>>,
}

impl Node for Struct {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = ContractChildType> {
        let methods = self.methods.borrow().clone();
        methods.into_iter().map(ContractChildType::Function)
    }
}

impl Struct {
    #[must_use]
    pub fn contract_name_from_syn_item(contract: &syn::ItemStruct) -> String {
        contract.ident.to_string()
    }

    pub fn get_methods(&self) -> impl Iterator<Item = RcFunction> {
        self.methods.borrow().clone().into_iter()
    }

    pub fn add_method(&self, function: Rc<Function>) {
        self.methods.borrow_mut().push(function);
    }

    pub fn is_struct_contract(struct_item: &syn::ItemStruct) -> bool {
        struct_item
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("contract"))
    }

    pub fn is_struct_contract_type(struct_item: &syn::ItemStruct) -> bool {
        struct_item
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("contracttype"))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        location,
        utils::test::{
            create_mock_contract, create_mock_contract_with_inner_struct, create_mock_function,
        },
    };

    use super::*;
    use syn::{parse_quote, ItemStruct};

    #[test]
    fn test_contract_children_empty() {
        let contract = create_mock_contract(1);
        let mut children = contract.get_methods();
        assert!(
            children.next().is_none(),
            "Contract should have no children initially"
        );
    }

    #[test]
    fn test_contract_children_non_empty() {
        let first_method = Rc::new(create_mock_function(1));
        let second_method = Rc::new(create_mock_function(2));

        let contract = create_mock_contract(1);
        contract.add_method(first_method.clone());
        contract.add_method(second_method.clone());

        let children: Vec<_> = contract.get_methods().collect();
        assert_eq!(children.len(), 2, "Contract should have two children");
        let func = &children[0];
        let func1 = &first_method;
        assert_eq!(Rc::as_ptr(func), Rc::as_ptr(func1));

        let func = &children[1];
        let func2 = &second_method;
        assert_eq!(Rc::as_ptr(func), Rc::as_ptr(func2));
    }

    #[test]
    fn test_contract_name() {
        let item_struct: ItemStruct = parse_quote! {
            struct TestStruct {
                field: u32,
            }
        };
        let contract = create_mock_contract_with_inner_struct(
            1,
            item_struct.ident.to_string(),
            location!(item_struct),
        );
        assert_eq!(contract.name, "TestStruct");
    }
}
