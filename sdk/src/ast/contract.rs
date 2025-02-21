#![warn(clippy::pedantic)]
use crate::ast_node;

use super::custom_type::{Type, TypeAlias};
use super::definition::{Const, Plane};
use super::function::Function;
use super::misc::Macro;
use super::node::{Location, Node, TLocation};
use super::node_type::{ContractChildType, RcFunction};

use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::rc::Rc;

ast_node! {
    pub struct Struct {
        pub name: String,
        pub fields: Vec<(String, Type)>,
    }
}

pub struct Contract {
    name: String,
    location: Location,
    fields: Vec<(String, Type)>,
    methods: RefCell<Vec<RcFunction>>,
    functions: RefCell<Vec<RcFunction>>,
    type_aliases: RefCell<Vec<Rc<TypeAlias>>>,
    constants: RefCell<Vec<Rc<Const>>>,
    macros: RefCell<Vec<Rc<Macro>>>,
    plane_defs: RefCell<Vec<Rc<Plane>>>,
}

impl Contract {
    pub fn new(name: String, location: Location, fields: Vec<(String, Type)>) -> Self {
        Self {
            name,
            location,
            fields,
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn fields(&self) -> Vec<(String, Type)> {
        self.fields.clone()
    }

    pub fn methods(&self) -> Vec<RcFunction> {
        self.methods.borrow().clone()
    }

    pub fn add_method(&self, method: RcFunction) {
        self.methods.borrow_mut().push(method);
    }

    pub fn functions(&self) -> Vec<RcFunction> {
        self.functions.borrow().clone()
    }

    pub fn add_function(&self, function: RcFunction) {
        self.functions.borrow_mut().push(function);
    }

    pub fn type_aliases(&self) -> Vec<Rc<TypeAlias>> {
        self.type_aliases.borrow().clone()
    }

    pub fn add_type_alias(&self, type_alias: Rc<TypeAlias>) {
        self.type_aliases.borrow_mut().push(type_alias);
    }

    pub fn constants(&self) -> Vec<Rc<Const>> {
        self.constants.borrow().clone()
    }

    pub fn add_constant(&self, constant: Rc<Const>) {
        self.constants.borrow_mut().push(constant);
    }

    pub fn macros(&self) -> Vec<Rc<Macro>> {
        self.macros.borrow().clone()
    }

    pub fn add_macro(&self, macro_: Rc<Macro>) {
        self.macros.borrow_mut().push(macro_);
    }

    pub fn plane_defs(&self) -> Vec<Rc<Plane>> {
        self.plane_defs.borrow().clone()
    }

    pub fn add_plane_def(&self, plane_def: Rc<Plane>) {
        self.plane_defs.borrow_mut().push(plane_def);
    }
}

impl Node for Struct {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = ContractChildType> {
        vec![].into_iter()
    }
}

impl Struct {
    #[must_use]
    pub fn contract_name_from_syn_item(contract: &syn::ItemStruct) -> String {
        contract.ident.to_string()
    }

    #[must_use = "Use this method to check if the syn struct has a `contract` attribute"]
    pub fn is_struct_contract(struct_item: &syn::ItemStruct) -> bool {
        struct_item
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("contract"))
    }

    #[must_use = "Use this method to check if the syn struct has a `contracttype` attribute"]
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
        let children_len = contract.methods.borrow().len();
        assert!(
            children_len == 0,
            "Contract should have no children initially"
        );
    }

    #[test]
    fn test_contract_children_non_empty() {
        let first_method = Rc::new(create_mock_function(1));
        let second_method = Rc::new(create_mock_function(2));

        let contract = create_mock_contract(1);
        contract.methods.borrow_mut().push(first_method.clone());
        contract.methods.borrow_mut().push(second_method.clone());

        let children: Vec<_> = contract.methods.borrow().clone();
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

    #[test]
    fn test_children_function() {
        let first_method = Rc::new(create_mock_function(1));
        let second_method = Rc::new(create_mock_function(2));

        let contract = create_mock_contract(1);
        contract.methods.borrow_mut().push(first_method.clone());
        contract.methods.borrow_mut().push(second_method.clone());

        let children: Vec<_> = contract.children().collect();
        assert_eq!(children.len(), 2, "Contract should have two children");

        if let ContractChildType::Function(func) = &children[0] {
            assert_eq!(Rc::as_ptr(func), Rc::as_ptr(&first_method));
        } else {
            panic!("Expected ContractChildType::Function");
        }

        if let ContractChildType::Function(func) = &children[1] {
            assert_eq!(Rc::as_ptr(func), Rc::as_ptr(&second_method));
        } else {
            panic!("Expected ContractChildType::Function");
        }
    }
}
