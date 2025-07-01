use super::custom_type::{Type, TypeAlias};
use super::definition::{Const, Definition, Plane};
use super::misc::Macro;
use super::node::{Location, Node};
use super::node_type::{NodeKind, RcFunction};
use crate::node::Visibility;
use crate::{ast_nodes, ast_nodes_impl};

use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

ast_nodes! {
    pub struct Struct {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub fields: Vec<(String, Type)>,
        pub is_contract: bool,
    }

    pub struct Contract {
        pub name: String,
        pub fields: Vec<(String, Type)>,
        pub methods: RefCell<Vec<RcFunction>>,
        pub functions: RefCell<Vec<RcFunction>>,
        pub type_aliases: RefCell<Vec<Rc<TypeAlias>>>,
        pub constants: RefCell<Vec<Rc<Const>>>,
        pub macros: RefCell<Vec<Rc<Macro>>>,
        pub plane_defs: RefCell<Vec<Rc<Plane>>>,
    }
}

ast_nodes_impl! {
    impl Node for Struct {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.fields
                .iter()
                .map(|(_, ty)| NodeKind::Type(ty.clone()))
                .collect()
        }
    }

    impl Node for Contract {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            let methods = self.methods.borrow().clone();
            let functions = self.functions.borrow().clone();
            let type_aliases = self.type_aliases.borrow().clone();
            let constants = self.constants.borrow().clone();
            let macros = self.macros.borrow().clone();
            let plane_defs = self.plane_defs.borrow().clone();
            let mut children: Vec<NodeKind> = vec![];
            children.extend(
                methods
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::Function(child.clone()))),
            );
            children.extend(
                functions
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::Function(child.clone()))),
            );
            children.extend(
                type_aliases
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::AssocType(child.clone()))),
            );
            children.extend(
                constants
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::Const(child.clone()))),
            );
            children.extend(
                macros
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::Macro(child.clone()))),
            );
            children.extend(
                plane_defs
                    .iter()
                    .map(|child| NodeKind::Definition(Definition::Plane(child.clone()))),
            );
            children.into_iter().collect()
        }
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

impl Contract {
    pub fn functions(&self) -> impl Iterator<Item = RcFunction> {
        let mut res = self.functions.borrow().clone();
        res.sort_by(|a, b| a.name.cmp(&b.name));
        res.into_iter()
    }

    pub fn functions_count(&self) -> usize {
        self.functions.borrow().len()
    }

    pub fn methods(&self) -> impl Iterator<Item = RcFunction> {
        let mut res = self.methods.borrow().clone();
        res.sort_by(|a, b| a.name.cmp(&b.name));
        res.into_iter()
    }

    pub fn methods_count(&self) -> usize {
        self.methods.borrow().len()
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

        let children: Vec<_> = contract.children();
        assert_eq!(children.len(), 2, "Contract should have two children");

        if let NodeKind::Definition(Definition::Function(func)) = &children[0] {
            assert_eq!(Rc::as_ptr(func), Rc::as_ptr(&first_method));
        } else {
            panic!("Expected NodeKind::Definition::Function");
        }

        if let NodeKind::Definition(Definition::Function(func)) = &children[1] {
            assert_eq!(Rc::as_ptr(func), Rc::as_ptr(&second_method));
        } else {
            panic!("Expected NodeKind::Definition::Function");
        }
    }
}
