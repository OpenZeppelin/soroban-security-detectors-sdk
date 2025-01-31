#![allow(dead_code)]

use crate::{
    contract::Struct,
    file::File,
    function::{FnParameter, Function},
    node::{Location, Visibility},
    node_type::TypeNode,
    source_code,
};
use std::{cell::RefCell, rc::Rc};

pub(crate) fn create_mock_location() -> Location {
    Location {
        source_code: "fn main() {}".to_string(),
        start_line: 1,
        start_col: 1,
        end_line: 1,
        end_col: 14,
    }
}

pub(crate) fn create_mock_file() -> File {
    create_mock_file_with_name_path("test_mod.rs", "./test_mod.rs")
}

pub(crate) fn create_mock_file_with_inner_struct(item: &syn::File) -> File {
    File {
        id: 1,
        children: RefCell::new(vec![]),
        name: "test_mod.rs".to_string(),
        path: "./test_mod.rs".to_string(),
        attributes: File::attributes_from_file_item(item),
        source_code: source_code!(item),
    }
}

pub(crate) fn create_mock_file_with_name_path(name: &str, path: &str) -> File {
    File {
        id: 1,
        children: RefCell::new(vec![]),
        name: name.to_string(),
        path: path.to_string(),
        attributes: vec![],
        source_code: "fn main() {}".to_string(),
    }
}

pub(crate) fn create_mock_function(id: u128) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        returns: TypeNode::Empty,
        parameters: vec![],
        body: None,
    }
}

pub(crate) fn create_mock_function_with_parameters(
    id: u128,
    parameters: &[Rc<FnParameter>],
) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        parameters: parameters.to_vec(),
        returns: TypeNode::Empty,
        body: None,
    }
}

pub(crate) fn create_mock_contract(id: u128) -> Struct {
    Struct {
        id,
        name: "TestContract".to_string(),
        location: create_mock_location(),
        fields: vec![],
        methods: RefCell::new(vec![]),
        functions: RefCell::new(vec![]),
        type_aliases: RefCell::new(vec![]),
    }
}

pub(crate) fn create_mock_contract_with_inner_struct(
    id: u128,
    name: String,
    location: Location,
) -> Struct {
    Struct {
        id,
        name,
        location,
        fields: vec![],
        methods: RefCell::new(vec![]),
        functions: RefCell::new(vec![]),
        type_aliases: RefCell::new(vec![]),
    }
}
