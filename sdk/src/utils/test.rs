#![allow(dead_code)]

use crate::ast::custom_type::{Type, Typename};
use crate::{
    contract::{Contract, Struct},
    file::File,
    function::{FnParameter, Function},
    node::{Location, Visibility},
    source_code,
};
use std::{cell::RefCell, rc::Rc};

pub(crate) fn create_mock_location() -> Location {
    Location {
        source: "fn main() {}".to_string(),
        offset_start: 1,
        offset_end: 1,
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 1,
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
        location: create_mock_location(),
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
        location: create_mock_location(),
    }
}

pub(crate) fn create_mock_function(id: u32) -> Function {
    Function {
        id,
        attributes: Vec::new(),
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        generics: Vec::new(),
        parameters: vec![],
        returns: Type::Typename(Rc::new(Typename {
            id: 0,
            location: create_mock_location(),
            name: "()".to_string(),
        })),
        body: None,
    }
}

pub(crate) fn create_mock_function_with_parameters(
    id: u32,
    parameters: &[Rc<FnParameter>],
) -> Function {
    Function {
        id,
        attributes: Vec::new(),
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        generics: Vec::new(),
        parameters: parameters.to_vec(),
        returns: Type::Typename(Rc::new(Typename {
            id: 0,
            location: create_mock_location(),
            name: "()".to_string(),
        })),
        body: None,
    }
}

pub(crate) fn create_mock_contract(id: u32) -> Contract {
    Contract {
        id,
        name: "TestContract".to_string(),
        location: create_mock_location(),
        fields: vec![],
        methods: RefCell::new(vec![]),
        functions: RefCell::new(vec![]),
        type_aliases: RefCell::new(vec![]),
        constants: RefCell::new(vec![]),
        macros: RefCell::new(vec![]),
        plane_defs: RefCell::new(vec![]),
    }
}

pub(crate) fn create_mock_contract_with_inner_struct(
    id: u32,
    name: String,
    location: Location,
) -> Struct {
    Struct {
        id,
        attributes: Vec::new(),
        name,
        location,
        fields: vec![],
        is_contract: false,
    }
}
