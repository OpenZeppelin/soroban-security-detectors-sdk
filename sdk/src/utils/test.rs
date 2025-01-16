use crate::{
    contract::Contract,
    file::File,
    function::{FnParameter, Function},
    node::{Location, Visibility},
    node_type::{FunctionChildType, TypeNode},
    source_code,
};
use std::{cell::RefCell, rc::Rc};

#[allow(dead_code)]
pub(crate) fn create_mock_location() -> Location {
    Location {
        source_code: "fn main() {}".to_string(),
        start_line: 1,
        start_col: 1,
        end_line: 1,
        end_col: 14,
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_file() -> File {
    create_mock_file_with_name_path("test_mod.rs", "./test_mod.rs")
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
pub(crate) fn create_mock_function(id: u128) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        children: RefCell::new(vec![]),
        returns: TypeNode::Empty,
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_parameters(
    id: u128,
    parameters: &[Rc<FnParameter>],
) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        children: RefCell::new(
            parameters
                .iter()
                .map(|p| FunctionChildType::Parameter(p.clone()))
                .collect::<Vec<_>>(),
        ),
        returns: TypeNode::Empty,
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract(id: u128) -> Contract {
    Contract {
        id,
        name: "TestContract".to_string(),
        location: create_mock_location(),
        children: RefCell::new(vec![]),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract_with_inner_struct(
    id: u128,
    name: String,
    location: Location,
) -> Contract {
    Contract {
        id,
        name,
        location,
        children: RefCell::new(vec![]),
    }
}
