use crate::{
    contract::Contract,
    file::File,
    function::{FnParameter, Function},
    node::{Location, Visibility},
    node_type::{ContractParentType, FunctionParentType, TypeNode},
    source_code,
};
use std::{cell::RefCell, rc::Rc};

#[allow(dead_code)]
pub(crate) fn create_mock_location() -> Location {
    Location {
        source_code: Some("fn main() {}".to_string()),
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
        children: vec![],
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
        children: vec![],
        name: name.to_string(),
        path: path.to_string(),
        attributes: vec![],
        source_code: Some("fn main() {}".to_string()),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_function(id: usize) -> Function {
    create_mock_function_with_parent(
        id,
        FunctionParentType::Contract(Rc::new(create_mock_contract(1))),
    )
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_inner_item(id: usize) -> Function {
    create_mock_function_with_parent(
        id,
        FunctionParentType::Contract(Rc::new(create_mock_contract(1))),
    )
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_parent(id: usize, parent: FunctionParentType) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        parent,
        children: RefCell::new(vec![]),
        parameters: vec![],
        returns: TypeNode::Empty,
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_parameters(
    id: usize,
    parameters: Vec<Rc<FnParameter>>,
) -> Function {
    Function {
        id,
        location: create_mock_location(),
        name: "test_function".to_string(),
        visibility: Visibility::Public,
        parent: FunctionParentType::Contract(Rc::new(create_mock_contract(1))),
        children: RefCell::new(vec![]),
        parameters,
        returns: TypeNode::Empty,
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract(id: usize) -> Contract {
    create_mock_contract_with_parent(id, ContractParentType::File(Rc::new(create_mock_file())))
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract_with_parent(id: usize, parent: ContractParentType) -> Contract {
    Contract {
        id,
        name: "TestContract".to_string(),
        location: create_mock_location(),
        parent,
        children: RefCell::new(vec![]),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract_with_inner_struct(
    id: usize,
    name: String,
    location: Location,
) -> Contract {
    Contract {
        id,
        name,
        location,
        parent: ContractParentType::File(Rc::new(create_mock_file())),
        children: RefCell::new(vec![]),
    }
}
