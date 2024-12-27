use std::{cell::RefCell, rc::Rc};

use syn::{parse_quote, ItemFn};

use crate::{
    contract::Contract,
    file::File,
    function::Function,
    node::Location,
    node_type::{ContractParentType, FunctionParentType},
};

pub(crate) struct MockLocation {
    source: Option<String>,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl Location for MockLocation {
    fn source_code(&self) -> Option<String> {
        self.source.clone()
    }

    fn start_line(&self) -> usize {
        self.start_line
    }

    fn start_col(&self) -> usize {
        self.start_col
    }

    fn end_line(&self) -> usize {
        self.end_line
    }

    fn end_col(&self) -> usize {
        self.end_col
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_location() -> MockLocation {
    MockLocation {
        source: Some("fn main() {}".to_string()),
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
pub(crate) fn create_mock_file_with_inner_struct(inner_struct: syn::File) -> File {
    File {
        id: 1,
        inner_struct: Rc::new(inner_struct),
        children: vec![],
        name: "test_mod.rs".to_string(),
        path: "./test_mod.rs".to_string(),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_file_with_name_path(name: &str, path: &str) -> File {
    let item_file: syn::File = parse_quote! {
        // Mock file
    };
    File {
        id: 1,
        inner_struct: Rc::new(item_file),
        children: vec![],
        name: name.to_string(),
        path: path.to_string(),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_function(id: usize) -> Function {
    create_mock_function_with_parent(
        id,
        parse_quote! { fn test_function() {} },
        Rc::new(FunctionParentType::Contract(Rc::new(create_mock_contract(
            1,
        )))),
    )
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_inner_item(id: usize, item_fn: ItemFn) -> Function {
    create_mock_function_with_parent(
        id,
        item_fn,
        Rc::new(FunctionParentType::Contract(Rc::new(create_mock_contract(
            1,
        )))),
    )
}

#[allow(dead_code)]
pub(crate) fn create_mock_function_with_parent(
    id: usize,
    item_fn: ItemFn,
    parent: Rc<FunctionParentType>,
) -> Function {
    Function {
        id,
        inner_struct: Rc::new(item_fn),
        parent,
        children: vec![],
        parameters: vec![],
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract(id: usize) -> Contract {
    create_mock_contract_with_parent(
        id,
        Rc::new(ContractParentType::File(Rc::new(create_mock_file()))),
    )
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract_with_parent(
    id: usize,
    parent: Rc<ContractParentType>,
) -> Contract {
    Contract {
        id,
        inner_struct: Rc::new(parse_quote! { struct MyContract {} }),
        parent,
        children: RefCell::new(vec![]),
    }
}

#[allow(dead_code)]
pub(crate) fn create_mock_contract_with_inner_struct(
    id: usize,
    inner_struct: syn::ItemStruct,
) -> Contract {
    Contract {
        id,
        inner_struct: Rc::new(inner_struct),
        parent: Rc::new(ContractParentType::File(Rc::new(create_mock_file()))),
        children: RefCell::new(vec![]),
    }
}
