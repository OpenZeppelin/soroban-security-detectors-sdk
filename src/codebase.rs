#![warn(clippy::pedantic)]
use syn::ItemFn;

use crate::ast::node_type::NodeType;
use crate::errors::SDKErr;
use crate::function::Function;
use crate::node_type::FunctionParentType;
use crate::{ast::contract::Contract, node_type::ContractParentType};
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use uuid::Uuid;

fn parse_file(file_name: &str, content: &mut str) -> Result<syn::File, SDKErr> {
    if let Ok(ast) = syn::parse_file(content) {
        Ok(ast)
    } else {
        Err(SDKErr::AstParseError(file_name.to_string()))
    }
}

#[allow(dead_code)]
trait CodebaseOpen {}
#[allow(dead_code)]
trait CodebaseSealed {}

pub struct OpenState;
impl CodebaseOpen for OpenState {}

pub struct SealedState;
impl CodebaseSealed for SealedState {}

#[derive(Clone)]
pub struct Codebase<S> {
    fname_ast_map: HashMap<String, Rc<syn::File>>,
    items: Vec<Rc<NodeType>>,
    fname_items_map: HashMap<String, Vec<usize>>,
    _state: PhantomData<S>,
}

impl Codebase<OpenState> {
    #[allow(clippy::new_without_default)]
    #[must_use]
    pub fn new() -> Self {
        Codebase {
            fname_ast_map: HashMap::new(),
            items: Vec::new(),
            fname_items_map: HashMap::new(),
            _state: PhantomData,
        }
    }

    /// Parse the file and add it to the codebase.
    /// # Errors
    /// - `SDKErr::AddDuplicateItemError` If the file is already added.
    pub fn parse_and_add_file(&mut self, file_name: &str, content: &mut str) -> Result<(), SDKErr> {
        if self.fname_ast_map.contains_key(file_name) {
            return Err(SDKErr::AddDuplicateItemError(file_name.to_string()));
        }
        let file = parse_file(file_name, content)?;
        self.fname_ast_map
            .insert(file_name.to_string(), Rc::new(file));
        Ok(())
    }

    /// Builds the API from the codebase.
    pub fn build_api(rc: RefCell<Codebase<OpenState>>) -> RefCell<Codebase<SealedState>> {
        let mut codebase = rc.into_inner();
        let mut new_items_map: HashMap<String, Vec<usize>> = HashMap::new();
        let mut items_to_revisit: HashMap<String, Vec<syn::Item>> = HashMap::new();
        for (fname, ast) in &codebase.fname_ast_map {
            for item in &ast.items {
                match item {
                    syn::Item::Struct(struct_item) => {
                        if struct_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contract"))
                        {
                            let contract = Contract {
                                id: Uuid::new_v4().as_u128() as usize,
                                inner_struct: Rc::new(struct_item.clone()),
                                parent: Rc::new(ContractParentType::File(ast.clone())),
                                children: RefCell::new(Vec::new()),
                            };
                            let rc_contract = Rc::new(contract);
                            codebase
                                .items
                                .push(Rc::new(NodeType::Contract(rc_contract.clone())));
                            new_items_map
                                .entry(fname.clone())
                                .or_default()
                                .push(rc_contract.id);
                        }
                    }
                    syn::Item::Impl(impl_item) => {
                        if impl_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contractimpl"))
                        {
                            if let Some(functions) = handle_item_impl(&codebase, impl_item) {
                                for function in functions {
                                    codebase
                                        .items
                                        .push(Rc::new(NodeType::Function(function.clone())));
                                    new_items_map
                                        .entry(fname.clone())
                                        .or_default()
                                        .push(function.id);
                                }
                            } else {
                                items_to_revisit
                                    .entry(fname.clone())
                                    .or_default()
                                    .push(syn::Item::Impl(impl_item.clone()));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        for (fname, items) in items_to_revisit {
            for item in items {
                if let syn::Item::Impl(impl_item) = item {
                    if let Some(rc_functions) = handle_item_impl(&codebase, &impl_item) {
                        for function in rc_functions {
                            codebase
                                .items
                                .push(Rc::new(NodeType::Function(function.clone())));
                            new_items_map
                                .entry(fname.clone())
                                .or_default()
                                .push(function.id);
                        }
                    }
                }
            }
        }
        codebase.fname_items_map.extend(new_items_map);
        RefCell::new(Codebase {
            fname_ast_map: codebase.fname_ast_map.clone(),
            items: codebase.items.clone(),
            fname_items_map: codebase.fname_items_map.clone(),
            _state: PhantomData,
        })
    }

    #[must_use]
    pub fn get_item_by_id(&self, id: usize) -> Option<Rc<NodeType>> {
        self.items.get(id).cloned()
    }
}

impl Codebase<SealedState> {
    pub fn contracts(&self) -> impl Iterator<Item = Rc<Contract>> {
        let mut res = Vec::new();
        for item in &self.items {
            if let NodeType::Contract(contract) = item.as_ref() {
                res.push(contract.clone());
            }
        }
        res.into_iter()
    }
}

fn handle_item_impl(
    codebase: &Codebase<OpenState>,
    impl_item: &syn::ItemImpl,
) -> Option<Vec<Rc<Function>>> {
    let contract_name = get_impl_type_name(impl_item).unwrap_or_default();
    if contract_name.is_empty() {
        return None;
    }

    let contract = codebase.items.iter().find_map(|item| match item.as_ref() {
        NodeType::Contract(contract) => {
            if contract.name() == contract_name {
                Some(contract.clone())
            } else {
                None
            }
        }
        _ => None,
    });

    let contract = contract?;

    let mut functions = Vec::new();
    for item in &impl_item.items {
        if let syn::ImplItem::Fn(assoc_fn) = item {
            let function = Rc::new(Function {
                id: Uuid::new_v4().as_u128() as usize,
                inner_struct: Rc::new(ItemFn {
                    attrs: assoc_fn.attrs.clone(),
                    vis: assoc_fn.vis.clone(),
                    sig: assoc_fn.sig.clone(),
                    block: Box::new(assoc_fn.block.clone()),
                }),
                parent: Rc::new(FunctionParentType::Contract(contract.clone())),
                children: Vec::new(),
            });
            contract.add_function(function.clone());
            functions.push(function);
        }
    }
    Some(functions)
}

fn get_impl_type_name(item_impl: &syn::ItemImpl) -> Option<String> {
    if let syn::Type::Path(type_path) = &*item_impl.self_ty {
        if let Some(segment) = type_path.path.segments.last() {
            return Some(segment.ident.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_parse_file() {
        let (file_name, mut content) = get_file_content("account.rs");
        let res = parse_file(&file_name, &mut content);
        assert!(res.is_ok());
    }

    #[test]
    fn test_codebase_parse_and_add_file() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        assert_eq!(codebase.borrow().fname_ast_map.len(), 1);
        let new_file_name = "new_file.rs";
        codebase
            .borrow_mut()
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        assert_eq!(codebase.borrow().fname_ast_map.len(), 2);
        assert!(codebase.borrow().fname_ast_map.contains_key(&file_name));
        assert!(codebase.borrow().fname_ast_map.contains_key(new_file_name));
    }

    #[test]
    fn test_parse_contracts_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let contracts = codebase.borrow().contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 1);

        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let new_file_name = "new_file.rs";
        codebase
            .borrow_mut()
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let contracts = codebase.borrow().contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_parse_contract_functions_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase.borrow();
        let contract = binding
            .items
            .iter()
            .filter(|item| {
                if let NodeType::Contract(contract) = item.as_ref() {
                    return contract.name() == "AccountContract";
                }
                false
            })
            .next()
            .unwrap();
        if let NodeType::Contract(contract) = contract.as_ref() {
            let contract_functions = contract.functions().collect::<Vec<_>>();
            assert_eq!(contract_functions.len(), 3);
            assert_eq!(contract_functions[0].name(), "init");
            assert_eq!(contract_functions[1].name(), "add_limit");
            assert_eq!(contract_functions[2].name(), "__check_auth");
        }
    }

    fn get_tests_dir_path() -> PathBuf {
        let current_dir = std::env::current_dir().unwrap();
        current_dir.join("tests")
    }

    fn get_file_content(test_file_name: &str) -> (String, String) {
        let current_dir = get_tests_dir_path();
        let file = current_dir
            .join(test_file_name)
            .to_str()
            .unwrap()
            .to_string();
        let content = std::fs::read_to_string(&file).unwrap();
        (file, content)
    }
}
