#![warn(clippy::pedantic)]
use crate::ast::contract::Contract;
use crate::ast::node_type::NodeType;
use crate::errors::SDKErr;
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

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

    pub fn build_api(rc: RefCell<Codebase<OpenState>>) -> RefCell<Codebase<SealedState>> {
        let mut mrc = rc.into_inner();
        let mut new_items = Vec::new();
        let mut new_items_map: HashMap<String, Vec<usize>> = HashMap::new();
        for (fname, ast) in &mrc.fname_ast_map {
            for item in &ast.items {
                match item {
                    syn::Item::Struct(item_struct) => {
                        if item_struct
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contract"))
                        {
                            let contract = Rc::new(Contract {
                                id: mrc.items.len() + new_items.len(),
                                inner_struct: Rc::new(item_struct.clone()),
                                parent: Rc::new(NodeType::File(ast.clone())),
                                children: Vec::new(),
                            });
                            new_items.push(NodeType::Contract(contract.clone()));
                            new_items_map
                                .entry(fname.to_string())
                                .or_default()
                                .push(contract.id);
                        }
                    }
                    syn::Item::Fn(_) => (),
                    _ => {}
                }
            }
        }
        mrc.items.extend(new_items.into_iter().map(Rc::new));
        mrc.fname_items_map.extend(new_items_map);
        RefCell::new(Codebase {
            fname_ast_map: mrc.fname_ast_map.clone(),
            items: mrc.items.clone(),
            fname_items_map: mrc.fname_items_map.clone(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_parse_file() {
        let (file_name, mut content) = get_file_content();
        let res = parse_file(&file_name, &mut content);
        assert!(res.is_ok());
    }

    #[test]
    fn test_codebase_parse_and_add_file() {
        let (file_name, mut content) = get_file_content();
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
        let (file_name, mut content) = get_file_content();
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        assert_eq!(codebase.borrow().items.len(), 1);

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
        assert_eq!(codebase.borrow().items.len(), 2);
    }

    fn get_tests_dir_path() -> PathBuf {
        let current_dir = std::env::current_dir().unwrap();
        current_dir.join("tests")
    }

    fn get_file_content() -> (String, String) {
        let current_dir = get_tests_dir_path();
        let file = current_dir.join("account.rs").to_str().unwrap().to_string();
        let content = std::fs::read_to_string(&file).unwrap();
        (file, content)
    }
}
