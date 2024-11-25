#![warn(clippy::pedantic)]
use crate::errors::SDKErr;
use macro_lib::node_location;
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use syn::{File, ItemStruct};

fn parse_file(file_name: &str, content: &mut str) -> Result<syn::File, SDKErr> {
    if let Ok(ast) = syn::parse_file(content) {
        Ok(ast)
    } else {
        Err(SDKErr::AstParseError(file_name.to_string()))
    }
}

#[allow(dead_code)]
pub trait CodebaseOpen {}
#[allow(dead_code)]
pub trait CodebaseSealed {}

pub struct CodebaseOpenState;
impl CodebaseOpen for CodebaseOpenState {}

pub struct CodebaseSealedState;
impl CodebaseSealed for CodebaseSealedState {}

#[derive(Clone)]
pub struct Codebase<S> {
    fname_ast_map: HashMap<String, Rc<syn::File>>,
    items: Vec<Rc<dyn Node>>,
    fname_items_map: HashMap<String, Vec<usize>>,
    _state: PhantomData<S>,
}

impl Codebase<CodebaseOpenState> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Codebase {
            fname_ast_map: HashMap::new(),
            items: Vec::new(),
            fname_items_map: HashMap::new(),
            _state: PhantomData,
        }
    }

    pub fn parse_and_add_file(&mut self, file_name: &str, content: &mut str) -> Result<(), SDKErr> {
        if self.fname_ast_map.contains_key(file_name) {
            return Err(SDKErr::AddDuplicateItemError(file_name.to_string()));
        }
        let file = parse_file(file_name, content)?;
        self.fname_ast_map
            .insert(file_name.to_string(), Rc::new(file));
        Ok(())
    }

    pub fn build_api(
        rc: RefCell<Codebase<CodebaseOpenState>>,
    ) -> RefCell<Codebase<CodebaseSealedState>> {
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
                                parent: ast.clone(),
                                children: Vec::new(),
                            });
                            new_items.push(contract.clone() as Rc<dyn Node>);
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
        mrc.items
            .extend(new_items.into_iter().map(|item| item as Rc<dyn Node>));
        mrc.fname_items_map.extend(new_items_map);
        RefCell::new(Codebase {
            fname_ast_map: mrc.fname_ast_map.clone(),
            items: mrc.items.clone(),
            fname_items_map: mrc.fname_items_map.clone(),
            _state: PhantomData,
        })
    }

    pub fn get_item_by_id(&self, id: usize) -> Option<Rc<dyn Node>> {
        self.items.get(id).cloned()
    }
}

pub enum NodeType {
    File,
    Contract,
    Function,
    Struct,
    Enum,
}

#[allow(dead_code)]
pub trait NodeLocation {
    fn source_code(&self) -> Option<String>;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
}

pub trait Node {
    fn parent(&self) -> Option<Rc<dyn Node>>;
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'a>;
    fn node_type(&self) -> NodeType;
}

impl Node for File {
    fn parent(&self) -> Option<Rc<dyn Node>> {
        None
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'a> {
        Box::new(Vec::new().into_iter())
    }

    fn node_type(&self) -> NodeType {
        NodeType::File
    }
}

#[node_location(inner = "inner_struct")]
pub struct Contract {
    pub id: usize,
    inner_struct: Rc<ItemStruct>,
    parent: Rc<File>,
    children: Vec<Rc<dyn Node>>,
}

impl Node for Contract {
    fn parent(&self) -> Option<Rc<dyn Node>> {
        Some(self.parent.clone())
    }

    fn children<'n>(&'n self) -> Box<dyn Iterator<Item = Rc<dyn Node>> + 'n> {
        Box::new(self.children.iter().cloned())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Contract
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
