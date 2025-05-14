use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::contract::Contract;
use crate::definition::Definition;
use crate::directive::Directive;
use crate::expression::Expression;
use crate::file::File;
use crate::node_type::{ContractType, FileChildType, TypeNode};
use crate::statement::Statement;
use crate::{ast::node_type::NodeKind, contract::Struct, custom_type::Type};
use crate::{symbol_table, NodesStorage, SymbolTable};
use serde::{Deserialize, Serialize};

#[allow(dead_code)]
trait CodebaseOpen {}
#[allow(dead_code)]
trait CodebaseSealed {}

pub struct OpenState;
impl CodebaseOpen for OpenState {}

pub struct SealedState;
impl CodebaseSealed for SealedState {}

#[derive(Serialize, Deserialize)]
pub struct Codebase<S> {
    pub(crate) storage: NodesStorage,
    pub(crate) files: Vec<Rc<File>>,
    #[serde(skip)]
    pub(crate) syn_files: HashMap<String, syn::File>,
    #[serde(skip)]
    pub(crate) contract_cache: RefCell<HashMap<u32, Rc<Contract>>>,
    #[serde(skip)]
    pub(crate) symbol_table: Option<SymbolTable>,
    _state: PhantomData<S>,
}

impl Default for Codebase<OpenState> {
    fn default() -> Self {
        Self {
            storage: NodesStorage::default(),
            files: Vec::new(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table: None,
            _state: PhantomData,
        }
    }
}

impl Codebase<SealedState> {
    #[must_use]
    pub fn new(storage: NodesStorage, symbol_table: Option<SymbolTable>) -> Self {
        Self {
            storage,
            files: Vec::new(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table,
            _state: PhantomData,
        }
    }

    pub fn files(&self) -> impl Iterator<Item = Rc<File>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::File(file) = item {
                res.push(file.clone());
            }
        }
        res.into_iter()
    }

    pub fn contracts(&self) -> impl Iterator<Item = Rc<Contract>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Statement(Statement::Definition(Definition::Struct(struct_node))) =
                item
            {
                if struct_node.is_contract {
                    res.push(self.construct_contract_from_struct(struct_node));
                }
            }
        }
        res.into_iter()
    }

    #[must_use = "Use this method to get `Node` source code"]
    pub fn get_node_source_code(&self, node_id: u32) -> Option<String> {
        self.storage.get_node_source_code(node_id)
    }

    fn construct_contract_from_struct(&self, struct_node: &Struct) -> Rc<Contract> {
        if let Some(cached) = self.contract_cache.borrow().get(&struct_node.id) {
            return cached.clone();
        }
        let mut methods = Vec::new();
        let mut functions = Vec::new();
        let mut type_aliases = Vec::new();
        let mut constants = Vec::new();
        let mut macros = Vec::new();
        let mut plane_defs = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Statement(Statement::Definition(Definition::Implementation(
                impl_node,
            ))) = item
            {
                if let Some(for_type) = &impl_node.for_type {
                    let name = match for_type {
                        Type::Typedef(t) => t.clone(),
                        Type::Alias(type_alias) => type_alias.name.clone(),
                        Type::Struct(tstruct) => tstruct.name.clone(),
                    };
                    if name != struct_node.name {
                        continue;
                    }
                } else {
                    continue;
                }
                impl_node.constants.iter().for_each(|constant| {
                    constants.push(constant.clone());
                });
                impl_node.type_aliases.iter().for_each(|type_alias| {
                    type_aliases.push(type_alias.clone());
                });
                impl_node.macros.iter().for_each(|macro_| {
                    macros.push(macro_.clone());
                });
                impl_node.plane_defs.iter().for_each(|plane_def| {
                    plane_defs.push(plane_def.clone());
                });
                for func in &impl_node.functions {
                    if func.is_method() {
                        methods.push(func.clone());
                    } else {
                        functions.push(func.clone());
                    }
                }
            }
        }
        let contract = Rc::new(Contract {
            id: struct_node.id,
            name: struct_node.name.clone(),
            location: struct_node.location.clone(),
            fields: struct_node.fields.clone(),
            methods: RefCell::new(methods),
            functions: RefCell::new(functions),
            type_aliases: RefCell::new(type_aliases),
            constants: RefCell::new(constants),
            macros: RefCell::new(macros),
            plane_defs: RefCell::new(plane_defs),
        });
        self.contract_cache
            .borrow_mut()
            .insert(struct_node.id, contract.clone());
        contract
    }

    pub fn get_symbol_type(&self, node_id: u32) -> Option<TypeNode> {
        if let Some(node) = self.storage.find_node(node_id) {
            if let Some(symbol_table) = &self.symbol_table {
                match node {
                    NodeKind::Statement(Statement::Expression(expr)) => {
                        symbol_table.infer_expr_type(&expr)
                    }
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<T> Codebase<T> {
    #[must_use = "Use this function to get a Node's source file"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
        self.storage.find_node_file(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build_codebase;

    #[test]
    fn test_find_node_file() {
        let src = "#![no_std]
use soroban_sdk::contract;

#[contract]
struct Contract1;";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let file = codebase.find_node_file(contract.id).unwrap();
        assert_eq!(file.path, "test.rs");
    }

    #[test]
    fn test_contract_methods() {
        let src = "#![no_std]
use soroban_sdk::contract;

#[contract]
struct Contract1;

impl Contract1 {
    fn get_field(&self) -> Self {
        self.field
    }
}";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.methods.borrow().len(), 1);
    }

    #[test]
    fn test_expression_types() {
        let src = "#![no_std]
    #[contract]
    struct Contract1;
    impl Contract1 {
        fn get_field(&self) -> Self {
            self.field
        }
        fn get_value(&self, a: i32) -> u32 {
            42
        }
        fn get_string(&self, s: &str) -> String {
            String::from(\"Hello\")
        }
        fn get_array(&self, v: [i32; 9]) -> Vec<u32> {
            vec![1, 2, 3]
        }
        fn get_tuple(&self, t: (u32, String)) -> (u32, String) {
            (42, String::from(\"Hello\"))
        }
        fn get_map(&self) -> HashMap<String, u32> {
            let mut map = HashMap::new();
            map.insert(String::from(\"key\"), 42);
            map
        }
        fn get_option(&self) -> Option<u32> {
            Some(42)
        }
        fn get_result(&self) -> Result<u32, String> {
            Ok(42)
        }
        fn get_reference(&self) -> &Self {
            self
        }
        fn get_pointer(&self) -> *const Self {
            self as *const Self
        }
        fn get_closure(&self) -> impl Fn(u32) -> u32 {
            |x| x + 1
        }
        fn get_trait(&self) -> &dyn std::fmt::Debug {
            self
        }
        fn get_function(&self) -> fn(u32) -> u32 {
            |x| x + 1
        }
        fn get_macro(&self) -> String {
            format!(\"Hello, {}!\", \"world\")
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.methods.borrow().len(), 14);
        let methods = contract.methods.borrow();
        assert_eq!(methods[0].name, "get_field");
        assert_eq!(methods[1].name, "get_value");
        assert_eq!(methods[2].name, "get_string");
        assert_eq!(methods[3].name, "get_array");
        assert_eq!(methods[4].name, "get_tuple");
        assert_eq!(methods[5].name, "get_map");
        assert_eq!(methods[6].name, "get_option");
        assert_eq!(methods[7].name, "get_result");
        assert_eq!(methods[8].name, "get_reference");
        assert_eq!(methods[9].name, "get_pointer");
        assert_eq!(methods[10].name, "get_closure");
        assert_eq!(methods[11].name, "get_trait");
        assert_eq!(methods[12].name, "get_function");
        assert_eq!(methods[13].name, "get_macro");
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        println!("{:?}", codebase.symbol_table);
        let t = codebase.symbol_table.unwrap().lookdown_symbol(&param.name);
        assert!(t.is_some());
        let t = t.unwrap();
    }
}
