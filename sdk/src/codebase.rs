use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::contract::Contract;
use crate::definition::Definition;
use crate::file::File;
use crate::function::Function;
use crate::node_type::ContractType;
use crate::statement::Statement;
use crate::NodesStorage;
use crate::{ast::node_type::NodeKind, contract::Struct, custom_type::Type};
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
    _state: PhantomData<S>,
}

impl Default for Codebase<OpenState> {
    fn default() -> Self {
        Self {
            storage: NodesStorage::default(),
            files: Vec::new(),
            syn_files: HashMap::new(),
            _state: PhantomData,
        }
    }
}

impl Codebase<SealedState> {
    #[must_use]
    pub fn new(storage: NodesStorage) -> Self {
        Self {
            storage,
            files: Vec::new(),
            syn_files: HashMap::new(),
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

    pub fn functions(&self) -> impl Iterator<Item = Rc<Function>> + '_ {
        self.list_nodes_cmp(|node| {
            if let NodeKind::Statement(Statement::Definition(Definition::Function(func))) = node {
                Some(func.clone())
            } else {
                None
            }
        })
    }

    //TODO memoize this
    fn construct_contract_from_struct(&self, struct_node: &Struct) -> Rc<Contract> {
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
                        Type::Typename(t) => t.clone(),
                        Type::Alias(type_alias) => type_alias.ty.clone(),
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
        Rc::new(Contract {
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
        })
    }

    #[must_use]
    pub fn get_parent_container(&self, id: u32) -> Option<NodeKind> {
        let mut current_id = id;
        while let Some(route) = self.storage.find_parent_node(current_id) {
            current_id = route.id;
            if let Some(node) = self.storage.find_node(current_id) {
                if let NodeKind::Statement(Statement::Definition(_)) = node {
                    return self.storage.find_node(node.id());
                }
            }
        }
        None
    }

    pub fn get_children_cmp<F>(&self, id: u32, comparator: F) -> Vec<NodeKind>
    where
        F: Fn(&NodeKind) -> bool,
    {
        let mut result = Vec::new();
        let mut stack: Vec<NodeKind> = Vec::new();

        if let Some(root_node) = self.storage.find_node(id) {
            stack.push(root_node.clone());
        }

        while let Some(current_node) = stack.pop() {
            if comparator(&current_node) {
                result.push(current_node.clone());
            }
            stack.extend(current_node.children());
        }

        result
    }

    fn list_nodes_cmp<'a, T, F>(&'a self, cast: F) -> impl Iterator<Item = T> + 'a
    where
        F: Fn(&NodeKind) -> Option<T> + 'a,
        T: Clone + 'static,
    {
        self.storage.nodes.iter().filter_map(cast)
    }
}

impl<T> Codebase<T> {
    #[must_use = "Use this function to get a Node's source file"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
        if let Some(file) = self.files.iter().find(|file| file.id == id) {
            Some(file.clone())
        } else {
            let mut node_id = id;
            while let Some(parent) = self.storage.find_parent_node(node_id) {
                if parent.is_root() {
                    if let Some(file) = self.storage.find_node(parent.id) {
                        match file {
                            NodeKind::File(f) => return Some(f.clone()),
                            _ => return None,
                        }
                    }
                }
                node_id = parent.id;
            }
            None
        }
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
}
