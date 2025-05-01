use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::contract::Contract;
use crate::definition::Definition;
use crate::file::File;
use crate::node_type::ContractType;
use crate::statement::Statement;
use crate::NodesStorage;
use crate::{ast::node_type::NodeKind, contract::Struct};
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
    #[serde(skip)]
    pub(crate) fname_ast_map: Option<HashMap<String, syn::File>>,
    _state: PhantomData<S>,
    contracts: Vec<Rc<Struct>>,
}

impl Default for Codebase<OpenState> {
    fn default() -> Self {
        Self {
            storage: NodesStorage::default(),
            fname_ast_map: Some(HashMap::new()),
            _state: PhantomData,
            contracts: Vec::new(),
        }
    }
}

impl Codebase<SealedState> {
    #[must_use]
    pub fn new(storage: NodesStorage) -> Self {
        Self {
            storage,
            fname_ast_map: None,
            _state: PhantomData,
            contracts: Vec::new(),
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

    #[must_use = "Use this method to get `Node` source code"] //TODO test me
    pub fn get_node_source_code(&self, node_id: u128) -> Option<String> {
        self.storage.get_node_source_code(node_id)
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
                if impl_node.for_type.is_none() {
                    continue;
                }
                if impl_node.for_type.as_ref().unwrap() == &struct_node.name {
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
}
