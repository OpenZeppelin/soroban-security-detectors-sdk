#![warn(clippy::pedantic)]
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::file::File;
use crate::node_type::ContractType;
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
    pub(crate) _state: PhantomData<S>,
}

impl Codebase<SealedState> {
    pub fn files(&self) -> impl Iterator<Item = Rc<File>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::File(file) = item {
                res.push(file.clone());
            }
        }
        res.into_iter()
    }

    pub fn contracts(&self) -> impl Iterator<Item = Rc<Struct>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Contract(ContractType::Contract(contract)) = item {
                res.push(contract.clone());
            }
        }
        res.into_iter()
    }

    #[must_use = "Use this method to get `Node` source code"]
    pub fn get_node_source_code(&self, node_id: u128) -> Option<String> {
        self.storage.get_node_source_code(node_id)
    }
}
