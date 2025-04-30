#![warn(clippy::pedantic)]

use std::{collections::HashMap, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{
    file::File,
    node_type::{get_node_kind_node_id, get_node_location, NodeKind},
};

#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodesStorage {
    node_routes: Vec<NodeRoute>,
    pub nodes: Vec<NodeKind>,
    file_content_map: HashMap<u128, String>,
}

impl NodesStorage {
    #[must_use = "Use this method to find a node in the storage by its id"]
    pub fn find_node(&self, id: u128) -> Option<NodeKind> {
        self.nodes
            .iter()
            .find(|n| get_node_kind_node_id(n) == id)
            .cloned()
    }

    /// # Panics
    ///
    /// This function will panic if the node with the given id is not found.
    #[must_use = "Use this method to find a Node's root File Node"]
    pub fn find_node_file(&self, id: u128) -> Option<Rc<File>> {
        if self.file_content_map.contains_key(&id) {
            let file = self.find_node(id).unwrap();
            match file {
                NodeKind::File(f) => Some(f),
                _ => None,
            }
        } else {
            let mut node_id = id;
            while let Some(parent) = self.find_parent_node(node_id) {
                if parent.parent.unwrap() == 0 {
                    let file = self.find_node(parent.id).unwrap();
                    match file {
                        NodeKind::File(f) => return Some(f),
                        _ => return None,
                    }
                }
                node_id = parent.id;
            }
            None
        }
    }

    #[must_use = "Use this method to find a Node's parent Node"]
    pub fn find_parent_node(&self, id: u128) -> Option<NodeRoute> {
        self.node_routes.iter().find(|n| n.id == id).cloned()
    }

    //TODO test this function and remove source_code attr from nodes
    #[must_use = "Use this method to get a Node's source code"]
    /// # Panics
    ///
    /// This function will panic if the node with the given id is not found.
    pub fn get_node_source_code(&self, id: u128) -> Option<String> {
        if let Some(node) = self.find_node(id) {
            let file = self.find_node_file(id).unwrap();
            let location = get_node_location(&node);
            let source_code = &file.source_code;

            let start_offset = source_code
                .lines()
                .take(location.start_line - 1)
                .map(|line| line.len() + 1) // +1 for newline character
                .sum::<usize>()
                + location.start_column;

            let end_offset = source_code
                .lines()
                .take(location.end_line - 1)
                .map(|line| line.len() + 1)
                .sum::<usize>()
                + location.end_column;

            Some(source_code[start_offset..end_offset].to_string())
        } else {
            None
        }
    }

    pub fn add_node(&mut self, item: NodeKind, parent: u128) {
        let id = get_node_kind_node_id(&item);
        self.nodes.push(item);
        self.add_storage_node(
            NodeRoute {
                id,
                parent: Some(parent),
                children: vec![],
            },
            parent,
        );
    }

    fn add_storage_node(&mut self, node: NodeRoute, parent: u128) {
        if let Some(parent_node) = self.node_routes.iter_mut().find(|n| n.id == parent) {
            parent_node.children.push(node.id);
        }
        self.node_routes.push(node);
    }

    pub fn seal(&mut self) {
        //for all node_routes fill children
        let routes = self.node_routes.clone();
        for node in routes {
            if let Some(parent) = node.parent {
                if let Some(parent_node) = self.node_routes.iter_mut().find(|n| n.id == parent) {
                    parent_node.children.push(node.id);
                }
            }
        }
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodeRoute {
    id: u128,
    parent: Option<u128>,
    children: Vec<u128>,
}
