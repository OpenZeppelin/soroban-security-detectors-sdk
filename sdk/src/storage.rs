#![warn(clippy::pedantic)]

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::node_type::{get_node_id, NodeKind};

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodesStorage {
    storage_nodes: Vec<StorageNode>,
    pub nodes: Vec<NodeKind>,
    fname_items_map: HashMap<String, Vec<u32>>,
}

impl NodesStorage {
    pub fn find_node(&self, id: u32) -> Option<NodeKind> {
        self.nodes.iter().find(|n| get_node_id(n) == id).cloned()
    }

    fn find_sorage_node(&self, id: u32) -> Option<&StorageNode> {
        self.storage_nodes.iter().find(|n| n.id == id)
    }

    pub fn add_node(&mut self, item: NodeKind, parent: u32, file_name: String) {
        let id = get_node_id(&item);
        self.nodes.push(item.clone());
        self.add_storage_node(
            StorageNode {
                id,
                parent: Some(parent),
                children: vec![],
            },
            parent,
        );
        self.fname_items_map
            .entry(file_name)
            .or_insert_with(Vec::new)
            .push(id);
    }

    fn add_storage_node(&mut self, node: StorageNode, parent: u32) {
        if let Some(parent_node) = self.storage_nodes.iter_mut().find(|n| n.id == parent) {
            parent_node.children.push(node.id);
        }
        self.storage_nodes.push(node);
    }

    fn children_of(&self, id: u32) -> Option<Vec<NodeKind>> {
        if let Some(node) = self.find_sorage_node(id) {
            let mut children = Vec::new();
            for child in &node.children {
                children.push(self.find_node(*child).unwrap());
            }
        }
        None
    }

    fn parent_of(&self, id: u32) -> Option<NodeKind> {
        if let Some(node) = self.find_sorage_node(id) {
            if let Some(parent) = node.parent {
                return self.find_node(parent);
            }
        }
        None
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct StorageNode {
    id: u32,
    parent: Option<u32>,
    children: Vec<u32>,
}
