#![warn(clippy::pedantic)]

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::node_type::{get_node_id, NodeKind};

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodesStorage {
    node_routes: Vec<NodeRoute>,
    pub nodes: Vec<NodeKind>,
    fname_items_map: HashMap<String, Vec<u128>>,
}

impl NodesStorage {
    #[must_use]
    pub fn find_node(&self, id: u128) -> Option<NodeKind> {
        self.nodes.iter().find(|n| get_node_id(n) == id).cloned()
    }

    pub fn add_node(&mut self, item: NodeKind, parent: u128, file_name: String) {
        let id = get_node_id(&item);
        self.nodes.push(item);
        self.add_storage_node(
            NodeRoute {
                id,
                parent: Some(parent),
                children: vec![],
            },
            parent,
        );
        self.fname_items_map.entry(file_name).or_default().push(id);
    }

    fn add_storage_node(&mut self, node: NodeRoute, parent: u128) {
        if let Some(parent_node) = self.node_routes.iter_mut().find(|n| n.id == parent) {
            parent_node.children.push(node.id);
        }
        self.node_routes.push(node);
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodeRoute {
    id: u128,
    parent: Option<u128>,
    children: Vec<u128>,
}
