use std::{collections::HashMap, rc::Rc};
use serde::{Deserialize, Serialize};
use crate::{file::File, node_type::NodeKind};


#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct NodesStorage {
    node_routes: Vec<NodeRoute>,
    pub nodes: Vec<NodeKind>,
    file_content_map: HashMap<u32, String>,
}

impl NodesStorage {
    #[must_use = "Use this method to find a node in the storage by its id"]
    pub fn find_node(&self, id: u32) -> Option<NodeKind> {
        self.nodes.iter().find(|n| n.id() == id).cloned()
    }

    /// # Panics
    ///
    /// This function will panic if the node with the given id is not found.
    #[must_use = "Use this method to find a Node's root File Node"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
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
    pub fn find_parent_node(&self, id: u32) -> Option<NodeRoute> {
        self.node_routes
            .iter()
            .find(|n| n.children.contains(&id))
            .cloned()
    }

    #[must_use = "Use this method to get a Node's source code"]
    pub fn get_node_source_code(&self, id: u32) -> Option<String> {
        self.find_node(id)
            .map(|node| node.location().source.clone())
    }

    pub(crate) fn add_node(&mut self, item: NodeKind, parent: u32) {
        let id = item.id();
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

    fn add_storage_node(&mut self, node: NodeRoute, parent: u32) {
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
    pub id: u32,
    parent: Option<u32>,
    children: Vec<u32>,
}

impl NodeRoute {
    #[must_use]
    pub fn is_root(&self) -> bool {
        self.parent.is_none() || self.parent == Some(0)
    }
}
