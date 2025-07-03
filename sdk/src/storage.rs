//! In-memory storage of AST nodes.
//!
//! Maintains all nodes with parent-child routes and provides lookup APIs
//! by node ID for source location and file resolution.
use crate::{file::File, node_type::NodeKind};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, rc::Rc};

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

    #[must_use = "Use this method to find a Node's root File Node"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
        let mut node_id = id;
        loop {
            if let Some(NodeKind::File(f)) = self.find_node(node_id) {
                return Some(f);
            }
            let parent = self.find_parent_node(node_id)?;
            node_id = parent.id;
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
        let parent_opt = if parent == 0 { None } else { Some(parent) };
        self.add_storage_node(
            NodeRoute {
                id,
                parent: parent_opt,
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
        // for all node_routes fill children
        let routes = self.node_routes.clone();
        for node in routes {
            if let Some(parent) = node.parent {
                if let Some(parent_node) = self.node_routes.iter_mut().find(|n| n.id == parent) {
                    parent_node.children.push(node.id);
                }
            }
        }
    }

    pub(crate) fn add_route_child(&mut self, parent_id: u32, child_id: u32) {
        if let Some(parent_route) = self.node_routes.iter_mut().find(|r| r.id == parent_id) {
            parent_route.children.push(child_id);
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
