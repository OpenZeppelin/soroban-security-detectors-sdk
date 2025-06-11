use crate::{ast_enum, ast_node, ast_node_impl};

use super::{
    definition::Definition,
    expression::Expression,
    node::{Location, Node, Visibility},
    node_type::NodeKind,
    pattern::Pattern,
};
use std::{collections::BTreeMap, rc::Rc};

ast_enum! {
    pub enum Directive {
        Use(Rc<Use>),
    }
}

ast_node! {
    pub struct Use {
        pub visibility: Visibility,
        pub path: String,
        pub imported_types: Vec<String>,
        pub target: std::cell::RefCell<BTreeMap<String, Option<u32>>>,
    }
}

ast_node_impl! {
    impl Node for Use {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use soroban_sdk::crypto::Hash;

    #[test]
    fn test_directive_id() {
        let use_directive = Use {
            id: 42,
            location: Location::default(),
            visibility: Visibility::Public,
            path: "some/path".to_string(),
            imported_types: vec![],
            target: std::cell::RefCell::new(BTreeMap::new()),
        };
        let directive = Directive::Use(Rc::new(use_directive));
        assert_eq!(directive.id(), 42);
    }

    #[test]
    fn test_directive_location() {
        let location = Location::default();
        let use_directive = Use {
            id: 42,
            location: location.clone(),
            visibility: Visibility::Public,
            path: "some/path".to_string(),
            imported_types: vec![],
            target: std::cell::RefCell::new(BTreeMap::new()),
        };
        let directive = Directive::Use(Rc::new(use_directive));
        assert_eq!(directive.location(), location);
    }

    #[test]
    fn test_use_struct() {
        let location = Location::default();
        let use_directive = Use {
            id: 42,
            location: location.clone(),
            visibility: Visibility::Public,
            path: "some/path".to_string(),
            imported_types: vec![],
            target: std::cell::RefCell::new(BTreeMap::new()),
        };
        assert_eq!(use_directive.id, 42);
        assert_eq!(use_directive.location, location);
        assert_eq!(use_directive.visibility, Visibility::Public);
        assert_eq!(use_directive.path, "some/path".to_string());
    }
}
