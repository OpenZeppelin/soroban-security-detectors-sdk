#![warn(clippy::pedantic)]
use crate::{ast_enum, ast_node};

use super::{
    definition::Definition,
    expression::Expression,
    node::{Location, TLocation, Visibility},
    pattern::Pattern,
};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

ast_enum! {
    pub enum Directive {
        Use(Rc<Use>),
    }
}

ast_node! {
    pub struct Use {
        pub visibility: Visibility,
        pub path: String,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directive_id() {
        let use_directive = Use {
            id: 42,
            location: Location::default(),
            visibility: Visibility::Public,
            path: "some/path".to_string(),
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
        };
        assert_eq!(use_directive.id, 42);
        assert_eq!(use_directive.location, location);
        assert_eq!(use_directive.visibility, Visibility::Public);
        assert_eq!(use_directive.path, "some/path".to_string());
    }
}
