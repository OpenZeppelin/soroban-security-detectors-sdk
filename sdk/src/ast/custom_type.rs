use crate::{ast_enum, ast_nodes};

use super::node::{Location, TLocation, Visibility};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

ast_enum! {
    pub enum Type {
        T(String), //TODO: Implement this
        Alias(Rc<TypeAlias>),
    }
}

impl Type {
    #[must_use = "Use this method to get the id of the type"]
    pub fn id(&self) -> u128 {
        match self {
            Type::Alias(alias) => alias.id,
            Type::T(_) => 0,
        }
    }

    #[must_use = "Use this method to get the location of the type"]
    pub fn location(&self) -> Location {
        match self {
            Type::Alias(alias) => alias.location.clone(),
            Type::T(_) => Location::default(),
        }
    }
}

ast_nodes! {
    pub struct T {
        //TODO use it
        pub name: String,
        pub visibility: Visibility,
        pub ty: String,
    }

    pub struct TypeAlias {
        pub name: String,
        pub visibility: Visibility,
        pub ty: Box<Type>,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_alias_id() {
        let alias = TypeAlias {
            id: 42,
            location: Location::default(),
            name: "alias".to_string(),
            visibility: Visibility::Public,
            ty: Box::new(Type::T("type".to_string())),
        };
        let type_alias = Type::Alias(Rc::new(alias));
        assert_eq!(type_alias.id(), 42);
    }

    #[test]
    fn test_type_t_id() {
        let type_t = Type::T("type".to_string());
        assert_eq!(type_t.id(), 0);
    }

    #[test]
    fn test_type_alias_location() {
        let location = Location::default();
        let alias = TypeAlias {
            id: 42,
            location: location.clone(),
            name: "alias".to_string(),
            visibility: Visibility::Public,
            ty: Box::new(Type::T("type".to_string())),
        };
        let type_alias = Type::Alias(Rc::new(alias));
        assert_eq!(type_alias.location(), location);
    }

    #[test]
    fn test_type_t_location() {
        let type_t = Type::T("type".to_string());
        assert_eq!(type_t.location(), Location::default());
    }

    #[test]
    fn test_t_id() {
        let t = T {
            id: 99,
            location: Location::default(),
            name: "t".to_string(),
            visibility: Visibility::Public,
            ty: "type".to_string(),
        };
        assert_eq!(t.id, 99);
    }

    #[test]
    fn test_t_location() {
        let location = Location::default();
        let t = T {
            id: 99,
            location: location.clone(),
            name: "t".to_string(),
            visibility: Visibility::Public,
            ty: "type".to_string(),
        };
        assert_eq!(t.location, location);
    }

    #[test]
    fn test_t_name() {
        let t = T {
            id: 99,
            location: Location::default(),
            name: "t".to_string(),
            visibility: Visibility::Public,
            ty: "type".to_string(),
        };
        assert_eq!(t.name, "t");
    }

    #[test]
    fn test_t_visibility() {
        let t = T {
            id: 99,
            location: Location::default(),
            name: "t".to_string(),
            visibility: Visibility::Public,
            ty: "type".to_string(),
        };
        assert_eq!(t.visibility, Visibility::Public);
    }

    #[test]
    fn test_t_ty() {
        let t = T {
            id: 99,
            location: Location::default(),
            name: "t".to_string(),
            visibility: Visibility::Public,
            ty: "type".to_string(),
        };
        assert_eq!(t.ty, "type");
    }
}
