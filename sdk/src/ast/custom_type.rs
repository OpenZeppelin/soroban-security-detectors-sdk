use super::node::{Location, TLocation, Visibility};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Type {
    Alias(Rc<TypeAlias>),
    T(String), //TODO: Implement this
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

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct T {
    //TODO use it
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub ty: String,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeAlias {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub ty: Box<Type>,
}
