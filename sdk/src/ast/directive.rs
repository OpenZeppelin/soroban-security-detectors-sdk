#![warn(clippy::pedantic)]
use super::{
    definition::Definition,
    expression::Expression,
    node::{Location, TLocation, Visibility},
    pattern::Pattern,
};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Directive {
    Use(Rc<Use>),
}

impl Directive {
    #[must_use = "Use this method to get the id of the directive"]
    pub fn id(&self) -> u128 {
        match self {
            Directive::Use(use_) => use_.id,
        }
    }

    #[must_use = "Use this method to get the location of the directive"]
    pub fn location(&self) -> Location {
        match self {
            Directive::Use(use_) => use_.location.clone(),
        }
    }
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Use {
    pub id: u128,
    pub location: Location,
    pub visibility: Visibility,
    pub path: String,
}
