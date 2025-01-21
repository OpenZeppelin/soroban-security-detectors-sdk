use std::cell::RefCell;

use soroban_security_rules_macro_lib::node_location;

use super::{
    node::{Location, TLocation},
    node_type::CustomTypeChildType,
};

#[derive(serde::Serialize, serde::Deserialize)]
pub enum Type {
    T(String), //TODO: Implement this
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct CustomType {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub children: RefCell<Vec<CustomTypeChildType>>,
}
