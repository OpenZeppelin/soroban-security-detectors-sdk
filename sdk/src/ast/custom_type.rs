use std::{cell::RefCell, rc::Rc};

use soroban_security_rules_macro_lib::node_location;

use super::{
    node::{Location, TLocation},
    node_type::CustomTypeChildType,
};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Type {
    T(String), //TODO: Implement this
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum CustomType {
    Struct(Rc<StructType>),
    Enum(Rc<EnumType>),
}

impl CustomType {
    #[must_use = "This function creates a new instance of CustomType"]
    pub fn id(&self) -> u128 {
        match self {
            CustomType::Struct(struct_type) => struct_type.id,
            CustomType::Enum(enum_type) => enum_type.id,
        }
    }

    #[must_use = "This function creates a new instance of CustomType"]
    pub fn location(&self) -> Location {
        match self {
            CustomType::Struct(struct_type) => struct_type.location.clone(),
            CustomType::Enum(enum_type) => enum_type.location.clone(),
        }
    }
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct StructType {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub children: RefCell<Vec<CustomTypeChildType>>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct EnumType {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub variants: Vec<String>,
    pub children: RefCell<Vec<CustomTypeChildType>>,
}
