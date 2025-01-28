#![warn(clippy::pedantic)]
use super::{
    contract::Struct,
    custom_type::Type,
    expression::Expression,
    function::Function,
    node::{Location, TLocation, Visibility},
    node_type::ContractType,
};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Definition {
    Const(Rc<Const>),
    ExternCrate(Rc<ExternCrate>),
    Enum(Rc<Enum>),
    Contract(ContractType),
    Struct(Rc<Struct>),
    Function(Rc<Function>),
}

impl Definition {
    #[must_use = "Use this method to get the id of the definition"]
    pub fn id(&self) -> u128 {
        match self {
            Definition::Const(const_) => const_.id,
            Definition::ExternCrate(extern_crate) => extern_crate.id,
            Definition::Enum(enum_) => enum_.id,
            Definition::Function(function) => function.id,
            Definition::Contract(contract) => match contract {
                ContractType::Contract(contract) => contract.id,
                ContractType::Enum(enum_) => enum_.id,
                ContractType::Struct(struct_) => struct_.id,
            },
            Definition::Struct(struct_) => struct_.id,
        }
    }

    #[must_use = "Use this method to get the location of the definition"]
    pub fn location(&self) -> Location {
        match self {
            Definition::Const(const_) => const_.location(),
            Definition::ExternCrate(extern_crate) => extern_crate.location(),
            Definition::Enum(enum_) => enum_.location(),
            Definition::Function(function) => function.location(),
            Definition::Contract(contract) => match contract {
                ContractType::Contract(contract) => contract.location(),
                ContractType::Enum(enum_) => enum_.location(),
                ContractType::Struct(struct_) => struct_.location(),
            },
            Definition::Struct(struct_) => struct_.location(),
        }
    }
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Const {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub type_: Type,
    pub value: Expression,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Enum {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub variants: Vec<String>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct ExternCrate {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub alias: Option<String>,
}
