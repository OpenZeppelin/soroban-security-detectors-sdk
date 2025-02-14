#![warn(clippy::pedantic)]
use std::rc::Rc;

use crate::ast_nodes;

use super::{
    custom_type::Type,
    node::{Location, Mutability, TLocation, Visibility},
};
use soroban_security_rules_macro_lib::node_location;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Misc {
    Field(Rc<Field>),
    Macro(Rc<Macro>),
}

impl Misc {
    #[must_use = "Use this method to get the id of the miscalaneous node"]
    pub fn id(&self) -> u128 {
        match self {
            Misc::Field(field) => field.id,
            Misc::Macro(macro_) => macro_.id,
        }
    }

    #[must_use = "Use this method to get the location of the miscalaneous node"]
    pub fn location(&self) -> Location {
        match self {
            Misc::Field(field) => field.location.clone(),
            Misc::Macro(macro_) => macro_.location.clone(),
        }
    }
}

ast_nodes! {
    pub struct Field {
        pub name: Option<String>,
        pub visibility: Visibility,
        pub mutability: Mutability,
        pub ty: Type,
    }

    pub struct Macro {
        pub name: String,
        pub text: String,
    }
}
