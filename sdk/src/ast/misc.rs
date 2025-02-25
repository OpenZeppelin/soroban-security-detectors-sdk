#![warn(clippy::pedantic)]
use std::rc::Rc;

use crate::{ast_enum, ast_nodes};

use super::{
    custom_type::Type,
    node::{Location, Mutability, TLocation, Visibility},
};
use soroban_security_rules_macro_lib::node_location;

ast_enum! {
    pub enum Misc {
        Field(Rc<Field>),
        Macro(Rc<Macro>),
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
