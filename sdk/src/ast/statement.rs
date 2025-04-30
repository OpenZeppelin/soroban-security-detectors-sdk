#![warn(clippy::pedantic)]
use crate::{ast_enum, ast_nodes};

use super::{
    definition::Definition,
    expression::Expression,
    misc::Macro,
    node::{Location, TLocation},
    pattern::Pattern,
};
use soroban_security_detectors_macro_lib::node_location;
use std::rc::Rc;

ast_enum! {
    pub enum Statement {
        @ty Expression(Expression),
        Block(Rc<Block>),
        Let(Rc<Let>),
        Macro(Rc<Macro>),
        @ty Definition(Definition),
    }
}

ast_nodes! {
    pub struct Block {
        pub statements: Vec<Statement>,
    }

    pub struct Let {
        pub name: String,
        pub pattern: Pattern,
        pub initial_value: Option<Expression>,
        pub initial_value_alternative: Option<Expression>,
    }
}
