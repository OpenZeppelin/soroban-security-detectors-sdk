#![warn(clippy::pedantic)]
use crate::{ast_enum, ast_nodes};

use super::{
    definition::Definition,
    expression::Expression,
    misc::Macro,
    node::{Location, TLocation},
    pattern::Pattern,
};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

ast_enum! {
    pub enum Statement {
        Expression(Expression),
        Block(Rc<Block>),
        Let(Rc<Let>),
        Macro(Rc<Macro>),
        Definition(Definition),
    }
}

impl Statement {
    #[must_use = "Use this method to get the id of the statement"]
    pub fn id(&self) -> u128 {
        match self {
            Statement::Expression(expr) => expr.id(),
            Statement::Block(block) => block.id,
            Statement::Let(let_) => let_.id,
            Statement::Macro(macro_) => macro_.id,
            Statement::Definition(definition) => definition.id(),
        }
    }

    #[must_use = "Use this method to get the location of the statement"]
    pub fn location(&self) -> Location {
        match self {
            Statement::Expression(expr) => expr.location(),
            Statement::Block(block) => block.location(),
            Statement::Let(let_) => let_.location(),
            Statement::Macro(macro_) => macro_.location(),
            Statement::Definition(definition) => definition.location(),
        }
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
