#![warn(clippy::pedantic)]
use super::{expression::Expression, node::Location, node::TLocation};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Statement {
    Expression(Expression),
    Block(Rc<Block>),
}

impl Statement {
    #[must_use = "Use this method to get the id of the statement"]
    pub fn id(&self) -> u128 {
        match self {
            Statement::Expression(expr) => expr.id(),
            Statement::Block(block) => block.id,
        }
    }

    #[must_use = "Use this method to get the location of the statement"]
    pub fn location(&self) -> Location {
        match self {
            Statement::Expression(expr) => expr.location(),
            Statement::Block(block) => block.location(),
        }
    }
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Block {
    pub id: u128,
    pub location: Location,
    pub statements: Vec<Statement>,
}
