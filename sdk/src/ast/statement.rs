#![warn(clippy::pedantic)]
use super::{
    definition::Definition,
    expression::Expression,
    misc::Macro,
    node::{Location, TLocation},
    pattern::Pattern,
};
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Statement {
    Expression(Expression),
    Block(Rc<Block>),
    Let(Rc<Let>),
    Macro(Rc<Macro>),
    Definition(Definition),
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

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Block {
    pub id: u128,
    pub location: Location,
    pub statements: Vec<Statement>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Let {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub pattern: Pattern,
    pub initial_value: Option<Expression>,
    pub initial_value_alternative: Option<Expression>,
}
