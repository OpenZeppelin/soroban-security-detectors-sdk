#![warn(clippy::pedantic)]
use super::expression::Expression;

#[derive(Clone)]
pub enum Statement {
    Expression(Expression),
}
