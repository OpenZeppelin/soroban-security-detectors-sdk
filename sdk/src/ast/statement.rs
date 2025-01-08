#![warn(clippy::pedantic)]
use super::expression::Expression;

pub enum Statement {
    Expression(Expression),
}
