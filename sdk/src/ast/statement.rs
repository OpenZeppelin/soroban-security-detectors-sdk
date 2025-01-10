#![warn(clippy::pedantic)]
use super::expression::Expression;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Statement {
    Expression(Expression),
}
