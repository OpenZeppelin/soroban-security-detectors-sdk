#![warn(clippy::pedantic)]

use super::{
    contract::Contract, expression::Expression, file::File, function::Function,
    statement::Statement,
};
use std::rc::Rc;

pub type RcFile = Rc<File>;
pub type RcContract = Rc<Contract>;
pub type RcFunction = Rc<Function>;
pub type RcExpression = Rc<Expression>;

pub enum NodeType {
    File(RcFile),
    Contract(RcContract),
    Function(RcFunction),
    Struct,
    Enum,
    Statement,
}

pub enum FileChildType {
    Contract(RcContract),
}

pub enum ContractParentType {
    File(RcFile),
}

pub enum ContractChildType {
    Function(RcFunction),
    Constant,
}

pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

pub enum FunctionChildType {
    Statement(Statement),
}

pub enum FunctionCallParentType {
    Function(RcFunction),
}

pub enum FunctionCallChildType {}

pub enum MethodCallParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

pub enum MethodCallChildType {
    Expression(RcExpression),
}
