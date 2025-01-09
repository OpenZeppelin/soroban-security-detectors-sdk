#![warn(clippy::pedantic)]

use super::{
    contract::Contract,
    expression::{Expression, FunctionCall, MethodCall},
    file::File,
    function::Function,
    statement::Statement,
};
use std::rc::Rc;

pub type RcFile = Rc<File>;
pub type RcContract = Rc<Contract>;
pub type RcFunction = Rc<Function>;
pub type RcExpression = Rc<Expression>;
pub type RcFunctionCall = Rc<FunctionCall>;
pub type RcMethodCall = Rc<MethodCall>;

#[derive(Clone)]
pub enum NodeType {
    File(RcFile),
    Contract(RcContract),
    Function(RcFunction),
    Struct,
    Enum,
    Statement,
    FunctionCall(RcFunctionCall),
    MethodCall(RcMethodCall),
}

#[derive(Clone)]
pub enum FileChildType {
    Contract(RcContract),
}

#[derive(Clone)]
pub enum ContractParentType {
    File(RcFile),
}

#[derive(Clone)]
pub enum ContractChildType {
    Function(RcFunction),
    Constant,
}

#[derive(Clone)]
pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

#[derive(Clone)]
pub enum FunctionChildType {
    Statement(Statement),
}

#[derive(Clone)]
pub enum FunctionCallParentType {
    Function(RcFunction),
}

#[derive(Clone)]
pub enum FunctionCallChildType {}

#[derive(Clone)]
pub enum MethodCallParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

#[derive(Clone)]
pub enum MethodCallChildType {
    Expression(RcExpression),
}
