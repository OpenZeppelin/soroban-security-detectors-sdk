#![warn(clippy::pedantic)]

use serde::{Deserialize, Serialize};

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

#[derive(Clone, Serialize, Deserialize)]
pub enum TypeNode {
    Empty,
}

impl TypeNode {
    #[must_use]
    pub fn from_syn_item(_: &syn::Type) -> TypeNode {
        TypeNode::Empty
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum NodeKind {
    File(Rc<File>),
    Contract(RcContract),
    Function(RcFunction),
    Struct,
    Enum,
    Statement(Statement),
    FunctionCall(RcFunctionCall),
    MethodCall(RcMethodCall),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FileChildType {
    Contract(RcContract),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum ContractParentType {
    File(RcFile),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum ContractChildType {
    Function(RcFunction),
    Constant,
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionChildType {
    Statement(Statement),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallParentType {
    Function(RcFunction),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallChildType {}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MethodCallParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MethodCallChildType {
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MemberAccessParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MemberAccessChildType {
    Expression(RcExpression),
}
