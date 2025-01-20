#![warn(clippy::pedantic)]

use serde::{Deserialize, Serialize};

use super::{
    contract::Contract,
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    node::{Location, TLocation},
    statement::Statement,
};
use std::rc::Rc;

pub type RcFile = Rc<File>;
pub type RcContract = Rc<Contract>;
pub type RcFunction = Rc<Function>;
pub type RcFnParameter = Rc<FnParameter>;
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
    FnParameter(RcFnParameter),
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
    Parameter(RcFnParameter),
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

//TODO: rewrite below

#[must_use]
pub fn get_node_kind_node_id(node: &NodeKind) -> u128 {
    match node {
        NodeKind::File(f) => f.id,
        NodeKind::Contract(c) => c.id,
        NodeKind::Function(f) => f.id,
        NodeKind::FnParameter(p) => p.id,
        NodeKind::Statement(s) => s.id(),
        NodeKind::FunctionCall(f) => f.id,
        NodeKind::MethodCall(m) => m.id,
    }
}

#[must_use]
pub fn get_expression_parent_type_id(node: &ExpressionParentType) -> u128 {
    match node {
        ExpressionParentType::Function(f) => f.id,
        ExpressionParentType::Expression(e) => e.id(),
    }
}

#[must_use]
pub fn get_node_location(node: &NodeKind) -> Location {
    match node {
        NodeKind::File(f) => Location {
            source_code: f.source_code.clone(),
            start_line: 0,
            start_col: 0,
            end_line: f.source_code.lines().count(),
            end_col: f.source_code.len(),
        },
        NodeKind::Contract(c) => c.location(),
        NodeKind::Function(f) => f.location(),
        NodeKind::FnParameter(p) => p.location(),
        NodeKind::Statement(s) => s.location(),
        NodeKind::FunctionCall(f) => f.location(),
        NodeKind::MethodCall(m) => m.location(),
    }
}
