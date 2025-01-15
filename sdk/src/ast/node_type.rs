#![warn(clippy::pedantic)]

use serde::{Deserialize, Serialize};

use super::{
    contract::Contract,
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::Function,
    node::TLocation,
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

#[must_use]
pub fn get_node_kind_node_id(node: &NodeKind) -> u128 {
    match node {
        NodeKind::File(f) => f.id,
        NodeKind::Contract(c) => c.id,
        NodeKind::Function(f) => f.id,
        NodeKind::Statement(s) => match s {
            Statement::Expression(e) => get_expression_id(e),
        },
        NodeKind::FunctionCall(f) => f.id,
        NodeKind::MethodCall(m) => m.id,
    }
}

#[must_use]
pub fn get_expression_parent_type_id(node: &ExpressionParentType) -> u128 {
    match node {
        ExpressionParentType::Function(f) => f.id,
        ExpressionParentType::Expression(e) => get_expression_id(e),
    }
}

#[must_use]
pub fn get_expression_id(node: &Expression) -> u128 {
    match node {
        Expression::FunctionCall(f) => f.id,
        Expression::MethodCall(m) => m.id,
        Expression::Empty => 0,
    }
}

#[must_use]
pub fn get_node_source_code(node: &NodeKind) -> String {
    match node {
        NodeKind::File(f) => f.source_code.clone(),
        NodeKind::Contract(c) => c.source_code(),
        NodeKind::Function(f) => f.source_code(),
        NodeKind::Statement(s) => match s {
            Statement::Expression(e) => match e {
                Expression::MethodCall(m) => m.source_code(),
                Expression::FunctionCall(f) => f.source_code(),
                Expression::Empty => String::new(),
            },
        },
        NodeKind::FunctionCall(f) => f.source_code(),
        NodeKind::MethodCall(m) => m.source_code(),
    }
}
