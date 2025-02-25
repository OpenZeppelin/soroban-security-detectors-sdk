#![warn(clippy::pedantic)]

use serde::{Deserialize, Serialize};

use super::{
    contract::{Contract, Struct},
    custom_type::TypeAlias,
    definition::{Const, Definition, Enum, Plane},
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    literal::Literal,
    misc::{Macro, Misc},
    node::{Location, TLocation},
    pattern::Pattern,
    statement::Statement,
};
use std::{default, rc::Rc};

pub type RcFile = Rc<File>;
pub type RcContract = Rc<Struct>;
pub type RcFunction = Rc<Function>;
pub type RcFnParameter = Rc<FnParameter>;
pub type RcExpression = Rc<Expression>;
pub type RcFunctionCall = Rc<FunctionCall>;
pub type RcMethodCall = Rc<MethodCall>;
pub type RcEnum = Rc<Enum>;
pub type RcStruct = Rc<Struct>;

#[derive(Clone, PartialEq, Eq, Debug, Default, serde::Serialize, serde::Deserialize)]
pub enum TypeNode {
    #[default]
    Empty,
}

impl TypeNode {
    #[must_use]
    pub fn from_syn_item(_: &syn::Type) -> TypeNode {
        TypeNode::Empty
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum ContractType {
    Struct(Rc<Contract>),
    Enum(Rc<Contract>),
}

impl ContractType {
    pub fn id(&self) -> u128 {
        match self {
            ContractType::Struct(c) => c.id,
            ContractType::Enum(c) => c.id,
        }
    }

    pub fn location(&self) -> Location {
        match self {
            ContractType::Struct(c) => c.location(),
            ContractType::Enum(c) => c.location(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum NodeKind {
    File(Rc<File>),
    FnParameter(RcFnParameter),
    Statement(Statement),
    Pattern(Pattern),
    Literal(Literal),
    Misc(Misc),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FileChildType {
    Definition(Definition),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum ContractParentType {
    File(RcFile),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum StructChildType {
    Function(RcFunction),
    TypeAlias(Rc<TypeAlias>),
    Constant(Rc<Const>),
    Macro(Rc<Macro>),
    Plane(Rc<Plane>),
}

pub type CustomTypeChildType = StructChildType;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionChildType {
    Expression(Expression),
    Statement(Statement),
    Parameter(RcFnParameter),
    Type(TypeNode),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallParentType {
    Function(RcFunction),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallChildType {
    Expression(RcExpression),
}

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
        NodeKind::FnParameter(p) => p.id,
        NodeKind::Statement(s) => s.id(),
        NodeKind::Pattern(p) => p.id,
        NodeKind::Literal(l) => l.id(),
        NodeKind::Misc(m) => m.id(),
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
        NodeKind::FnParameter(p) => p.location(),
        NodeKind::Statement(s) => s.location(),
        NodeKind::Pattern(p) => p.location(),
        NodeKind::Literal(l) => l.location(),
        NodeKind::Misc(m) => m.location(),
    }
}
