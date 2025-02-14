#![warn(clippy::pedantic)]

use serde::{Deserialize, Serialize};

use super::{
    contract::Struct,
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
    Struct(RcStruct),
    Enum(RcEnum),
    Contract(RcStruct),
}

impl Default for ContractType {
    fn default() -> Self {
        ContractType::Struct(RcStruct::default())
    }
}

impl TLocation for ContractType {
    fn location(&self) -> Location {
        match self {
            ContractType::Contract(c) => c.location.clone(),
            ContractType::Struct(s) => s.location.clone(),
            ContractType::Enum(e) => e.location.clone(),
        }
    }
    fn source_code(&self) -> String {
        match self {
            ContractType::Contract(c) => c.source_code().clone(),
            ContractType::Struct(s) => s.source_code().clone(),
            ContractType::Enum(e) => e.source_code().clone(),
        }
    }
    fn start_line(&self) -> usize {
        match self {
            ContractType::Contract(c) => c.start_line(),
            ContractType::Struct(s) => s.start_line(),
            ContractType::Enum(e) => e.start_line(),
        }
    }
    fn start_col(&self) -> usize {
        match self {
            ContractType::Contract(c) => c.start_col(),
            ContractType::Struct(s) => s.start_col(),
            ContractType::Enum(e) => e.start_col(),
        }
    }
    fn end_line(&self) -> usize {
        match self {
            ContractType::Contract(c) => c.end_line(),
            ContractType::Struct(s) => s.end_line(),
            ContractType::Enum(e) => e.end_line(),
        }
    }
    fn end_col(&self) -> usize {
        match self {
            ContractType::Contract(c) => c.end_col(),
            ContractType::Struct(s) => s.end_col(),
            ContractType::Enum(e) => e.end_col(),
        }
    }
}

impl ContractType {
    #[must_use = "Use this method to get the id of the contract sub-type"]
    pub fn id(&self) -> u128 {
        match self {
            ContractType::Contract(c) => c.id,
            ContractType::Struct(s) => s.id,
            ContractType::Enum(e) => e.id,
        }
    }
    #[must_use = "Use this method to get the location of the contract sub-type"]
    pub fn location(&self) -> Location {
        match self {
            ContractType::Contract(c) => c.location.clone(),
            ContractType::Struct(s) => s.location.clone(),
            ContractType::Enum(e) => e.location.clone(),
        }
    }
    #[must_use = "Use this method to get the name of the contract sub-type"]
    pub fn name(&self) -> String {
        match self {
            ContractType::Contract(c) => c.name.clone(),
            ContractType::Struct(s) => s.name.clone(),
            ContractType::Enum(e) => e.name.clone(),
        }
    }
    #[must_use = "Use this method to get methods iterator of the contract sub-type"]
    pub fn get_methods(&self) -> impl Iterator<Item = RcFunction> {
        match self {
            ContractType::Contract(c) => c.methods.borrow().clone().into_iter(),
            ContractType::Struct(s) => s.methods.borrow().clone().into_iter(),
            ContractType::Enum(e) => e.methods.borrow().clone().into_iter(),
        }
    }

    pub fn add_method(&self, function: Rc<Function>) {
        match self {
            ContractType::Contract(c) => c.methods.borrow_mut().push(function),
            ContractType::Struct(s) => s.methods.borrow_mut().push(function),
            ContractType::Enum(e) => e.methods.borrow_mut().push(function),
        }
    }

    #[must_use = "Use this method to get functions of the contract sub-type"]
    pub fn get_functions(&self) -> impl Iterator<Item = RcFunction> {
        match self {
            ContractType::Contract(c) => c.functions.borrow().clone().into_iter(),
            ContractType::Struct(s) => s.functions.borrow().clone().into_iter(),
            ContractType::Enum(e) => e.functions.borrow().clone().into_iter(),
        }
    }

    pub fn add_function(&self, function: Rc<Function>) {
        match self {
            ContractType::Contract(c) => c.functions.borrow_mut().push(function),
            ContractType::Struct(s) => s.functions.borrow_mut().push(function),
            ContractType::Enum(e) => e.functions.borrow_mut().push(function),
        }
    }

    pub(crate) fn add_type_alias(&self, type_alias: Rc<TypeAlias>) {
        match self {
            ContractType::Contract(c) => c.type_aliases.borrow_mut().push(type_alias),
            ContractType::Struct(s) => s.type_aliases.borrow_mut().push(type_alias),
            ContractType::Enum(e) => e.type_aliases.borrow_mut().push(type_alias),
        }
    }

    pub(crate) fn add_constant(&self, constant: Rc<Const>) {
        match self {
            ContractType::Contract(c) => c.constants.borrow_mut().push(constant),
            ContractType::Struct(s) => s.constants.borrow_mut().push(constant),
            ContractType::Enum(e) => e.constants.borrow_mut().push(constant),
        }
    }

    pub(crate) fn add_macro(&self, macro_: Rc<Macro>) {
        match self {
            ContractType::Contract(c) => c.macros.borrow_mut().push(macro_),
            ContractType::Struct(s) => s.macros.borrow_mut().push(macro_),
            ContractType::Enum(e) => e.macros.borrow_mut().push(macro_),
        }
    }

    pub(crate) fn add_plane_def(&self, plane_def: Rc<Plane>) {
        match self {
            ContractType::Contract(c) => c.plane_defs.borrow_mut().push(plane_def),
            ContractType::Struct(s) => s.plane_defs.borrow_mut().push(plane_def),
            ContractType::Enum(_) => panic!("Enum cannot have plane definitions"),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum NodeKind {
    File(Rc<File>),
    Contract(ContractType),
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
pub enum ContractChildType {
    Function(RcFunction),
    Constant,
}

pub type CustomTypeChildType = ContractChildType;

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
        NodeKind::Contract(c) => c.id(),
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
        NodeKind::Contract(c) => c.location(),
        NodeKind::FnParameter(p) => p.location(),
        NodeKind::Statement(s) => s.location(),
        NodeKind::Pattern(p) => p.location(),
        NodeKind::Literal(l) => l.location(),
        NodeKind::Misc(m) => m.location(),
    }
}
