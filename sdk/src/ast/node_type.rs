use serde::{Deserialize, Serialize};

use super::{
    contract::{Contract, Struct},
    custom_type::{Type, TypeAlias},
    definition::{Const, Definition, Enum, Plane},
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    literal::Literal,
    misc::{Macro, Misc},
    node::Location,
    pattern::Pattern,
    statement::Statement,
};
use quote::ToTokens;
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
    /// A named type or path, including any generics as represented in the token stream
    Path(String),
    /// A reference `&T` or `&mut T`
    Reference { inner: Box<TypeNode>, mutable: bool },
    /// A raw pointer `*const T` or `*mut T`
    Ptr { inner: Box<TypeNode>, mutable: bool },
    /// A tuple type `(T1, T2, ...)`
    Tuple(Vec<TypeNode>),
    /// An array type `[T; len]`, with optional length if parseable
    Array {
        inner: Box<TypeNode>,
        len: Option<usize>,
    },
    /// A slice type `[T]`
    Slice(Box<TypeNode>),
    /// A bare function pointer `fn(a, b) -> R`
    BareFn {
        inputs: Vec<TypeNode>,
        output: Box<TypeNode>,
    },
}

impl TypeNode {
    /// Build a `TypeNode` representation from a `syn::Type`
    #[must_use]
    pub fn from_syn_item(ty: &syn::Type) -> TypeNode {
        match ty {
            syn::Type::Path(type_path) => TypeNode::Path(type_path.to_token_stream().to_string()),
            syn::Type::Reference(type_ref) => {
                let inner = TypeNode::from_syn_item(&type_ref.elem);
                TypeNode::Reference {
                    inner: Box::new(inner),
                    mutable: type_ref.mutability.is_some(),
                }
            }
            syn::Type::Ptr(type_ptr) => {
                let inner = TypeNode::from_syn_item(&type_ptr.elem);
                TypeNode::Ptr {
                    inner: Box::new(inner),
                    mutable: type_ptr.mutability.is_some(),
                }
            }
            syn::Type::Array(type_array) => {
                let inner = TypeNode::from_syn_item(&type_array.elem);
                // Array length parsing not supported; default to None
                let len = None;
                TypeNode::Array {
                    inner: Box::new(inner),
                    len,
                }
            }
            syn::Type::Slice(type_slice) => {
                let inner = TypeNode::from_syn_item(&type_slice.elem);
                TypeNode::Slice(Box::new(inner))
            }
            syn::Type::Tuple(type_tuple) => {
                let elems = type_tuple
                    .elems
                    .iter()
                    .map(TypeNode::from_syn_item)
                    .collect();
                TypeNode::Tuple(elems)
            }
            syn::Type::BareFn(type_fn) => {
                let inputs = type_fn
                    .inputs
                    .iter()
                    .map(|arg| TypeNode::from_syn_item(&arg.ty))
                    .collect();
                let output = if let syn::ReturnType::Type(_, ty) = &type_fn.output {
                    TypeNode::from_syn_item(ty)
                } else {
                    TypeNode::Empty
                };
                TypeNode::BareFn {
                    inputs,
                    output: Box::new(output),
                }
            }
            syn::Type::Group(type_group) => TypeNode::from_syn_item(&type_group.elem),
            syn::Type::Paren(type_paren) => TypeNode::from_syn_item(&type_paren.elem),
            syn::Type::Infer(_) => TypeNode::Path("_".to_string()),
            syn::Type::Never(_) => TypeNode::Path("!".to_string()),
            syn::Type::Macro(mac) => TypeNode::Path(mac.mac.path.to_token_stream().to_string()),
            _ => TypeNode::Path(ty.to_token_stream().to_string()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum ContractType {
    Struct(Rc<Contract>),
    Enum(Rc<Contract>),
}

impl ContractType {
    #[must_use = "Use this method to get the id of the contract"]
    pub fn id(&self) -> u32 {
        match self {
            ContractType::Struct(c) | ContractType::Enum(c) => c.id,
        }
    }

    #[must_use = "Use this method to get the location of the contract"]
    pub fn location(&self) -> Location {
        match self {
            ContractType::Struct(c) | ContractType::Enum(c) => c.location.clone(),
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
pub fn get_node_kind_node_id(node: &NodeKind) -> u32 {
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
pub fn get_expression_parent_type_id(node: &ExpressionParentType) -> u32 {
    match node {
        ExpressionParentType::Function(f) => f.id,
        ExpressionParentType::Expression(e) => e.id(),
    }
}

#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub fn get_node_location(node: &NodeKind) -> Location {
    match node {
        NodeKind::File(f) => Location {
            source: f.source_code.clone(),
            offset_start: 0,
            offset_end: f.source_code.len() as u32,
            start_column: 0,
            start_line: 0,
            end_column: f.source_code.lines().last().unwrap_or_default().len() as u32,
            end_line: f.source_code.lines().count() as u32,
        },
        NodeKind::FnParameter(p) => p.location.clone(),
        NodeKind::Statement(s) => s.location(),
        NodeKind::Pattern(p) => p.location.clone(),
        NodeKind::Literal(l) => l.location(),
        NodeKind::Misc(m) => m.location(),
    }
}
