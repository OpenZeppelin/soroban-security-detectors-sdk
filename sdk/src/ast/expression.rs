#![warn(clippy::pedantic)]
use super::function::Function;
use super::node::{Location, Node};
use super::node_type::{
    FunctionCallChildType, FunctionCallParentType, MethodCallChildType, MethodCallParentType,
    NodeType,
};
use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::rc::Rc;
use syn::spanned::Spanned;
use syn::{Expr, ExprCall, ExprMethodCall};

#[derive(Clone)]
pub enum Expression {
    FunctionCall(Rc<FunctionCall>),
    MethodCall(Rc<MethodCall>),
    Empty, //TODO remove this option after all expression variants are implemented. Implement Deref for Expression after that.
}

#[allow(clippy::module_name_repetitions)]
pub enum ExpressionParentType {
    Function(Rc<Function>),
    Expression(Rc<Expression>),
}

#[node_location(inner = "inner_struct")]
pub struct FunctionCall {
    pub id: usize,
    pub(crate) inner_struct: Rc<ExprCall>,
    pub parent: FunctionCallParentType,
    pub children: Vec<FunctionCallChildType>,
    pub is_tried: bool,
}

impl Node for FunctionCall {
    fn parent(&self) -> Option<NodeType> {
        match &self.parent {
            FunctionCallParentType::Function(parent) => Some(NodeType::Function(parent.clone())),
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FunctionCallChildType> {
        self.children.iter().cloned()
    }
}

impl FunctionCall {
    #[must_use]
    pub fn function_name(&self) -> String {
        match self.inner_struct.func.as_ref() {
            Expr::Path(ref expr_path) => expr_path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        }
    }
}

#[node_location(inner = "inner_struct")]
pub struct MethodCall {
    pub id: usize,
    pub(crate) inner_struct: Rc<ExprMethodCall>,
    pub parent: MethodCallParentType,
    pub children: RefCell<Vec<MethodCallChildType>>,
    pub is_tried: bool,
}

impl Node for MethodCall {
    fn parent(&self) -> Option<NodeType> {
        match &self.parent {
            MethodCallParentType::Function(parent) => Some(NodeType::Function(parent.clone())),
            MethodCallParentType::Expression(parent) => match &**parent {
                Expression::MethodCall(parent) => parent.parent(),
                Expression::FunctionCall(parent) => NodeType::FunctionCall(parent.clone()).into(),
                Expression::Empty => None,
            },
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = MethodCallChildType> {
        self.children.borrow().clone().into_iter()
    }
}

#[cfg(test)]
mod function_call_tests {
    use super::*;
    use crate::utils::test::create_mock_function;
    use syn::parse_quote;

    #[test]
    fn test_function_call_function_name() {
        let inner_struct = parse_quote! {
            execute("Hello, world!")
        };
        let function_call = FunctionCall {
            id: 0,
            inner_struct: Rc::new(inner_struct),
            parent: FunctionCallParentType::Function(Rc::new(create_mock_function(0))),
            children: vec![],
            is_tried: false,
        };

        assert_eq!(function_call.function_name(), "execute");
    }
}
