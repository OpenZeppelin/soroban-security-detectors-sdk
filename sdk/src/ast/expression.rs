#![warn(clippy::pedantic)]
use super::function::Function;
use super::node::{Location, Node, TLocation};
use super::node_type::{FunctionCallChildType, MemberAccessChildType, MethodCallChildType};
use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::rc::Rc;
use syn::{Expr, ExprCall, ExprMethodCall};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
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

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionCall {
    pub id: u128,
    pub location: Location,
    pub function_name: String,
    pub children: Vec<FunctionCallChildType>,
    pub is_tried: bool,
}

impl Node for FunctionCall {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FunctionCallChildType> {
        self.children.iter().cloned()
    }
}

impl FunctionCall {
    #[must_use]
    pub fn function_name_from_syn_item(function_call: &ExprCall) -> String {
        match function_call.func.as_ref() {
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

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct MethodCall {
    pub id: u128,
    pub location: Location,
    pub method_name: String,
    pub children: RefCell<Vec<MethodCallChildType>>,
    pub is_tried: bool,
}

impl Node for MethodCall {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = MethodCallChildType> {
        self.children.borrow().clone().into_iter()
    }
}

impl MethodCall {
    #[must_use]
    pub fn method_name_from_syn_item(method_call: &ExprMethodCall) -> String {
        method_call.method.to_string()
    }
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct MemberAccess {
    pub id: usize,
    pub location: Location,
    pub member_name: String,
    pub children: Vec<MemberAccessChildType>,
    pub is_tried: bool,
}

impl Node for MemberAccess {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = MemberAccessChildType> {
        self.children.iter().cloned()
    }
}

impl MemberAccess {
    // #[must_use]
    // pub fn member_name(&self) -> String {
    //     match &self.inner_item.member {
    //         syn::Member::Named(ident) => ident.to_string(),
    //         syn::Member::Unnamed(index) => index.index.to_string(),
    //     }
    // }

    // #[must_use]
    // pub fn base(&self) -> Expression {
    //     self.inner_struct.base
    // }
}

#[cfg(test)]
mod function_call_tests {
    use super::*;
    use crate::location;
    use syn::parse_quote;

    #[test]
    fn test_function_call_function_name() {
        let inner_struct: ExprCall = parse_quote! {
            execute("Hello, world!")
        };
        let function_call = FunctionCall {
            id: 0,
            location: location!(inner_struct),
            function_name: FunctionCall::function_name_from_syn_item(&inner_struct),
            children: vec![],
            is_tried: false,
        };

        assert_eq!(function_call.function_name, "execute");
    }
}
