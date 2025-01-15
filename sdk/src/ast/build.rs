#![warn(clippy::pedantic)]

use std::{cell::RefCell, rc::Rc};

use uuid::Uuid;

use crate::{location, Codebase, OpenState};

use super::expression::{
    Array, Expression, FunctionCall, Identifier, MemberAccess, MethodCall, Reference,
};

pub(crate) fn build_array_expression(
    codebase: &mut Codebase<OpenState>,
    array_expr: &syn::ExprArray,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let elements = array_expr
        .elems
        .iter()
        .map(|elem| codebase.build_expression(elem, id, is_tried))
        .collect();
    Expression::Array(Rc::new(Array {
        id,
        location: location!(array_expr),
        elements,
    }))
}

pub(crate) fn build_function_call_expression(
    codebase: &mut Codebase<OpenState>,
    expr_call: &syn::ExprCall,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let parameters = expr_call
        .args
        .iter()
        .map(|arg| codebase.build_expression(arg, id, false))
        .collect();
    Expression::FunctionCall(Rc::new(FunctionCall {
        id: Uuid::new_v4().as_u128(),
        location: location!(expr_call),
        function_name: FunctionCall::function_name_from_syn_item(expr_call),
        parameters,
        children: Vec::new(),
        is_tried,
    }))
}

pub(crate) fn build_method_call(
    codebase: &mut Codebase<OpenState>,
    method_call: &syn::ExprMethodCall,
    is_tried: bool,
) -> Expression {
    let method_call_id = Uuid::new_v4().as_u128();
    let base = codebase.build_expression(&method_call.receiver, method_call_id, false);
    Expression::MethodCall(Rc::new(MethodCall {
        id: method_call_id,
        location: location!(method_call),
        method_name: MethodCall::method_name_from_syn_item(method_call),
        base,
        children: RefCell::new(Vec::new()),
        is_tried,
    }))
}

pub(crate) fn build_reference(
    codebase: &mut Codebase<OpenState>,
    expr_ref: &syn::ExprReference,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let inner = codebase.build_expression(&expr_ref.expr, id, is_tried);
    Expression::Reference(Rc::new(Reference {
        id,
        location: location!(expr_ref),
        inner,
        is_mutable: expr_ref.mutability.is_some(),
    }))
}

pub(crate) fn build_identifier(expr_path: &syn::ExprPath) -> Expression {
    Expression::Identifier(Rc::new(Identifier {
        id: Uuid::new_v4().as_u128(),
        location: location!(expr_path),
        name: quote::quote! {#expr_path}.to_string(),
    }))
}

pub(crate) fn build_member_access(
    codebase: &mut Codebase<OpenState>,
    member_access: &syn::ExprField,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let base = codebase.build_expression(&member_access.base, id, is_tried);
    Expression::MemberAccess(Rc::new(MemberAccess {
        id,
        location: location!(member_access),
        base,
        member_name: MemberAccess::member_name_from_syn_item(member_access),
        children: Vec::new(),
        is_tried,
    }))
}
