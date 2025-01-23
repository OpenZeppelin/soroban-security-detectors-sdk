#![warn(clippy::pedantic)]

use std::{cell::RefCell, rc::Rc};

use quote::ToTokens;
use syn::{ItemEnum, ItemStruct};
use uuid::Uuid;

use crate::{location, Codebase, OpenState};

use super::{
    contract::Contract,
    custom_type::{CustomType, EnumType, StructType, Type},
    expression::{
        Array, Assign, BinEx, Binary, Break, Cast, ConstBlock, EBlock, Expression, ForLoop,
        FunctionCall, Identifier, If, IndexAccess, LetGuard, MemberAccess, MethodCall, Reference,
    },
    pattern::Pattern,
    statement::{Block, Statement},
};

pub(crate) fn build_contract(struct_item: &ItemStruct) -> Rc<Contract> {
    let mut fields = Vec::new();
    for field in &struct_item.fields {
        let field_name = match &field.ident {
            Some(ident) => ident.to_string(),
            None => "unnamed".to_string(),
        };
        let field_type = Type::T(field.ty.to_token_stream().to_string());
        fields.push((field_name, field_type));
    }
    Rc::new(Contract {
        id: Uuid::new_v4().as_u128(),
        location: location!(struct_item),
        name: Contract::contract_name_from_syn_item(struct_item),
        fields,
        methods: RefCell::new(Vec::new()),
    })
}

pub(crate) fn build_struct_custom_type(struct_item: &ItemStruct) -> CustomType {
    let mut fields = Vec::new();
    for field in &struct_item.fields {
        let field_name = match &field.ident {
            Some(ident) => ident.to_string(),
            None => "unnamed".to_string(),
        };
        let field_type = Type::T(field.ty.to_token_stream().to_string());
        fields.push((field_name, field_type));
    }
    CustomType::Struct(Rc::new(StructType {
        id: Uuid::new_v4().as_u128(),
        location: location!(struct_item),
        name: struct_item.ident.to_string(),
        fields,
        children: RefCell::new(Vec::new()),
    }))
}

pub(crate) fn build_enum_custom_type(enum_item: &ItemEnum) -> CustomType {
    let mut variants = Vec::new();
    for variant in &enum_item.variants {
        let variant_name = variant.ident.to_string();
        variants.push(variant_name);
    }
    CustomType::Enum(Rc::new(EnumType {
        id: Uuid::new_v4().as_u128(),
        location: location!(enum_item),
        name: enum_item.ident.to_string(),
        variants,
        children: RefCell::new(Vec::new()),
    }))
}

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
        is_tried,
    }))
}

pub(crate) fn build_method_call_expression(
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
        is_tried,
    }))
}

pub(crate) fn build_reference_expression(
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

pub(crate) fn build_member_access_expression(
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
        is_tried,
    }))
}

pub(crate) fn build_assign_expresison(
    codebase: &mut Codebase<OpenState>,
    assign: &syn::ExprAssign,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let left = codebase.build_expression(&assign.left, id, is_tried);
    let right = codebase.build_expression(&assign.right, id, is_tried);
    Expression::Assign(Rc::new(Assign {
        id,
        location: location!(assign),
        left,
        right,
    }))
}

pub(crate) fn build_binary_expression(
    codebase: &mut Codebase<OpenState>,
    binary: &syn::ExprBinary,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let left = codebase.build_expression(&binary.left, id, is_tried);
    let right = codebase.build_expression(&binary.right, id, is_tried);
    let binex = Rc::new(BinEx {
        id,
        location: location!(binary),
        left,
        right,
    });
    Expression::Binary(Binary::from_syn_item(binex, &binary.op))
}

pub(crate) fn build_break_expression(
    codebase: &mut Codebase<OpenState>,
    expr_break: &syn::ExprBreak,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    if let Some(inner_expr) = &expr_break.expr {
        let expression = Some(codebase.build_expression(inner_expr, id, is_tried));
        Expression::Break(Rc::new(Break {
            id,
            location: location!(expr_break),
            expression,
        }))
    } else {
        Expression::Break(Rc::new(Break {
            id,
            location: location!(expr_break),
            expression: None,
        }))
    }
}

pub(crate) fn build_cast_expression(
    codebase: &mut Codebase<OpenState>,
    expr_cast: &syn::ExprCast,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let base = codebase.build_expression(&expr_cast.expr, id, is_tried);
    Expression::Cast(Rc::new(Cast {
        id,
        location: location!(expr_cast),
        base,
        target_type: Type::T(expr_cast.ty.to_token_stream().to_string()),
    }))
}

pub(crate) fn build_block_statement(
    codebase: &mut Codebase<OpenState>,
    block: &syn::Block,
) -> Statement {
    let id = Uuid::new_v4().as_u128();
    let statements = block
        .stmts
        .iter()
        .map(|stmt| codebase.build_statement(stmt, id))
        .collect();
    Statement::Block(Rc::new(Block {
        id,
        location: location!(block),
        statements,
    }))
}

pub(crate) fn build_eblock_expression(block: &Rc<Block>, id: u128) -> Expression {
    Expression::EBlock(Rc::new(EBlock {
        id,
        location: block.location.clone(),
        block: block.clone(),
    }))
}

pub(crate) fn build_const_block_expression(block: &Rc<Block>, id: u128) -> Expression {
    Expression::Const(Rc::new(ConstBlock {
        id,
        location: block.location.clone(),
        block: block.clone(),
    }))
}

pub(crate) fn build_for_loop_expression(
    codebase: &mut Codebase<OpenState>,
    for_loop: &syn::ExprForLoop,
    block: &Rc<Block>,
    id: u128,
) -> Expression {
    let expression = codebase.build_expression(&for_loop.expr, id, false);
    Expression::ForLoop(Rc::new(ForLoop {
        id,
        location: location!(for_loop),
        expression,
        block: block.clone(),
    }))
}

pub(crate) fn build_if_expression(
    codebase: &mut Codebase<OpenState>,
    if_expr: &syn::ExprIf,
    then_branch: Rc<Block>,
    id: u128,
) -> Expression {
    let condition = codebase.build_expression(&if_expr.cond, id, false);
    let else_branch = if let Some((_, else_expr)) = &if_expr.else_branch {
        Some(codebase.build_expression(else_expr, id, false))
    } else {
        None
    };
    Expression::If(Rc::new(If {
        id,
        location: location!(if_expr),
        condition,
        then_branch,
        else_branch,
    }))
}

pub(crate) fn build_index_access_expression(
    codebase: &mut Codebase<OpenState>,
    index_access: &syn::ExprIndex,
    is_tried: bool,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let base = codebase.build_expression(&index_access.expr, id, is_tried);
    let index = codebase.build_expression(&index_access.index, id, is_tried);
    Expression::IndexAccess(Rc::new(IndexAccess {
        id,
        location: location!(index_access),
        base,
        index,
    }))
}

pub(crate) fn build_let_guard_expression(
    codebase: &mut Codebase<OpenState>,
    let_guard: &syn::ExprLet,
    guard: Pattern,
    id: u128,
    is_tried: bool,
) -> Expression {
    let value = codebase.build_expression(&let_guard.expr, id, is_tried);
    Expression::LetGuard(Rc::new(LetGuard {
        id,
        location: location!(let_guard),
        guard,
        value,
    }))
}

//TODO if a deeper analysis of patterns is needed, this function should be updated
pub(crate) fn build_pattern(pat: &syn::Pat) -> Pattern {
    let id = Uuid::new_v4().as_u128();
    let location = location!(pat);
    let pat_string = match pat {
        syn::Pat::Const(expr_const) => expr_const.to_token_stream().to_string(),
        syn::Pat::Ident(pat_ident) => pat_ident.to_token_stream().to_string(),
        syn::Pat::Lit(expr_lit) => expr_lit.to_token_stream().to_string(),
        syn::Pat::Macro(expr_macro) => expr_macro.to_token_stream().to_string(),
        syn::Pat::Or(pat_or) => pat_or.to_token_stream().to_string(),
        syn::Pat::Paren(pat_paren) => pat_paren.to_token_stream().to_string(),
        syn::Pat::Path(expr_path) => expr_path.to_token_stream().to_string(),
        syn::Pat::Range(expr_range) => expr_range.to_token_stream().to_string(),
        syn::Pat::Reference(pat_reference) => pat_reference.to_token_stream().to_string(),
        syn::Pat::Rest(pat_rest) => pat_rest.to_token_stream().to_string(),
        syn::Pat::Slice(pat_slice) => pat_slice.to_token_stream().to_string(),
        syn::Pat::Struct(pat_struct) => pat_struct.to_token_stream().to_string(),
        syn::Pat::Tuple(pat_tuple) => pat_tuple.to_token_stream().to_string(),
        syn::Pat::TupleStruct(pat_tuple_struct) => pat_tuple_struct.to_token_stream().to_string(),
        syn::Pat::Type(pat_type) => pat_type.to_token_stream().to_string(),
        syn::Pat::Verbatim(token_stream) => token_stream.to_string(),
        syn::Pat::Wild(_) => "_".to_string(),
        _ => String::new(),
    };
    Pattern {
        id,
        location,
        kind: pat_string.to_string(),
    }
}
