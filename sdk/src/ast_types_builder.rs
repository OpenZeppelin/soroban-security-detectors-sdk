use std::{cell::RefCell, rc::Rc};

use proc_macro2::token_stream;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{Attribute, ExprBlock, ItemConst, ItemEnum, ItemFn, ItemStruct, PointerMutability};
use uuid::Uuid;

use crate::ast::custom_type::{Type, TypeAlias};
use crate::ast::definition::{Module, Plane, Static, Trait};
use crate::ast::expression::{
    Addr, Array, Assign, BinEx, Binary, Break, Cast, Closure, ConstBlock, Continue, EBlock,
    EStruct, ForLoop, FunctionCall, Identifier, If, IndexAccess, LetGuard, Lit, Loop, Match,
    MatchArm, MemberAccess, MethodCall, Parenthesized, Range, Reference, Repeat, Return, Try,
    Tuple, UnEx, Unary, Unsafe, While,
};
use crate::ast::misc::{Field, Macro, Misc};
use crate::ast::node::Mutability;
use crate::ast::node_type::ContractType;
use crate::custom_type::Typedef;
use crate::definition::Implementation;
use crate::location;
use crate::{Codebase, OpenState};

use crate::ast::contract::Struct;
use crate::ast::definition::{Const, Definition, Enum};
use crate::ast::directive::{Directive, Use};
use crate::ast::expression::Expression;
use crate::ast::function::{FnParameter, Function};
use crate::ast::literal::{
    LBString, LBool, LByte, LCString, LChar, LFloat, LInt, LString, Literal,
};
use crate::ast::node::Visibility;
use crate::ast::node_type::{NodeKind, TypeNode};
use crate::ast::pattern::Pattern;
use crate::ast::statement::{Block, Statement};

#[allow(clippy::cast_possible_truncation)]
fn get_node_id() -> u32 {
    Uuid::new_v4().as_u128() as u32
}

/// Extracts attribute names (first path segment) from a list of attributes.
fn extract_attrs(attrs: &[Attribute]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|a| a.path().segments.first())
        .map(|seg| seg.ident.to_string())
        .collect()
}

pub(crate) fn build_struct(
    codebase: &mut Codebase<OpenState>,
    struct_item: &ItemStruct,
    parent_id: u32,
) -> Rc<Struct> {
    let attributes = extract_attrs(&struct_item.attrs);
    let mut fields = Vec::new();
    for field in &struct_item.fields {
        let field_name = match &field.ident {
            Some(ident) => ident.to_string(),
            None => "unnamed".to_string(),
        };
        let field_type = Type::Typedef(field.ty.to_token_stream().to_string());
        fields.push((field_name, field_type));
    }
    let rc_struct = Rc::new(Struct {
        id: get_node_id(),
        attributes,
        location: location!(struct_item),
        name: Struct::contract_name_from_syn_item(struct_item),
        fields,
        is_contract: Struct::is_struct_contract(struct_item),
    });
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Struct(rc_struct.clone()))),
        parent_id,
    );
    rc_struct
}

pub(crate) fn build_enum(
    codebase: &mut Codebase<OpenState>,
    enum_item: &ItemEnum,
    parent_id: u32,
) -> Rc<Enum> {
    let attributes = extract_attrs(&enum_item.attrs);
    let mut variants = Vec::new();
    for variant in &enum_item.variants {
        let variant_name = variant.ident.to_string();
        variants.push(variant_name);
    }
    let rc_enum = Rc::new(Enum {
        id: get_node_id(),
        attributes,
        location: location!(enum_item),
        name: enum_item.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&enum_item.vis),
        variants,
    });
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Enum(rc_enum.clone()))),
        parent_id,
    );
    rc_enum
}

pub(crate) fn build_array_expression(
    codebase: &mut Codebase<OpenState>,
    array_expr: &syn::ExprArray,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let elements = array_expr
        .elems
        .iter()
        .map(|elem| codebase.build_expression(elem, id))
        .collect();
    let expr = Expression::Array(Rc::new(Array {
        id,
        location: location!(array_expr),
        elements,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_function_call_expression(
    codebase: &mut Codebase<OpenState>,
    expr_call: &syn::ExprCall,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let parameters = expr_call
        .args
        .iter()
        .map(|arg| codebase.build_expression(arg, id))
        .collect();
    let function_call_id = get_node_id();
    let expr = Expression::FunctionCall(Rc::new(FunctionCall {
        id: function_call_id,
        location: location!(expr_call),
        function_name: FunctionCall::function_name_from_syn_item(expr_call),
        expression: codebase.build_expression(&expr_call.func, function_call_id),
        parameters,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_method_call_expression(
    codebase: &mut Codebase<OpenState>,
    method_call: &syn::ExprMethodCall,
    parent_id: u32,
) -> Expression {
    let method_call_id = get_node_id();
    let base = codebase.build_expression(&method_call.receiver, method_call_id);
    let expr = Expression::MethodCall(Rc::new(MethodCall {
        id: method_call_id,
        location: location!(method_call),
        method_name: MethodCall::method_name_from_syn_item(method_call),
        base,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

fn get_impl_type_name(item_impl: &syn::ItemImpl) -> Option<String> {
    if let syn::Type::Path(type_path) = &*item_impl.self_ty {
        if let Some(segment) = type_path.path.segments.last() {
            return Some(segment.ident.to_string());
        }
    }
    None
}

pub(crate) fn process_item_impl(
    codebase: &mut Codebase<OpenState>,
    item_impl: &syn::ItemImpl,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_impl.attrs);
    let for_type: Option<Type> = get_impl_type_name(item_impl).map(Type::Typedef);
    let mut functions = Vec::new();
    let mut constants = Vec::new();
    let mut types = Vec::new();
    let mut macroses = Vec::new();
    let mut planes = Vec::new();

    for item in &item_impl.items {
        match item {
            syn::ImplItem::Fn(assoc_fn) => {
                let function = build_function_from_impl_item_fn(codebase, assoc_fn, id);
                functions.push(function.clone());
            }
            syn::ImplItem::Const(impl_item_const) => {
                let const_definition =
                    build_const_definition_for_impl_item_const(codebase, impl_item_const, id);
                if let Definition::Const(constant) = const_definition {
                    constants.push(constant);
                }
            }
            syn::ImplItem::Type(impl_item_type) => {
                let type_alias = build_type_alias_from_impl_item_type(codebase, impl_item_type, id);
                types.push(type_alias);
            }
            syn::ImplItem::Macro(impl_item_macro) => {
                let macro_definition =
                    build_marco_definition_for_impl_item_macro(codebase, impl_item_macro, id);
                if let Definition::Macro(macro_definition) = macro_definition {
                    macroses.push(macro_definition);
                }
            }
            syn::ImplItem::Verbatim(token_stream) => {
                let def = build_plane_definition(codebase, token_stream, id);
                if let Definition::Plane(plane) = def {
                    planes.push(plane);
                }
            }
            _ => unreachable!(),
        }
    }

    let implementation_definition = Definition::Implementation(Rc::new(Implementation {
        id,
        attributes,
        location: location!(item_impl),
        for_type,
        functions,
        constants,
        type_aliases: types,
        macros: macroses,
        plane_defs: planes,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(implementation_definition.clone())),
        parent_id,
    );
    implementation_definition
}

pub(crate) fn build_reference_expression(
    codebase: &mut Codebase<OpenState>,
    expr_ref: &syn::ExprReference,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let inner = codebase.build_expression(&expr_ref.expr, id);
    let expr = Expression::Reference(Rc::new(Reference {
        id,
        location: location!(expr_ref),
        inner,
        is_mutable: expr_ref.mutability.is_some(),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_return_expression(
    codebase: &mut Codebase<OpenState>,
    expr_return: &syn::ExprReturn,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = expr_return
        .expr
        .as_ref()
        .map(|expr| codebase.build_expression(expr, id));
    let expr = Expression::Return(Rc::new(Return {
        id,
        location: location!(expr_return),
        expression,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_repeat_expression(
    codebase: &mut Codebase<OpenState>,
    expr_repeat: &syn::ExprRepeat,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = codebase.build_expression(&expr_repeat.expr, id);
    let count = codebase.build_expression(&expr_repeat.len, id);
    let expr = Expression::Repeat(Rc::new(Repeat {
        id,
        location: location!(expr_repeat),
        expression,
        count,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_identifier(
    codebase: &mut Codebase<OpenState>,
    expr_path: &syn::ExprPath,
    parent_id: u32,
) -> Expression {
    let expr = Expression::Identifier(Rc::new(Identifier {
        id: get_node_id(),
        location: location!(expr_path),
        name: quote::quote! {#expr_path}.to_string(),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_parenthesied_expression(
    codebase: &mut Codebase<OpenState>,
    parenthesied: &syn::ExprParen,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = codebase.build_expression(&parenthesied.expr, id);
    let expr = Expression::Parenthesized(Rc::new(Parenthesized {
        id,
        location: location!(parenthesied),
        expression,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_range_expression(
    codebase: &mut Codebase<OpenState>,
    expr_range: &syn::ExprRange,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let start = match &expr_range.start {
        Some(start) => {
            let expr = codebase.build_expression(start.as_ref(), id);
            Some(expr)
        }
        None => None,
    };
    let end = match &expr_range.end {
        Some(end) => {
            let expr = codebase.build_expression(end.as_ref(), id);
            Some(expr)
        }
        None => None,
    };
    let is_closed = matches!(expr_range.limits, syn::RangeLimits::Closed(_));
    let range = Expression::Range(Rc::new(Range {
        id,
        location: location!(expr_range),
        start,
        end,
        is_closed,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(range.clone())),
        parent_id,
    );
    range
}

pub(crate) fn build_member_access_expression(
    codebase: &mut Codebase<OpenState>,
    member_access: &syn::ExprField,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = codebase.build_expression(&member_access.base, id);
    let expr = Expression::MemberAccess(Rc::new(MemberAccess {
        id,
        location: location!(member_access),
        base,
        member_name: MemberAccess::member_name_from_syn_item(member_access),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_assign_expresison(
    codebase: &mut Codebase<OpenState>,
    assign: &syn::ExprAssign,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let left = codebase.build_expression(&assign.left, id);
    let right = codebase.build_expression(&assign.right, id);
    let expr = Expression::Assign(Rc::new(Assign {
        id,
        location: location!(assign),
        left,
        right,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_binary_expression(
    codebase: &mut Codebase<OpenState>,
    binary: &syn::ExprBinary,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let left = codebase.build_expression(&binary.left, id);
    let right = codebase.build_expression(&binary.right, id);
    let binex = Rc::new(BinEx {
        id,
        location: location!(binary),
        left,
        right,
    });
    let expr = Expression::Binary(Binary::from_syn_item(binex, &binary.op));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_unary_expression(
    codebase: &mut Codebase<OpenState>,
    unary: &syn::ExprUnary,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let inner = codebase.build_expression(&unary.expr, id);
    let uex = Rc::new(UnEx {
        id,
        location: location!(unary),
        expression: inner,
    });
    let expr = Expression::Unary(Unary::from_syn_item(uex, &unary.op));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_break_expression(
    codebase: &mut Codebase<OpenState>,
    expr_break: &syn::ExprBreak,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expr = if let Some(inner_expr) = &expr_break.expr {
        let expression = Some(codebase.build_expression(inner_expr, id));
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
    };
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_closure_expression(
    codebase: &mut Codebase<OpenState>,
    expr_closure: &syn::ExprClosure,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let body = codebase.build_expression(expr_closure.body.as_ref(), id);
    let captures = &expr_closure
        .capture
        .iter()
        .map(|capture| {
            let name = capture.span.source_text().unwrap();
            let identifier = Rc::new(Identifier {
                id: get_node_id(),
                location: location!(capture),
                name,
            });
            codebase.add_node(
                NodeKind::Statement(Statement::Expression(Expression::Identifier(
                    identifier.clone(),
                ))),
                id,
            );
            identifier
        })
        .collect::<Vec<_>>();
    let returns = Type::Typedef(expr_closure.output.clone().into_token_stream().to_string());
    codebase.add_node(NodeKind::Statement(Statement::Expression(body.clone())), id);
    let closure = Expression::Closure(Rc::new(Closure {
        id,
        location: location!(expr_closure),
        captures: captures.clone(),
        returns,
        body,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(closure.clone())),
        parent_id,
    );
    closure
}

pub(crate) fn build_cast_expression(
    codebase: &mut Codebase<OpenState>,
    expr_cast: &syn::ExprCast,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = codebase.build_expression(&expr_cast.expr, id);
    let expr = Expression::Cast(Rc::new(Cast {
        id,
        location: location!(expr_cast),
        base,
        target_type: Type::Typedef(expr_cast.ty.to_token_stream().to_string()),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_block_statement(
    codebase: &mut Codebase<OpenState>,
    block: &syn::Block,
    parent_id: u32,
) -> Statement {
    let id = get_node_id();
    let statements = block
        .stmts
        .iter()
        .map(|stmt| codebase.build_statement(stmt, id))
        .collect();
    let stmt = Statement::Block(Rc::new(Block {
        id,
        location: location!(block),
        statements,
    }));
    codebase.add_node(NodeKind::Statement(stmt.clone()), parent_id);
    stmt
}

pub(crate) fn build_block_expression(
    codebase: &mut Codebase<OpenState>,
    block_expr: &ExprBlock,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let stmt = build_block_statement(codebase, &block_expr.block, id);
    codebase.add_node(NodeKind::Statement(stmt.clone()), parent_id);
    let block = Expression::EBlock(Rc::new(EBlock {
        id,
        location: location!(block_expr),
        block: match stmt {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(block.clone())),
        parent_id,
    );
    block
}

pub(crate) fn build_eblock_expression(block: &Rc<Block>, id: u32) -> Expression {
    Expression::EBlock(Rc::new(EBlock {
        id,
        location: block.location.clone(),
        block: block.clone(),
    }))
}

pub(crate) fn build_const_block_expression(
    codebase: &mut Codebase<OpenState>,
    expr_const: &syn::ExprConst,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(codebase, &expr_const.block, parent_id);
    let const_block = Expression::Const(Rc::new(ConstBlock {
        id,
        location: block_statement.location().clone(),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(const_block.clone())),
        parent_id,
    );
    const_block
}

pub(crate) fn build_continue_expression(
    codebase: &mut Codebase<OpenState>,
    expr_continue: &syn::ExprContinue,
    parent_id: u32,
) -> Expression {
    let expr = Expression::Continue(Rc::new(Continue {
        id: get_node_id(),
        location: location!(expr_continue),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_for_loop_expression(
    codebase: &mut Codebase<OpenState>,
    for_loop: &syn::ExprForLoop,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(codebase, &for_loop.body, parent_id);
    let expression = codebase.build_expression(&for_loop.expr, id);
    let for_loop = Expression::ForLoop(Rc::new(ForLoop {
        id,
        location: location!(for_loop),
        expression,
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(for_loop.clone())),
        parent_id,
    );
    for_loop
}

pub(crate) fn build_if_expression(
    codebase: &mut Codebase<OpenState>,
    if_expr: &syn::ExprIf,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let then_block = build_block_statement(codebase, &if_expr.then_branch, parent_id);
    let condition = codebase.build_expression(&if_expr.cond, id);
    let else_branch = if let Some((_, else_expr)) = &if_expr.else_branch {
        Some(codebase.build_expression(else_expr, id))
    } else {
        None
    };
    let expr = Expression::If(Rc::new(If {
        id,
        location: location!(if_expr),
        condition,
        then_branch: match then_block {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
        else_branch,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_loop_expression(
    codebase: &mut Codebase<OpenState>,
    loop_expr: &syn::ExprLoop,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(codebase, &loop_expr.body, parent_id);
    let eloop = Loop {
        id,
        location: location!(loop_expr),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    };
    let expr = Expression::Loop(Rc::new(eloop));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_while_expression(
    codebase: &mut Codebase<OpenState>,
    expr_while: &syn::ExprWhile,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let label = expr_while
        .label
        .as_ref()
        .map(|label| label.to_token_stream().to_string());
    let block_statement = build_block_statement(codebase, &expr_while.body, parent_id);
    let condition = codebase.build_expression(&expr_while.cond, id);
    let expr = Expression::While(Rc::new(While {
        id,
        location: location!(expr_while),
        label: label.clone(),
        condition,
        block: match block_statement {
            Statement::Block(block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_discarded_identifier(
    codebase: &mut Codebase<OpenState>,
    expr: &syn::Expr,
    parent_id: u32,
) -> Expression {
    let identifier = Expression::Identifier(Rc::new(Identifier {
        id: get_node_id(),
        location: location!(expr),
        name: "_".to_string(),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(identifier.clone())),
        parent_id,
    );
    identifier
}

pub(crate) fn build_index_access_expression(
    codebase: &mut Codebase<OpenState>,
    index_access: &syn::ExprIndex,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = codebase.build_expression(&index_access.expr, id);
    let index = codebase.build_expression(&index_access.index, id);
    let expr = Expression::IndexAccess(Rc::new(IndexAccess {
        id,
        location: location!(index_access),
        base,
        index,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_let_guard_expression(
    codebase: &mut Codebase<OpenState>,
    let_guard: &syn::ExprLet,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let guard = build_pattern(&let_guard.pat);
    codebase.add_node(NodeKind::Pattern(guard.clone()), id);
    let value = codebase.build_expression(&let_guard.expr, id);
    let expr = Expression::LetGuard(Rc::new(LetGuard {
        id,
        location: location!(let_guard),
        guard,
        value,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_macro_expression(
    codebase: &mut Codebase<OpenState>,
    macro_expr: &syn::ExprMacro,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let name = macro_expr.mac.path.to_token_stream().to_string();
    let text = macro_expr.mac.tokens.clone().to_string();
    let macro_ = Expression::Macro(Rc::new(Macro {
        id,
        location: location!(macro_expr),
        name,
        text,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(macro_.clone())),
        parent_id,
    );
    macro_
}

pub(crate) fn build_macro_definition(
    codebase: &mut Codebase<OpenState>,
    macro_def: &syn::ItemMacro,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let name = macro_def.ident.to_token_stream().to_string();
    let text = macro_def.mac.tokens.clone().to_string();
    let def = Definition::Macro(Rc::new(Macro {
        id,
        location: location!(macro_def),
        name,
        text,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(def.clone())),
        parent_id,
    );
    def
}

pub(crate) fn build_match_arm(
    codebase: &mut Codebase<OpenState>,
    arm: &syn::Arm,
    id: u32,
) -> MatchArm {
    let pattern = build_pattern(&arm.pat);
    let expression = codebase.build_expression(&arm.body, id);
    MatchArm {
        pattern,
        expression,
    }
}

pub(crate) fn build_match_expression(
    codebase: &mut Codebase<OpenState>,
    match_expr: &syn::ExprMatch,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = codebase.build_expression(&match_expr.expr, id);
    let arms = match_expr
        .arms
        .iter()
        .map(|arm| build_match_arm(codebase, arm, id))
        .collect();
    let expr = Expression::Match(Rc::new(Match {
        id,
        location: location!(match_expr),
        expression,
        arms,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_unsafe_expression(
    codebase: &mut Codebase<OpenState>,
    unsafe_expr: &syn::ExprUnsafe,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(codebase, &unsafe_expr.block, parent_id);
    let expr = Expression::Unsafe(Rc::new(Unsafe {
        id,
        location: location!(unsafe_expr),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_tuple_expression(
    codebase: &mut Codebase<OpenState>,
    expr_tuple: &syn::ExprTuple,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let elements = expr_tuple
        .elems
        .iter()
        .map(|expr| codebase.build_expression(expr, id))
        .collect::<Vec<_>>();
    let expr = Expression::Tuple(Rc::new(Tuple {
        id,
        location: location!(expr_tuple),
        elements,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_struct_expression(
    codebase: &mut Codebase<OpenState>,
    expr_struct: &syn::ExprStruct,
    parent_id: u32,
) -> Expression {
    let name = expr_struct.path.segments.last().unwrap().ident.to_string();
    let fields = expr_struct
        .fields
        .iter()
        .map(|field| {
            let name = match field.member {
                syn::Member::Named(ref ident) => ident.to_string(),
                syn::Member::Unnamed(_) => String::new(),
            };
            let value = codebase.build_expression(&field.expr, parent_id);
            (name, value)
        })
        .collect::<Vec<_>>();
    let rest_dots = expr_struct
        .rest
        .as_ref()
        .map(|expr| codebase.build_expression(expr, parent_id));
    let estruct = Expression::EStruct(Rc::new(EStruct {
        id: get_node_id(),
        location: location!(expr_struct),
        name,
        fields,
        rest_dots,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(estruct.clone())),
        parent_id,
    );
    estruct
}

pub(crate) fn build_addr_expression(
    codebase: &mut Codebase<OpenState>,
    expr_raddr: &syn::ExprRawAddr,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = codebase.build_expression(&expr_raddr.expr, id);
    let mutability = match expr_raddr.mutability {
        PointerMutability::Const(_) => Mutability::Constant,
        PointerMutability::Mut(_) => Mutability::Mutable,
    };
    let addr = Expression::Addr(Rc::new(Addr {
        id,
        location: location!(expr_raddr),
        mutability,
        expression,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(addr.clone())),
        parent_id,
    );
    addr
}

pub(crate) fn build_try_expression(
    codebase: &mut Codebase<OpenState>,
    expr_try: &syn::ExprTry,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = codebase.build_expression(&expr_try.expr, id);
    let expr = Expression::Try(Rc::new(Try {
        id,
        location: location!(expr_try),
        expression,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_try_block_expression(
    codebase: &mut Codebase<OpenState>,
    expr_try_block: &syn::ExprTryBlock,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(codebase, &expr_try_block.block, parent_id);
    let expr = match block_statement {
        Statement::Block(block) => build_eblock_expression(&block, id),
        _ => panic!(
            "Expected a block statement but got {}",
            serde_json::to_string(&block_statement).unwrap()
        ),
    };
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_pattern(pat: &syn::Pat) -> Pattern {
    let id = get_node_id();
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

pub(crate) fn build_literal_expression(
    codebase: &mut Codebase<OpenState>,
    lit_expr: &syn::ExprLit,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let literal = build_literal(&lit_expr.lit);
    let expr = Expression::Literal(Rc::new(Lit {
        id,
        location: location!(lit_expr),
        value: literal,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_literal(lit: &syn::Lit) -> Literal {
    let id = get_node_id();
    let location = location!(lit);
    match lit {
        syn::Lit::Bool(lit_bool) => Literal::Bool(Rc::new(LBool {
            id,
            location,
            value: lit_bool.value,
        })),
        syn::Lit::Byte(lit_byte) => Literal::Byte(Rc::new(LByte {
            id,
            location,
            value: lit_byte.value(),
        })),
        syn::Lit::Char(lit_char) => Literal::Char(Rc::new(LChar {
            id,
            location,
            value: lit_char.value(),
        })),
        syn::Lit::Float(lit_float) => Literal::Float(Rc::new(LFloat {
            id,
            location,
            value: lit_float.base10_digits().parse().unwrap(),
        })),
        syn::Lit::Int(lit_int) => Literal::Int(Rc::new(LInt {
            id,
            location,
            value: lit_int.base10_digits().parse().unwrap(),
        })),
        syn::Lit::Str(lit_str) => Literal::String(Rc::new(LString {
            id,
            location,
            value: lit_str.value(),
        })),
        syn::Lit::ByteStr(lit_bstr) => Literal::BString(Rc::new(LBString {
            id,
            location,
            value: String::from_utf8(lit_bstr.value()).expect("Invalid UTF-8 sequence"),
        })),
        syn::Lit::CStr(lit_cstr) => Literal::CString(Rc::new(LCString {
            id,
            location,
            value: lit_cstr.value().to_string_lossy().into_owned(),
        })),
        _ => panic!("Unsupported literal type"),
    }
}

pub(crate) fn build_macro_statement(
    codebase: &mut Codebase<OpenState>,
    macro_stmt: &syn::StmtMacro,
    parent_id: u32,
) -> Statement {
    let id = get_node_id();
    let location = location!(macro_stmt);
    let macro_ = Statement::Macro(Rc::new(Macro {
        id,
        location,
        name: "macro".to_string(),
        text: quote::quote! {#macro_stmt}.to_string(),
    }));
    codebase.add_node(NodeKind::Statement(macro_.clone()), parent_id);
    macro_
}

pub(crate) fn build_let_statement(
    codebase: &mut Codebase<OpenState>,
    stmt_let: &syn::Local,
    parent_id: u32,
) -> Statement {
    let location = location!(stmt_let);
    let name = stmt_let.pat.to_token_stream().to_string();
    let pattern = build_pattern(&stmt_let.pat);
    let id = get_node_id();
    let mut initial_value = None;
    let mut initial_value_alternative = None;
    let ref_stmt = &stmt_let;
    if ref_stmt.init.is_some() {
        let expr = codebase.build_expression(&stmt_let.init.as_ref().unwrap().expr, id);
        initial_value = Some(expr.clone());
        codebase.add_node(NodeKind::Statement(Statement::Expression(expr)), id);

        if stmt_let.init.clone().unwrap().diverge.is_some() {
            let expr = codebase.build_expression(
                stmt_let
                    .init
                    .as_ref()
                    .unwrap()
                    .diverge
                    .as_ref()
                    .unwrap()
                    .1
                    .as_ref(),
                id,
            );
            initial_value_alternative = Some(expr.clone());
            codebase.add_node(NodeKind::Statement(Statement::Expression(expr)), id);
        }
    }

    let statement = Statement::Let(Rc::new(super::statement::Let {
        id,
        location,
        name,
        pattern,
        initial_value,
        initial_value_alternative,
    }));
    codebase.add_node(NodeKind::Statement(statement.clone()), parent_id);
    statement
}

pub(crate) fn build_const_definition(
    codebase: &mut Codebase<OpenState>,
    item_const: &syn::ItemConst,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item_const);
    let name = item_const.ident.to_string();
    let value = codebase.build_expression(&item_const.expr, id);
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(value.clone())),
        id,
    );
    let ty = Type::Typedef(
        item_const
            .ty
            .as_ref()
            .clone()
            .into_token_stream()
            .to_string(),
    );
    let visibility = Visibility::from_syn_visibility(&item_const.vis);

    let def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value: Some(value),
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(def.clone())),
        parent_id,
    );

    def
}

pub(crate) fn build_extern_crate_definition(
    codebase: &mut Codebase<OpenState>,
    stmt_extern_crate: &syn::ItemExternCrate,
    parent_id: u32,
) -> Definition {
    let def = Definition::ExternCrate(Rc::new(super::definition::ExternCrate {
        id: get_node_id(),
        location: location!(stmt_extern_crate),
        name: stmt_extern_crate.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&stmt_extern_crate.vis),
        alias: stmt_extern_crate
            .rename
            .as_ref()
            .map(|(_, ident)| ident.to_string()),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(def.clone())),
        parent_id,
    );
    def
}

pub(crate) fn build_function_from_item_fn(
    codebase: &mut Codebase<OpenState>,
    item_fn: &syn::ItemFn,
    parent_id: u32,
) -> Rc<Function> {
    let id = get_node_id();
    let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
    for arg in &item_fn.sig.inputs {
        match arg {
            syn::FnArg::Receiver(receiver) => {
                let rc_param = Rc::new(FnParameter {
                    id: get_node_id(),
                    name: "self".to_string(),
                    location: location!(receiver),
                    type_name: FnParameter::type_name_from_syn_item(&receiver.ty),
                    is_self: true,
                    is_mut: receiver.mutability.is_some(),
                });
                fn_parameters.push(rc_param.clone());
                codebase.add_node(NodeKind::FnParameter(rc_param), id);
            }
            syn::FnArg::Typed(pat_type) => {
                if let syn::Pat::Ident(pat_ident) = &*pat_type.pat {
                    let name = pat_ident.ident.to_string();
                    let is_self = name == "self";
                    let arg_type = &pat_type.ty;
                    let rc_param = Rc::new(FnParameter {
                        id: get_node_id(),
                        name,
                        location: location!(arg),
                        type_name: FnParameter::type_name_from_syn_item(arg_type),
                        is_self,
                        is_mut: pat_ident.mutability.is_some(),
                    });
                    fn_parameters.push(rc_param.clone());
                    codebase.add_node(NodeKind::FnParameter(rc_param), id);
                }
            }
        }
    }
    let mut returns: TypeNode = TypeNode::Empty;

    if let syn::ReturnType::Type(_, ty) = &item_fn.sig.output {
        returns = TypeNode::from_syn_item(&ty.clone());
    }
    let block_statement = build_block_statement(codebase, &item_fn.block, parent_id);
    let block = match block_statement.clone() {
        Statement::Block(block) => {
            codebase.add_node(NodeKind::Statement(Statement::Block(block.clone())), id);
            Some(block)
        }
        _ => None,
    };

    let generics = item_fn
        .sig
        .generics
        .params
        .iter()
        .map(|p| p.to_token_stream().to_string())
        .collect();
    let attributes = item_fn
        .attrs
        .iter()
        .map(|a| a.path().segments[0].ident.to_string())
        .collect();
    let function = Rc::new(Function {
        id: get_node_id(),
        attributes,
        location: location!(item_fn),
        name: item_fn.sig.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&item_fn.vis),
        generics,
        parameters: fn_parameters.clone(),
        returns: RefCell::new(returns),
        body: block,
    });
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Function(
            function.clone(),
        ))),
        parent_id,
    );
    function
}

pub(crate) fn build_function_from_impl_item_fn(
    codebase: &mut Codebase<OpenState>,
    item_fn: &syn::ImplItemFn,
    id: u32,
) -> Rc<Function> {
    build_function_from_item_fn(
        codebase,
        &ItemFn {
            attrs: item_fn.attrs.clone(),
            vis: item_fn.vis.clone(),
            sig: item_fn.sig.clone(),
            block: Box::new(item_fn.block.clone()),
        },
        id,
    )
}

pub(crate) fn build_type_alias_from_impl_item_type(
    codebase: &mut Codebase<OpenState>,
    item_type: &syn::ImplItemType,
    parent_id: u32,
) -> Rc<TypeAlias> {
    let id = get_node_id();
    let type_alias = Rc::new(TypeAlias {
        id,
        location: location!(item_type),
        name: item_type.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&item_type.vis),
        ty: Box::new(Type::Typedef(
            item_type.ty.clone().into_token_stream().to_string(),
        )),
    });
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::CustomType(Type::Alias(
            type_alias.clone(),
        )))),
        parent_id,
    );
    type_alias
}

pub(crate) fn build_static_definition(
    codebase: &mut Codebase<OpenState>,
    item_static: &syn::ItemStatic,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_static.attrs);
    let location = location!(item_static);
    let name = item_static.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_static.vis);
    let mutable = matches!(item_static.mutability, syn::StaticMutability::Mut(_));
    let ty = Type::Typedef(item_static.ty.to_token_stream().to_string());
    let expr = codebase.build_expression(&item_static.expr, id);

    let static_def = Definition::Static(Rc::new(Static {
        id,
        attributes,
        location,
        name,
        visibility,
        mutable,
        ty,
        value: expr,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(static_def.clone())),
        parent_id,
    );
    static_def
}

pub(crate) fn build_mod_definition(
    codebase: &mut Codebase<OpenState>,
    item_mod: &syn::ItemMod,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_mod.attrs);
    let location = location!(item_mod);
    let name = item_mod.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_mod.vis);
    let definitions = item_mod.content.as_ref().map(|(_, items)| {
        items
            .iter()
            .map(|item| codebase.build_definition(item, id))
            .collect::<Vec<_>>()
    });

    let mod_def = Definition::Module(Rc::new(Module {
        id,
        location,
        attributes,
        name,
        visibility,
        definitions,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(mod_def.clone())),
        parent_id,
    );
    mod_def
}

pub(crate) fn build_plane_definition(
    codebase: &mut Codebase<OpenState>,
    token_stream: &proc_macro2::TokenStream,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(token_stream);
    let value = token_stream.to_string();
    let plane_def = Definition::Plane(Rc::new(Plane {
        id,
        location,
        value,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(plane_def.clone())),
        parent_id,
    );
    plane_def
}

pub(crate) fn build_use_directive(
    codebase: &mut Codebase<OpenState>,
    use_directive: &syn::ItemUse,
    parent_id: u32,
) -> Directive {
    let directive = Directive::Use(Rc::new(Use {
        id: get_node_id(),
        location: location!(use_directive),
        visibility: Visibility::from_syn_visibility(&use_directive.vis),
        path: use_directive.tree.to_token_stream().to_string(),
        target: std::cell::RefCell::new(None),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Directive(
            directive.clone(),
        ))),
        parent_id,
    );
    directive
}

pub(crate) fn build_type_definition(
    codebase: &mut Codebase<OpenState>,
    item_type: &syn::ItemType,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item_type);
    let name = item_type.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_type.vis);
    let ty = item_type.ty.to_token_stream().to_string();
    let attributes = extract_attrs(&item_type.attrs);

    let type_def = Definition::Type(Rc::new(Typedef {
        id,
        location,
        attributes,
        name,
        visibility,
        ty,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(type_def.clone())),
        parent_id,
    );
    type_def
}

pub(crate) fn build_field(
    codebase: &mut Codebase<OpenState>,
    field: &syn::Field,
    parent_id: u32,
) -> Rc<Field> {
    let id = get_node_id();
    let location = location!(field);
    let name = field.ident.as_ref().map(std::string::ToString::to_string);
    let visibility = Visibility::from_syn_visibility(&field.vis);
    let mutability = match field.mutability {
        syn::FieldMutability::None => Mutability::Constant,
        _ => Mutability::Mutable,
    };
    let ty = Type::Typedef(field.ty.to_token_stream().to_string());
    let field = Rc::new(Field {
        id,
        location,
        name,
        visibility,
        mutability,
        ty,
    });
    codebase.add_node(NodeKind::Misc(Misc::Field(field.clone())), parent_id);
    field
}

pub(crate) fn build_union_definition(
    codebase: &mut Codebase<OpenState>,
    item_union: &syn::ItemUnion,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_union.attrs);
    let location = location!(item_union);
    let name = item_union.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_union.vis);
    let fields = item_union
        .fields
        .named
        .iter()
        .map(|f| build_field(codebase, f, id))
        .collect();

    let union_def = Definition::Union(Rc::new(super::definition::Union {
        id,
        location,
        attributes,
        name,
        visibility,
        fields,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(union_def.clone())),
        parent_id,
    );
    union_def
}

pub(crate) fn build_const_definition_for_impl_item_const(
    codebase: &mut Codebase<OpenState>,
    item: &syn::ImplItemConst,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item.vis);

    let ty = Type::Typedef(item.ty.to_token_stream().to_string());
    let value = codebase.build_expression(&item.expr, id);

    let constant_def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value: Some(value),
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(constant_def.clone())),
        parent_id,
    );

    constant_def
}

fn build_const_definition_from_trait_item(
    codebase: &mut Codebase<OpenState>,
    item: &syn::TraitItemConst,
    visibility: Visibility,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.ident.to_string();
    let ty = Type::Typedef(item.ty.to_token_stream().to_string());
    let value = item
        .default
        .as_ref()
        .map(|expr| codebase.build_expression(&expr.1, id));

    let constant_def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(constant_def.clone())),
        parent_id,
    );

    constant_def
}

fn build_function_definition_for_trait_item_fn(
    codebase: &mut Codebase<OpenState>,
    item: &syn::TraitItemFn,
    visibility: Visibility,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let name = item.sig.ident.to_string();
    let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
    for arg in &item.sig.inputs {
        match arg {
            syn::FnArg::Receiver(receiver) => {
                let rc_param = Rc::new(FnParameter {
                    id: get_node_id(),
                    name: "self".to_string(),
                    location: location!(receiver),
                    type_name: FnParameter::type_name_from_syn_item(&receiver.ty),
                    is_self: true,
                    is_mut: receiver.mutability.is_some(),
                });
                fn_parameters.push(rc_param.clone());
                codebase.add_node(NodeKind::FnParameter(rc_param), id);
            }
            syn::FnArg::Typed(pat_type) => {
                if let syn::Pat::Ident(pat_ident) = &*pat_type.pat {
                    let name = pat_ident.ident.to_string();
                    let is_self = name == "self";
                    let arg_type = &pat_type.ty;
                    let rc_param = Rc::new(FnParameter {
                        id: get_node_id(),
                        name,
                        location: location!(arg),
                        type_name: FnParameter::type_name_from_syn_item(arg_type),
                        is_self,
                        is_mut: pat_ident.mutability.is_some(),
                    });
                    fn_parameters.push(rc_param.clone());
                    codebase.add_node(NodeKind::FnParameter(rc_param), id);
                }
            }
        }
    }
    let mut returns: TypeNode = TypeNode::Empty;

    if let syn::ReturnType::Type(_, ty) = &item.sig.output {
        returns = TypeNode::from_syn_item(&ty.clone());
    }

    let generics = item
        .sig
        .generics
        .params
        .iter()
        .map(|p| p.to_token_stream().to_string())
        .collect();
    let attributes = item
        .attrs
        .iter()
        .map(|a| a.path().segments[0].ident.to_string())
        .collect();
    let function = Rc::new(Function {
        id: get_node_id(),
        attributes,
        location: location!(item),
        name,
        visibility,
        generics,
        parameters: fn_parameters.clone(),
        returns: RefCell::new(returns),
        body: None,
    });
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Function(
            function.clone(),
        ))),
        parent_id,
    );
    Definition::Function(function)
}

fn build_type_alias_definition_for_trait_item_type(
    codebase: &mut Codebase<OpenState>,
    item: &syn::TraitItemType,
    visibility: Visibility,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.ident.to_string();
    let type_alias = TypeAlias {
        id,
        location,
        name: name.clone(),
        visibility,
        ty: Box::new(Type::Typedef(name)),
    };
    let def = Definition::CustomType(Type::Alias(Rc::new(type_alias)));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(def.clone())),
        parent_id,
    );
    def
}

fn build_macro_definition_for_trait_item_macro(
    codebase: &mut Codebase<OpenState>,
    item: &syn::TraitItemMacro,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.mac.path.to_token_stream().to_string();
    let text = item.mac.tokens.clone().to_string();
    let macro_ = Definition::Macro(Rc::new(Macro {
        id,
        location,
        name,
        text,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(macro_.clone())),
        parent_id,
    );
    macro_
}

pub(crate) fn build_trait_definition(
    codebase: &mut Codebase<OpenState>,
    item_trait: &syn::ItemTrait,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_trait.attrs);
    let location = location!(item_trait);
    let name = item_trait.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_trait.vis);
    let supertraits = item_trait.supertraits.to_token_stream().to_string();
    let items = item_trait
        .items
        .iter()
        .map(|item| match item {
            syn::TraitItem::Const(item) => {
                build_const_definition_from_trait_item(codebase, item, visibility.clone(), id)
            }
            syn::TraitItem::Fn(item) => {
                build_function_definition_for_trait_item_fn(codebase, item, visibility.clone(), id)
            }
            syn::TraitItem::Type(item) => build_type_alias_definition_for_trait_item_type(
                codebase,
                item,
                visibility.clone(),
                id,
            ),
            syn::TraitItem::Macro(item) => {
                build_macro_definition_for_trait_item_macro(codebase, item, id)
            }
            syn::TraitItem::Verbatim(token_stream) => {
                build_plane_definition(codebase, token_stream, id)
            }
            _ => todo!(),
        })
        .collect();

    let trait_def = Definition::Trait(Rc::new(Trait {
        id,
        location,
        attributes,
        name,
        visibility,
        supertraits,
        items,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(trait_def.clone())),
        parent_id,
    );
    trait_def
}

pub(crate) fn build_trait_alias_definition(
    codebase: &mut Codebase<OpenState>,
    item_trait_alias: &syn::ItemTraitAlias,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_trait_alias.attrs);
    let location = location!(item_trait_alias);
    let name = item_trait_alias.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_trait_alias.vis);
    let bounds = item_trait_alias.bounds.to_token_stream().to_string();

    let trait_alias_def = Definition::TraitAlias(Rc::new(super::definition::TraitAlias {
        id,
        location,
        attributes,
        name,
        visibility,
        bounds,
    }));

    codebase.add_node(
        NodeKind::Statement(Statement::Definition(trait_alias_def.clone())),
        parent_id,
    );
    trait_alias_def
}

pub(crate) fn build_marco_definition_for_impl_item_macro(
    codebase: &mut Codebase<OpenState>,
    item: &syn::ImplItemMacro,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.mac.path.to_token_stream().to_string();
    let text = item.mac.tokens.clone().to_string();
    let macro_ = Definition::Macro(Rc::new(Macro {
        id,
        location,
        name,
        text,
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(macro_.clone())),
        parent_id,
    );
    macro_
}
