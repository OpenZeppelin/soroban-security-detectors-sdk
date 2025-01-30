#![warn(clippy::pedantic)]

use std::{cell::RefCell, rc::Rc};

use quote::ToTokens;
use syn::{ExprBlock, ItemEnum, ItemFn, ItemStruct, PointerMutability};
use uuid::Uuid;

use crate::{
    expression::{
        Addr, Closure, Continue, EStruct, Lit, Loop, Parenthesized, Range, Repeat, Return, Try,
        Tuple, While,
    },
    location,
    node::Mutability,
    node_type::ContractType,
    Codebase, OpenState,
};

use super::{
    contract::Struct,
    custom_type::Type,
    definition::{Const, Definition, Enum},
    directive::{Directive, Use},
    expression::{
        Array, Assign, BinEx, Binary, Break, Cast, ConstBlock, EBlock, Expression, ForLoop,
        FunctionCall, Identifier, If, IndexAccess, LetGuard, Macro, Match, MatchArm, MemberAccess,
        MethodCall, Reference, UnEx, Unary, Unsafe,
    },
    function::{FnParameter, Function},
    literal::{LBString, LBool, LByte, LCString, LChar, LFloat, LInt, LString, Literal},
    node::Visibility,
    node_type::{NodeKind, TypeNode},
    pattern::Pattern,
    statement::{Block, Statement},
};

pub(crate) fn build_struct(
    codebase: &mut Codebase<OpenState>,
    struct_item: &ItemStruct,
    parent_id: u128,
) -> Rc<Struct> {
    let mut fields = Vec::new();
    for field in &struct_item.fields {
        let field_name = match &field.ident {
            Some(ident) => ident.to_string(),
            None => "unnamed".to_string(),
        };
        let field_type = Type::T(field.ty.to_token_stream().to_string());
        fields.push((field_name, field_type));
    }
    let rc_struct = Rc::new(Struct {
        id: Uuid::new_v4().as_u128(),
        location: location!(struct_item),
        name: Struct::contract_name_from_syn_item(struct_item),
        fields,
        methods: RefCell::new(Vec::new()),
        functions: RefCell::new(Vec::new()),
    });
    if Struct::is_struct_contract(struct_item) {
        let contract: ContractType = ContractType::Contract(rc_struct.clone());
        codebase.add_node(NodeKind::Contract(contract), parent_id);
    } else if Struct::is_struct_contract_type(struct_item) {
        let contract_type = ContractType::Struct(rc_struct.clone());
        codebase.add_node(NodeKind::Contract(contract_type), parent_id);
    } else {
        codebase.add_node(
            NodeKind::Statement(Statement::Definition(Definition::Struct(rc_struct.clone()))),
            parent_id,
        );
    }
    rc_struct
}

pub(crate) fn build_enum(
    codebase: &mut Codebase<OpenState>,
    enum_item: &ItemEnum,
    parent_id: u128,
) -> Rc<Enum> {
    let mut variants = Vec::new();
    for variant in &enum_item.variants {
        let variant_name = variant.ident.to_string();
        variants.push(variant_name);
    }
    let rc_enum = Rc::new(Enum {
        id: Uuid::new_v4().as_u128(),
        location: location!(enum_item),
        name: enum_item.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&enum_item.vis),
        variants,
        methods: RefCell::new(Vec::new()),
        functions: RefCell::new(Vec::new()),
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let parameters = expr_call
        .args
        .iter()
        .map(|arg| codebase.build_expression(arg, id))
        .collect();
    let expr = Expression::FunctionCall(Rc::new(FunctionCall {
        id: Uuid::new_v4().as_u128(),
        location: location!(expr_call),
        function_name: FunctionCall::function_name_from_syn_item(expr_call),
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
    parent_id: u128,
) -> Expression {
    let method_call_id = Uuid::new_v4().as_u128();
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

pub(crate) fn build_reference_expression(
    codebase: &mut Codebase<OpenState>,
    expr_ref: &syn::ExprReference,
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let expression = match &expr_return.expr {
        Some(expr) => Some(codebase.build_expression(expr, id)),
        None => None,
    };
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let expr = Expression::Identifier(Rc::new(Identifier {
        id: Uuid::new_v4().as_u128(),
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let body = codebase.build_expression(expr_closure.body.as_ref(), id);
    let captures = &expr_closure
        .capture
        .iter()
        .map(|capture| {
            let name = capture.span.source_text().unwrap();
            let identifier = Rc::new(Identifier {
                id: Uuid::new_v4().as_u128(),
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
    let returns = Type::T(expr_closure.output.clone().into_token_stream().to_string());
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let base = codebase.build_expression(&expr_cast.expr, id);
    let expr = Expression::Cast(Rc::new(Cast {
        id,
        location: location!(expr_cast),
        base,
        target_type: Type::T(expr_cast.ty.to_token_stream().to_string()),
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
    parent_id: u128,
) -> Statement {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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

pub(crate) fn build_eblock_expression(block: &Rc<Block>, id: u128) -> Expression {
    Expression::EBlock(Rc::new(EBlock {
        id,
        location: block.location.clone(),
        block: block.clone(),
    }))
}

pub(crate) fn build_const_block_expression(
    codebase: &mut Codebase<OpenState>,
    expr_const: &syn::ExprConst,
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let expr = Expression::Continue(Rc::new(Continue {
        id: Uuid::new_v4().as_u128(),
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let identifier = Expression::Identifier(Rc::new(Identifier {
        id: Uuid::new_v4().as_u128(),
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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

pub(crate) fn build_match_arm(
    codebase: &mut Codebase<OpenState>,
    arm: &syn::Arm,
    id: u128,
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
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
        id: Uuid::new_v4().as_u128(),
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
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

//TODO if a deeper analysis of patterns is needed, this function and its callee should be updated
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

pub(crate) fn build_literal_expression(
    codebase: &mut Codebase<OpenState>,
    lit_expr: &syn::ExprLit,
    parent_id: u128,
) -> Expression {
    let id = Uuid::new_v4().as_u128();
    let literal = build_literal(&lit_expr.lit);
    let expr = Expression::Lit(Rc::new(Lit {
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
    let id = Uuid::new_v4().as_u128();
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
    parent_id: u128,
) -> Statement {
    let id = Uuid::new_v4().as_u128();
    let location = location!(macro_stmt);
    let macro_ = Statement::Macro(Rc::new(super::statement::Macro {
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
    parent_id: u128,
) -> Statement {
    let location = location!(stmt_let);
    let name = stmt_let.pat.to_token_stream().to_string();
    let pattern = build_pattern(&stmt_let.pat);
    let id = Uuid::new_v4().as_u128();
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
    stmt_const: &syn::ItemConst,
    parent_id: u128,
) -> Definition {
    let id = Uuid::new_v4().as_u128();
    let location = location!(stmt_const);
    let name = stmt_const.ident.to_string();
    let value = codebase.build_expression(&stmt_const.expr, id);
    codebase.add_node(
        NodeKind::Statement(Statement::Expression(value.clone())),
        id,
    );
    let ty = Type::T(
        stmt_const
            .ty
            .as_ref()
            .clone()
            .into_token_stream()
            .to_string(),
    );
    let visibility = Visibility::from_syn_visibility(&stmt_const.vis);

    let def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value,
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
    parent_id: u128,
) -> Definition {
    let def = Definition::ExternCrate(Rc::new(super::definition::ExternCrate {
        id: Uuid::new_v4().as_u128(),
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
    parent_id: u128,
) -> Rc<Function> {
    let id = Uuid::new_v4().as_u128();
    let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
    for arg in &item_fn.sig.inputs {
        if let syn::FnArg::Typed(type_) = arg {
            if let syn::Pat::Ident(pat_ident) = &*type_.pat {
                let name = pat_ident.ident.to_string();
                let is_self = name == "self";
                let arg_type = *(type_.ty.clone());
                let rc_param = Rc::new(FnParameter {
                    id: Uuid::new_v4().as_u128(),
                    name,
                    location: location!(arg_type),
                    type_name: FnParameter::type_name_from_syn_item(&arg_type),
                    is_self,
                });
                fn_parameters.push(rc_param.clone());
                codebase.add_node(NodeKind::FnParameter(rc_param), id);
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

    let function = Rc::new(Function {
        id: Uuid::new_v4().as_u128(),
        location: location!(item_fn),
        name: item_fn.sig.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&item_fn.vis),
        parameters: fn_parameters.clone(),
        returns,
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
    id: u128,
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

pub(crate) fn build_use_directive(
    codebase: &mut Codebase<OpenState>,
    use_directive: &syn::ItemUse,
    parent_id: u128,
) -> Directive {
    let directive = Directive::Use(Rc::new(Use {
        id: Uuid::new_v4().as_u128(),
        location: location!(use_directive),
        visibility: Visibility::from_syn_visibility(&use_directive.vis),
        path: use_directive.tree.to_token_stream().to_string(),
    }));
    codebase.add_node(
        NodeKind::Statement(Statement::Definition(Definition::Directive(
            directive.clone(),
        ))),
        parent_id,
    );
    directive
}
