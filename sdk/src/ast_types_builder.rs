use std::path::Path;
use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use quote::ToTokens;
use syn::{Attribute, ExprBlock, ItemEnum, ItemFn, ItemStruct, PointerMutability};
use uuid::Uuid;

use crate::ast::custom_type::{Type, TypeAlias, Typename};
use crate::ast::definition::{Module, Plane, Static, Trait};
use crate::ast::expression::{
    Addr, Array, Assign, Binary, Break, Cast, Closure, ConstBlock, Continue, EBlock, EStruct,
    ForLoop, FunctionCall, Identifier, If, IndexAccess, LetGuard, Lit, Loop, Match, MatchArm,
    MemberAccess, MethodCall, Parenthesized, Range, Reference, Repeat, Return, Try, Tuple, Unary,
    Unsafe, While,
};
use crate::ast::misc::{Field, Macro, Misc};
use crate::ast::node::Mutability;
use crate::definition::Implementation;
use crate::expression::{BinOp, UnOp};
use crate::file::File;
use crate::{location, source_code, NodesStorage};

use crate::ast::contract::Struct;
use crate::ast::definition::{Const, Definition, Enum};
use crate::ast::directive::{Directive, Use};
use crate::ast::expression::Expression;
use crate::ast::function::{FnParameter, Function};
use crate::ast::literal::{
    LBString, LBool, LByte, LCString, LChar, LFloat, LInt, LString, Literal,
};
use crate::ast::node::Visibility;
use crate::ast::node_type::NodeKind;
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

#[allow(clippy::cast_possible_truncation)]
pub(crate) fn build_file(
    storage: &mut NodesStorage,
    file_path: String,
    file_item: syn::File,
) -> Rc<File> {
    let mut file_name = String::new();
    let path = Path::new(&file_path);
    if let Some(filename) = path.file_name() {
        file_name = filename.to_string_lossy().to_string();
    }
    let rc_file = Rc::new(File {
        id: Uuid::new_v4().as_u128() as u32,
        children: RefCell::new(Vec::new()),
        name: file_name.clone(),
        path: file_path,
        attributes: File::attributes_from_file_item(&file_item),
        source_code: source_code!(file_item),
        location: location!(file_item),
    });
    let file_mod = rc_file.file_module_name();
    let file_node = NodeKind::File(rc_file.clone());
    storage.add_node(file_node, 0);
    for item in file_item.items {
        if let syn::Item::Use(item_use) = &item {
            let directive = build_use_directive(storage, item_use, rc_file.id, &file_mod);
            rc_file
                .children
                .borrow_mut()
                .push(NodeKind::Directive(directive));
        } else {
            let definition = build_definition(storage, &item, rc_file.id);
            rc_file
                .children
                .borrow_mut()
                .push(NodeKind::Definition(definition));
        }
    }
    rc_file
}

#[allow(unused_variables, clippy::too_many_lines)]
pub(crate) fn build_definition(
    storage: &mut NodesStorage,
    item: &syn::Item,
    parent_id: u32,
) -> Definition {
    match item {
        syn::Item::Const(item_const) => build_const_definition(storage, item_const, parent_id),
        syn::Item::Enum(item_enum) => Definition::Enum(build_enum(storage, item_enum, parent_id)),
        syn::Item::ExternCrate(item_extern_crate) => {
            build_extern_crate_definition(storage, item_extern_crate, parent_id)
        }
        syn::Item::Fn(item_fn) => {
            Definition::Function(build_function_from_item_fn(storage, item_fn, parent_id))
        }
        syn::Item::ForeignMod(_) => todo!("Should not appear"),
        syn::Item::Impl(item_impl) => process_item_impl(storage, item_impl, parent_id),
        syn::Item::Macro(item_macro) => build_macro_definition(storage, item_macro, parent_id),
        syn::Item::Mod(item_mod) => build_mod_definition(storage, item_mod, parent_id),
        syn::Item::Static(item_static) => build_static_definition(storage, item_static, parent_id),
        syn::Item::Struct(item_struct) => {
            Definition::Struct(build_struct(storage, item_struct, parent_id))
        }
        syn::Item::Trait(item_trait) => build_trait_definition(storage, item_trait, parent_id),
        syn::Item::TraitAlias(item_trait_alias) => {
            build_trait_alias_definition(storage, item_trait_alias, parent_id)
        }
        syn::Item::Type(item_type) => build_type_alias_definition(storage, item_type, parent_id),
        syn::Item::Union(item_union) => build_union_definition(storage, item_union, parent_id),
        syn::Item::Verbatim(token_stream) => {
            build_plane_definition(storage, token_stream, parent_id)
        }
        _ => todo!("Unsupported item type: {}", item.into_token_stream()),
    }
}

pub(crate) fn build_statement(
    storage: &mut NodesStorage,
    stmt: &syn::Stmt,
    parent_id: u32,
) -> Statement {
    match stmt {
        syn::Stmt::Expr(stmt_expr, _) => {
            Statement::Expression(build_expression(storage, stmt_expr, parent_id))
        }
        syn::Stmt::Local(stmt_let) => build_let_statement(storage, stmt_let, parent_id),
        syn::Stmt::Macro(stmt_macro) => build_macro_statement(storage, stmt_macro, parent_id),
        syn::Stmt::Item(stmt_item) => {
            //TODO: handle it separately in case here `use` directive is present
            Statement::Definition(build_definition(storage, stmt_item, parent_id))
        }
    }
}

#[allow(clippy::too_many_lines, clippy::match_wildcard_for_single_variants)]
pub(crate) fn build_expression(
    storage: &mut NodesStorage,
    expr: &syn::Expr,
    parent_id: u32,
) -> Expression {
    match expr {
        syn::Expr::Array(array_expr) => build_array_expression(storage, array_expr, parent_id),
        syn::Expr::Assign(assign_expr) => build_assign_expresison(storage, assign_expr, parent_id),
        syn::Expr::Async(_) => {
            panic!("async expressions are not supported");
        }
        syn::Expr::Await(_) => {
            panic!("await expressions are not supported");
        }
        syn::Expr::Binary(expr_binary) => build_binary_expression(storage, expr_binary, parent_id),
        syn::Expr::Unary(expr_unary) => build_unary_expression(storage, expr_unary, parent_id),
        syn::Expr::Break(expr_break) => build_break_expression(storage, expr_break, parent_id),
        syn::Expr::Block(block_expr) => build_block_expression(storage, block_expr, parent_id),
        syn::Expr::Call(expr_call) => build_function_call_expression(storage, expr_call, parent_id),
        syn::Expr::Cast(expr_cast) => build_cast_expression(storage, expr_cast, parent_id),
        syn::Expr::Closure(expr_closure) => {
            build_closure_expression(storage, expr_closure, parent_id)
        }
        syn::Expr::Const(expr_const) => {
            build_const_block_expression(storage, expr_const, parent_id)
        }
        syn::Expr::Continue(expr_continue) => {
            build_continue_expression(storage, expr_continue, parent_id)
        }
        syn::Expr::ForLoop(expr_forloop) => {
            build_for_loop_expression(storage, expr_forloop, parent_id)
        }
        syn::Expr::Field(field_expr) => {
            build_member_access_expression(storage, field_expr, parent_id)
        }
        syn::Expr::If(expr_if) => build_if_expression(storage, expr_if, parent_id),
        syn::Expr::Index(expr_index) => {
            build_index_access_expression(storage, expr_index, parent_id)
        }
        syn::Expr::Infer(_) => build_discarded_identifier(storage, expr, parent_id),
        syn::Expr::Let(expr_let) => build_let_guard_expression(storage, expr_let, parent_id),
        syn::Expr::Lit(expr_lit) => build_literal_expression(storage, expr_lit, parent_id),
        syn::Expr::Loop(expr_loop) => build_loop_expression(storage, expr_loop, parent_id),
        syn::Expr::Macro(expr_macro) => build_macro_expression(storage, expr_macro, parent_id),
        syn::Expr::Match(expr_match) => build_match_expression(storage, expr_match, parent_id),
        syn::Expr::MethodCall(method_call) => {
            build_method_call_expression(storage, method_call, parent_id)
        }
        syn::Expr::Paren(expr_paren) => {
            build_parenthesied_expression(storage, expr_paren, parent_id)
        }
        syn::Expr::Path(expr_path) => build_identifier(storage, expr_path, parent_id),
        syn::Expr::Range(expr_range) => build_range_expression(storage, expr_range, parent_id),
        syn::Expr::RawAddr(expr_raddr) => build_addr_expression(storage, expr_raddr, parent_id),
        syn::Expr::Reference(expr_ref) => build_reference_expression(storage, expr_ref, parent_id),
        syn::Expr::Repeat(expr_repeat) => build_repeat_expression(storage, expr_repeat, parent_id),
        syn::Expr::Return(expr_return) => build_return_expression(storage, expr_return, parent_id),
        syn::Expr::Yield(_) => panic!("yield expressions are not supported"),
        syn::Expr::Struct(expr_struct) => build_struct_expression(storage, expr_struct, parent_id),
        syn::Expr::Try(expr_try) => build_try_expression(storage, expr_try, parent_id),
        syn::Expr::TryBlock(expr_try_block) => {
            build_try_block_expression(storage, expr_try_block, parent_id)
        }
        syn::Expr::Tuple(expr_tuple) => build_tuple_expression(storage, expr_tuple, parent_id),
        syn::Expr::Unsafe(expr_unsafe) => build_unsafe_expression(storage, expr_unsafe, parent_id),
        syn::Expr::While(expr_while) => build_while_expression(storage, expr_while, parent_id),
        _ => panic!("Unsupported expression type {}", expr.into_token_stream()),
    }
}

pub(crate) fn build_struct(
    storage: &mut NodesStorage,
    struct_item: &ItemStruct,
    parent_id: u32,
) -> Rc<Struct> {
    let attributes = extract_attrs(&struct_item.attrs);
    let mut fields = Vec::new();
    let id = get_node_id();
    for field in &struct_item.fields {
        let field_name = match &field.ident {
            Some(ident) => ident.to_string(),
            None => "unnamed".to_string(),
        };
        let field_type = build_type(storage, &field.ty, id);
        fields.push((field_name, field_type));
    }
    let rc_struct = Rc::new(Struct {
        id,
        attributes,
        location: location!(struct_item),
        name: Struct::contract_name_from_syn_item(struct_item),
        fields,
        is_contract: Struct::is_struct_contract(struct_item),
    });
    storage.add_node(
        NodeKind::Definition(Definition::Struct(rc_struct.clone())),
        parent_id,
    );
    rc_struct
}

pub(crate) fn build_type(_storage: &mut NodesStorage, ty: &syn::Type, _parent_id: u32) -> Type {
    let id = get_node_id();
    let location = location!(ty);
    let name = ty.to_token_stream().to_string();
    let node = Rc::new(Typename { id, location, name });
    Type::Typename(node)
}

pub(crate) fn build_enum(
    storage: &mut NodesStorage,
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
    storage.add_node(
        NodeKind::Definition(Definition::Enum(rc_enum.clone())),
        parent_id,
    );
    rc_enum
}

pub(crate) fn build_array_expression(
    storage: &mut NodesStorage,
    array_expr: &syn::ExprArray,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let elements = array_expr
        .elems
        .iter()
        .map(|elem| build_expression(storage, elem, id))
        .collect();
    let expr = Expression::Array(Rc::new(Array {
        id,
        location: location!(array_expr),
        elements,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_function_call_expression(
    storage: &mut NodesStorage,
    expr_call: &syn::ExprCall,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let parameters = expr_call
        .args
        .iter()
        .map(|arg| build_expression(storage, arg, id))
        .collect();
    let expr = Expression::FunctionCall(Rc::new(FunctionCall {
        id,
        location: location!(expr_call),
        function_name: FunctionCall::function_name_from_syn_item(expr_call),
        expression: build_expression(storage, &expr_call.func, id),
        parameters,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_method_call_expression(
    storage: &mut NodesStorage,
    method_call: &syn::ExprMethodCall,
    parent_id: u32,
) -> Expression {
    let method_call_id = get_node_id();
    let base = build_expression(storage, &method_call.receiver, method_call_id);
    let expr = Expression::MethodCall(Rc::new(MethodCall {
        id: method_call_id,
        location: location!(method_call),
        method_name: MethodCall::method_name_from_syn_item(method_call),
        base,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

// fn get_impl_type_name(item_impl: &syn::ItemImpl) -> Option<String> {
//     if let syn::Type::Path(type_path) = &*item_impl.self_ty {
//         if let Some(segment) = type_path.path.segments.last() {
//             return Some(segment.ident.to_string());
//         }
//     }
//     None
// }

pub(crate) fn process_item_impl(
    storage: &mut NodesStorage,
    item_impl: &syn::ItemImpl,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_impl.attrs);
    let for_type: Type = build_type(storage, item_impl.self_ty.as_ref(), id);
    let mut functions = Vec::new();
    let mut constants = Vec::new();
    let mut types = Vec::new();
    let mut macroses = Vec::new();
    let mut planes = Vec::new();

    for item in &item_impl.items {
        match item {
            syn::ImplItem::Fn(assoc_fn) => {
                let function = build_function_from_impl_item_fn(
                    storage,
                    assoc_fn,
                    item_impl.self_ty.as_ref(),
                    id,
                );
                functions.push(function.clone());
            }
            syn::ImplItem::Const(impl_item_const) => {
                let const_definition =
                    build_const_definition_for_impl_item_const(storage, impl_item_const, id);
                if let Definition::Const(constant) = const_definition {
                    constants.push(constant);
                }
            }
            syn::ImplItem::Type(impl_item_type) => {
                let type_alias = build_associated_type(storage, impl_item_type, id);
                types.push(type_alias);
            }
            syn::ImplItem::Macro(impl_item_macro) => {
                let macro_definition =
                    build_marco_definition_for_impl_item_macro(storage, impl_item_macro, id);
                if let Definition::Macro(macro_definition) = macro_definition {
                    macroses.push(macro_definition);
                }
            }
            syn::ImplItem::Verbatim(token_stream) => {
                let def = build_plane_definition(storage, token_stream, id);
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
    storage.add_node(
        NodeKind::Definition(implementation_definition.clone()),
        parent_id,
    );
    implementation_definition
}

pub(crate) fn build_reference_expression(
    storage: &mut NodesStorage,
    expr_ref: &syn::ExprReference,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let inner = build_expression(storage, &expr_ref.expr, id);
    let expr = Expression::Reference(Rc::new(Reference {
        id,
        location: location!(expr_ref),
        inner,
        is_mutable: expr_ref.mutability.is_some(),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_return_expression(
    storage: &mut NodesStorage,
    expr_return: &syn::ExprReturn,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = expr_return
        .expr
        .as_ref()
        .map(|expr| build_expression(storage, expr, id));
    let expr = Expression::Return(Rc::new(Return {
        id,
        location: location!(expr_return),
        expression,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_repeat_expression(
    storage: &mut NodesStorage,
    expr_repeat: &syn::ExprRepeat,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = build_expression(storage, &expr_repeat.expr, id);
    let count = build_expression(storage, &expr_repeat.len, id);
    let expr = Expression::Repeat(Rc::new(Repeat {
        id,
        location: location!(expr_repeat),
        expression,
        count,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_identifier(
    storage: &mut NodesStorage,
    expr_path: &syn::ExprPath,
    parent_id: u32,
) -> Expression {
    let expr = Expression::Identifier(Rc::new(Identifier {
        id: get_node_id(),
        location: location!(expr_path),
        name: quote::quote! {#expr_path}.to_string(),
    }));
    storage.add_node(NodeKind::Expression(expr.clone()), parent_id);
    expr
}

pub(crate) fn build_parenthesied_expression(
    storage: &mut NodesStorage,
    parenthesied: &syn::ExprParen,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = build_expression(storage, &parenthesied.expr, id);
    let expr = Expression::Parenthesized(Rc::new(Parenthesized {
        id,
        location: location!(parenthesied),
        expression,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_range_expression(
    storage: &mut NodesStorage,
    expr_range: &syn::ExprRange,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let start = match &expr_range.start {
        Some(start) => {
            let expr = build_expression(storage, start.as_ref(), id);
            Some(expr)
        }
        None => None,
    };
    let end = match &expr_range.end {
        Some(end) => {
            let expr = build_expression(storage, end.as_ref(), id);
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(range.clone())),
        parent_id,
    );
    range
}

pub(crate) fn build_member_access_expression(
    storage: &mut NodesStorage,
    member_access: &syn::ExprField,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = build_expression(storage, &member_access.base, id);
    let expr = Expression::MemberAccess(Rc::new(MemberAccess {
        id,
        location: location!(member_access),
        base,
        member_name: MemberAccess::member_name_from_syn_item(member_access),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_assign_expresison(
    storage: &mut NodesStorage,
    assign: &syn::ExprAssign,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let left = build_expression(storage, &assign.left, id);
    let right = build_expression(storage, &assign.right, id);
    let expr = Expression::Assign(Rc::new(Assign {
        id,
        location: location!(assign),
        left,
        right,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_binary_expression(
    storage: &mut NodesStorage,
    binary: &syn::ExprBinary,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let left = build_expression(storage, &binary.left, id);
    let right = build_expression(storage, &binary.right, id);
    let expr = Expression::Binary(Rc::new(Binary {
        id,
        location: location!(binary),
        left,
        right,
        operator: BinOp::from_syn_item(&binary.op),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_unary_expression(
    storage: &mut NodesStorage,
    unary: &syn::ExprUnary,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let inner = build_expression(storage, &unary.expr, id);
    let expr = Expression::Unary(Rc::new(Unary {
        id,
        location: location!(unary),
        expression: inner,
        operator: UnOp::from_syn_item(&unary.op),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_break_expression(
    storage: &mut NodesStorage,
    expr_break: &syn::ExprBreak,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expr = if let Some(inner_expr) = &expr_break.expr {
        let expression = Some(build_expression(storage, inner_expr, id));
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_closure_expression(
    storage: &mut NodesStorage,
    expr_closure: &syn::ExprClosure,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let body = build_expression(storage, expr_closure.body.as_ref(), id);
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
            storage.add_node(
                NodeKind::Statement(Statement::Expression(Expression::Identifier(
                    identifier.clone(),
                ))),
                id,
            );
            identifier
        })
        .collect::<Vec<_>>();
    // build the AST type node for the closure return annotation, if present
    let returns = if let syn::ReturnType::Type(_, ty) = &expr_closure.output {
        build_type(storage, ty, id)
    } else {
        // no explicit return type on closure, use infer placeholder
        build_type(storage, &syn::parse_str::<syn::Type>("_").unwrap(), id)
    };
    storage.add_node(NodeKind::Type(returns.clone()), id);
    storage.add_node(NodeKind::Statement(Statement::Expression(body.clone())), id);
    let closure = Expression::Closure(Rc::new(Closure {
        id,
        location: location!(expr_closure),
        captures: captures.clone(),
        returns,
        body,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(closure.clone())),
        parent_id,
    );
    closure
}

pub(crate) fn build_cast_expression(
    storage: &mut NodesStorage,
    expr_cast: &syn::ExprCast,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = build_expression(storage, &expr_cast.expr, id);
    // build the AST type node for the cast target
    let ty_node = build_type(storage, &expr_cast.ty, id);
    storage.add_node(NodeKind::Type(ty_node.clone()), id);
    let expr = Expression::Cast(Rc::new(Cast {
        id,
        location: location!(expr_cast),
        base,
        target_type: ty_node,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_block_statement(
    storage: &mut NodesStorage,
    block: &syn::Block,
    parent_id: u32,
) -> Statement {
    let id = get_node_id();
    let statements = block
        .stmts
        .iter()
        .filter(|item| !matches!(item, syn::Stmt::Item(syn::Item::Use(_)))) //TODO: handle it
        .map(|stmt| build_statement(storage, stmt, id))
        .collect();
    let stmt = Statement::Block(Rc::new(Block {
        id,
        location: location!(block),
        statements,
    }));
    storage.add_node(NodeKind::Statement(stmt.clone()), parent_id);
    stmt
}

pub(crate) fn build_block_expression(
    storage: &mut NodesStorage,
    block_expr: &ExprBlock,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let stmt = build_block_statement(storage, &block_expr.block, id);
    storage.add_node(NodeKind::Statement(stmt.clone()), parent_id);
    let block = Expression::EBlock(Rc::new(EBlock {
        id,
        location: location!(block_expr),
        block: match stmt {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    storage.add_node(
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
    storage: &mut NodesStorage,
    expr_const: &syn::ExprConst,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(storage, &expr_const.block, parent_id);
    let const_block = Expression::Const(Rc::new(ConstBlock {
        id,
        location: block_statement.location().clone(),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(const_block.clone())),
        parent_id,
    );
    const_block
}

pub(crate) fn build_continue_expression(
    storage: &mut NodesStorage,
    expr_continue: &syn::ExprContinue,
    parent_id: u32,
) -> Expression {
    let expr = Expression::Continue(Rc::new(Continue {
        id: get_node_id(),
        location: location!(expr_continue),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_for_loop_expression(
    storage: &mut NodesStorage,
    for_loop: &syn::ExprForLoop,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(storage, &for_loop.body, parent_id);
    let expression = build_expression(storage, &for_loop.expr, id);
    let for_loop = Expression::ForLoop(Rc::new(ForLoop {
        id,
        location: location!(for_loop),
        expression,
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(for_loop.clone())),
        parent_id,
    );
    for_loop
}

pub(crate) fn build_if_expression(
    storage: &mut NodesStorage,
    if_expr: &syn::ExprIf,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let then_block = build_block_statement(storage, &if_expr.then_branch, parent_id);
    let condition = build_expression(storage, &if_expr.cond, id);
    let else_branch = if let Some((_, else_expr)) = &if_expr.else_branch {
        Some(build_expression(storage, else_expr, id))
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_loop_expression(
    storage: &mut NodesStorage,
    loop_expr: &syn::ExprLoop,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(storage, &loop_expr.body, parent_id);
    let eloop = Loop {
        id,
        location: location!(loop_expr),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    };
    let expr = Expression::Loop(Rc::new(eloop));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_while_expression(
    storage: &mut NodesStorage,
    expr_while: &syn::ExprWhile,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let label = expr_while
        .label
        .as_ref()
        .map(|label| label.to_token_stream().to_string());
    let block_statement = build_block_statement(storage, &expr_while.body, parent_id);
    let condition = build_expression(storage, &expr_while.cond, id);
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_discarded_identifier(
    storage: &mut NodesStorage,
    expr: &syn::Expr,
    parent_id: u32,
) -> Expression {
    let identifier = Expression::Identifier(Rc::new(Identifier {
        id: get_node_id(),
        location: location!(expr),
        name: "_".to_string(),
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(identifier.clone())),
        parent_id,
    );
    identifier
}

pub(crate) fn build_index_access_expression(
    storage: &mut NodesStorage,
    index_access: &syn::ExprIndex,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let base = build_expression(storage, &index_access.expr, id);
    let index = build_expression(storage, &index_access.index, id);
    let expr = Expression::IndexAccess(Rc::new(IndexAccess {
        id,
        location: location!(index_access),
        base,
        index,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_let_guard_expression(
    storage: &mut NodesStorage,
    let_guard: &syn::ExprLet,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let guard = build_pattern(&let_guard.pat);
    storage.add_node(NodeKind::Pattern(guard.clone()), id);
    let value = build_expression(storage, &let_guard.expr, id);
    let expr = Expression::LetGuard(Rc::new(LetGuard {
        id,
        location: location!(let_guard),
        guard,
        value,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_macro_expression(
    storage: &mut NodesStorage,
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(macro_.clone())),
        parent_id,
    );
    macro_
}

pub(crate) fn build_macro_definition(
    storage: &mut NodesStorage,
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
    storage.add_node(
        NodeKind::Statement(Statement::Definition(def.clone())),
        parent_id,
    );
    def
}

pub(crate) fn build_match_arm(storage: &mut NodesStorage, arm: &syn::Arm, id: u32) -> MatchArm {
    let pattern = build_pattern(&arm.pat);
    let expression = build_expression(storage, &arm.body, id);
    MatchArm {
        pattern,
        expression,
    }
}

pub(crate) fn build_match_expression(
    storage: &mut NodesStorage,
    match_expr: &syn::ExprMatch,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = build_expression(storage, &match_expr.expr, id);
    let arms = match_expr
        .arms
        .iter()
        .map(|arm| build_match_arm(storage, arm, id))
        .collect();
    let expr = Expression::Match(Rc::new(Match {
        id,
        location: location!(match_expr),
        expression,
        arms,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_unsafe_expression(
    storage: &mut NodesStorage,
    unsafe_expr: &syn::ExprUnsafe,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(storage, &unsafe_expr.block, parent_id);
    let expr = Expression::Unsafe(Rc::new(Unsafe {
        id,
        location: location!(unsafe_expr),
        block: match block_statement {
            Statement::Block(ref block) => block.clone(),
            _ => panic!("Expected a block statement"),
        },
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_tuple_expression(
    storage: &mut NodesStorage,
    expr_tuple: &syn::ExprTuple,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let elements = expr_tuple
        .elems
        .iter()
        .map(|expr| build_expression(storage, expr, id))
        .collect::<Vec<_>>();
    let expr = Expression::Tuple(Rc::new(Tuple {
        id,
        location: location!(expr_tuple),
        elements,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_struct_expression(
    storage: &mut NodesStorage,
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
            let value = build_expression(storage, &field.expr, parent_id);
            (name, value)
        })
        .collect::<Vec<_>>();
    let rest_dots = expr_struct
        .rest
        .as_ref()
        .map(|expr| build_expression(storage, expr, parent_id));
    let estruct = Expression::EStruct(Rc::new(EStruct {
        id: get_node_id(),
        location: location!(expr_struct),
        name,
        fields,
        rest_dots,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(estruct.clone())),
        parent_id,
    );
    estruct
}

pub(crate) fn build_addr_expression(
    storage: &mut NodesStorage,
    expr_raddr: &syn::ExprRawAddr,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = build_expression(storage, &expr_raddr.expr, id);
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
    storage.add_node(
        NodeKind::Statement(Statement::Expression(addr.clone())),
        parent_id,
    );
    addr
}

pub(crate) fn build_try_expression(
    storage: &mut NodesStorage,
    expr_try: &syn::ExprTry,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let expression = build_expression(storage, &expr_try.expr, id);
    let expr = Expression::Try(Rc::new(Try {
        id,
        location: location!(expr_try),
        expression,
    }));
    storage.add_node(
        NodeKind::Statement(Statement::Expression(expr.clone())),
        parent_id,
    );
    expr
}

pub(crate) fn build_try_block_expression(
    storage: &mut NodesStorage,
    expr_try_block: &syn::ExprTryBlock,
    parent_id: u32,
) -> Expression {
    let id = get_node_id();
    let block_statement = build_block_statement(storage, &expr_try_block.block, parent_id);
    let expr = match block_statement {
        Statement::Block(block) => build_eblock_expression(&block, id),
        _ => panic!(
            "Expected a block statement but got {}",
            serde_json::to_string(&block_statement).unwrap()
        ),
    };
    storage.add_node(
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
    storage: &mut NodesStorage,
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
    storage.add_node(
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
    storage: &mut NodesStorage,
    macro_stmt: &syn::StmtMacro,
    parent_id: u32,
) -> Statement {
    let id = get_node_id();
    let location = location!(macro_stmt);
    let macro_ = Statement::Macro(Rc::new(Macro {
        id,
        location,
        name: macro_stmt.mac.path.to_token_stream().to_string(),
        text: quote::quote! {#macro_stmt}.to_string().replace(' ', ""),
    }));
    storage.add_node(NodeKind::Statement(macro_.clone()), parent_id);
    macro_
}

pub(crate) fn build_let_statement(
    storage: &mut NodesStorage,
    stmt_let: &syn::Local,
    parent_id: u32,
) -> Statement {
    let location = location!(stmt_let);
    let name = match &stmt_let.pat {
        syn::Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
        syn::Pat::Type(pat_type) => match &*pat_type.pat {
            syn::Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
            other => other.to_token_stream().to_string(),
        },
        other => other.to_token_stream().to_string(),
    };
    let pattern = build_pattern(&stmt_let.pat);
    let id = get_node_id();
    let mut initial_value = None;
    let mut initial_value_alternative = None;
    let ref_stmt = &stmt_let;
    if ref_stmt.init.is_some() {
        let expr = build_expression(storage, &stmt_let.init.as_ref().unwrap().expr, id);
        initial_value = Some(expr.clone());
        storage.add_node(NodeKind::Statement(Statement::Expression(expr)), id);

        if stmt_let.init.clone().unwrap().diverge.is_some() {
            let expr = build_expression(
                storage,
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
            storage.add_node(NodeKind::Statement(Statement::Expression(expr)), id);
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
    storage.add_node(NodeKind::Statement(statement.clone()), parent_id);
    statement
}

pub(crate) fn build_const_definition(
    storage: &mut NodesStorage,
    item_const: &syn::ItemConst,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item_const);
    let name = item_const.ident.to_string();
    let value = build_expression(storage, &item_const.expr, id);
    storage.add_node(
        NodeKind::Statement(Statement::Expression(value.clone())),
        id,
    );
    let ty = build_type(storage, item_const.ty.as_ref(), id);
    let visibility = Visibility::from_syn_visibility(&item_const.vis);

    let def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value: Some(value),
    }));

    storage.add_node(NodeKind::Definition(def.clone()), parent_id);

    def
}

pub(crate) fn build_extern_crate_definition(
    storage: &mut NodesStorage,
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
    storage.add_node(NodeKind::Definition(def.clone()), parent_id);
    def
}

pub(crate) fn build_function_from_item_fn(
    storage: &mut NodesStorage,
    item_fn: &syn::ItemFn,
    parent_id: u32,
) -> Rc<Function> {
    let id: u32 = get_node_id();
    let name = item_fn.sig.ident.to_string();
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
                storage.add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
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
                    storage.add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                }
            }
        }
    }
    // build the AST type node for the function return annotation, defaulting to unit
    let mut returns: Type = build_type(storage, &syn::parse_str::<syn::Type>("()").unwrap(), id);
    if let syn::ReturnType::Type(_, ty) = &item_fn.sig.output {
        returns = build_type(storage, ty, id);
    }
    storage.add_node(NodeKind::Type(returns.clone()), id);
    let block_statement = build_block_statement(storage, &item_fn.block, parent_id);
    let block = match block_statement.clone() {
        Statement::Block(block) => {
            storage.add_node(NodeKind::Statement(Statement::Block(block.clone())), id);
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
        id,
        attributes,
        location: location!(item_fn),
        name,
        visibility: Visibility::from_syn_visibility(&item_fn.vis),
        generics,
        parameters: fn_parameters,
        returns,
        body: block,
    });
    storage.add_node(
        NodeKind::Definition(Definition::Function(function.clone())),
        parent_id,
    );
    function
}

pub(crate) fn build_function_from_impl_item_fn(
    storage: &mut NodesStorage,
    item_fn: &syn::ImplItemFn,
    self_ty: &syn::Type,
    id: u32,
) -> Rc<Function> {
    // Build the function and then adjust its return type for methods (mapping `Self` to concrete type)
    let mut function = build_function_from_item_fn(
        storage,
        &ItemFn {
            attrs: item_fn.attrs.clone(),
            vis: item_fn.vis.clone(),
            sig: item_fn.sig.clone(),
            block: Box::new(item_fn.block.clone()),
        },
        id,
    );
    // If the return type refers to `Self`, replace it with the concrete `self_ty`
    let self_name = self_ty.to_token_stream().to_string().replace(' ', "");
    // Ensure unique ownership before mutating
    let func_mut = Rc::make_mut(&mut function);
    func_mut.returns = match &func_mut.returns {
        Type::Typename(tn) => {
            let name = tn
                .name
                .split_whitespace()
                .map(|token| if token == "Self" { &self_name } else { token })
                .collect::<Vec<_>>()
                .join(" ");
            let new_tn = Rc::new(Typename {
                id: tn.id,
                location: tn.location.clone(),
                name,
            });
            Type::Typename(new_tn)
        }
        Type::Alias(alias) => Type::Alias(alias.clone()),
        Type::Struct(ts) => Type::Struct(ts.clone()),
    };
    function
}

pub(crate) fn build_associated_type(
    storage: &mut NodesStorage,
    item_type: &syn::ImplItemType,
    parent_id: u32,
) -> Rc<TypeAlias> {
    let id = get_node_id();
    let type_alias = Rc::new(TypeAlias {
        id,
        location: location!(item_type),
        name: item_type.ident.to_string(),
        visibility: Visibility::from_syn_visibility(&item_type.vis),
        ty: build_type(storage, &item_type.ty, parent_id),
    });
    storage.add_node(
        NodeKind::Definition(Definition::AssocType(type_alias.clone())),
        parent_id,
    );
    type_alias
}

pub(crate) fn build_static_definition(
    storage: &mut NodesStorage,
    item_static: &syn::ItemStatic,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let attributes = extract_attrs(&item_static.attrs);
    let location = location!(item_static);
    let name = item_static.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_static.vis);
    let mutable = matches!(item_static.mutability, syn::StaticMutability::Mut(_));
    let ty = build_type(storage, &item_static.ty, id);
    let expr = build_expression(storage, &item_static.expr, id);

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

    storage.add_node(NodeKind::Definition(static_def.clone()), parent_id);
    static_def
}

pub(crate) fn build_mod_definition(
    storage: &mut NodesStorage,
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
            .filter(|item| !matches!(item, syn::Item::Use(_))) //TODO: handle it
            .map(|item| build_definition(storage, item, id))
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

    storage.add_node(NodeKind::Definition(mod_def.clone()), parent_id);
    mod_def
}

pub(crate) fn build_plane_definition(
    storage: &mut NodesStorage,
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
    storage.add_node(NodeKind::Definition(plane_def.clone()), parent_id);
    plane_def
}

pub(crate) fn build_use_directive(
    storage: &mut NodesStorage,
    use_directive: &syn::ItemUse,
    parent_id: u32,
    file_mod: &str,
) -> Directive {
    let raw_path = use_directive.tree.to_token_stream().to_string();
    let prefix = if let syn::UseTree::Path(use_path) = &use_directive.tree {
        if use_path.ident == "crate" {
            file_mod
        } else {
            "" //file_mod.split("::").next().unwrap_or("")
        }
    } else {
        ""
    };
    let imported = use_tree_to_full_paths(&use_directive.tree, prefix);
    let directive = Directive::Use(Rc::new(Use {
        id: get_node_id(),
        location: location!(use_directive),
        visibility: Visibility::from_syn_visibility(&use_directive.vis),
        path: raw_path,
        imported_types: imported,
        target: RefCell::new(BTreeMap::new()),
    }));
    storage.add_node(NodeKind::Directive(directive.clone()), parent_id);
    directive
}

fn use_tree_to_full_paths(tree: &syn::UseTree, prefix: &str) -> Vec<String> {
    match tree {
        syn::UseTree::Path(syn::UsePath { ident, tree, .. }) => {
            let next = if ident == "crate" {
                prefix.split("::").next().unwrap_or(prefix).to_string()
            } else if prefix.is_empty() {
                ident.to_string()
            } else {
                format!("{prefix}::{ident}")
            };
            use_tree_to_full_paths(tree, &next)
        }
        syn::UseTree::Name(syn::UseName { ident, .. }) => {
            let path = if prefix.is_empty() {
                ident.to_string()
            } else {
                format!("{prefix}::{ident}")
            };
            vec![path]
        }
        syn::UseTree::Rename(use_rename) => {
            let base = if prefix.is_empty() {
                use_rename.ident.to_string()
            } else {
                format!("{}::{}", prefix, use_rename.ident)
            };
            vec![format!("{}%{}", base, use_rename.rename)]
        }
        syn::UseTree::Glob(_) => {
            let path = if prefix.is_empty() {
                "*".to_string()
            } else {
                format!("{prefix}::*")
            };
            vec![path]
        }
        syn::UseTree::Group(syn::UseGroup { items, .. }) => items
            .iter()
            .flat_map(|t| use_tree_to_full_paths(t, prefix))
            .collect(),
    }
}

pub(crate) fn build_type_alias_definition(
    storage: &mut NodesStorage,
    item_type: &syn::ItemType,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item_type);
    let name = item_type.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item_type.vis);
    let ty = build_type(storage, &item_type.ty, id);
    // let attributes = extract_attrs(&item_type.attrs);

    let type_def = Definition::TypeAlias(Rc::new(TypeAlias {
        id,
        location,
        name,
        visibility,
        ty,
    }));

    storage.add_node(NodeKind::Definition(type_def.clone()), parent_id);
    type_def
}

pub(crate) fn build_field(
    storage: &mut NodesStorage,
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
    let ty = build_type(storage, &field.ty, id);
    let field = Rc::new(Field {
        id,
        location,
        name,
        visibility,
        mutability,
        ty,
    });
    storage.add_node(NodeKind::Misc(Misc::Field(field.clone())), parent_id);
    field
}

pub(crate) fn build_union_definition(
    storage: &mut NodesStorage,
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
        .map(|f| build_field(storage, f, id))
        .collect();

    let union_def = Definition::Union(Rc::new(super::definition::Union {
        id,
        location,
        attributes,
        name,
        visibility,
        fields,
    }));

    storage.add_node(NodeKind::Definition(union_def.clone()), parent_id);
    union_def
}

pub(crate) fn build_const_definition_for_impl_item_const(
    storage: &mut NodesStorage,
    item: &syn::ImplItemConst,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.ident.to_string();
    let visibility = Visibility::from_syn_visibility(&item.vis);

    let ty = build_type(storage, &item.ty, id);
    let value = build_expression(storage, &item.expr, id);

    let constant_def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value: Some(value),
    }));

    storage.add_node(NodeKind::Definition(constant_def.clone()), parent_id);

    constant_def
}

fn build_const_definition_from_trait_item(
    storage: &mut NodesStorage,
    item: &syn::TraitItemConst,
    visibility: Visibility,
    parent_id: u32,
) -> Definition {
    let id = get_node_id();
    let location = location!(item);
    let name = item.ident.to_string();
    let ty = build_type(storage, &item.ty, id);
    let value = item
        .default
        .as_ref()
        .map(|expr| build_expression(storage, &expr.1, id));

    let constant_def = Definition::Const(Rc::new(Const {
        id,
        location,
        name,
        visibility,
        type_: ty,
        value,
    }));

    storage.add_node(NodeKind::Definition(constant_def.clone()), parent_id);

    constant_def
}

fn build_function_definition_for_trait_item_fn(
    storage: &mut NodesStorage,
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
                storage.add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
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
                    storage.add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                }
            }
        }
    }
    let mut returns: Type = build_type(storage, &syn::parse_str::<syn::Type>("()").unwrap(), id);
    if let syn::ReturnType::Type(_, ty) = &item.sig.output {
        returns = build_type(storage, ty, id);
    }
    storage.add_node(NodeKind::Type(returns.clone()), id);

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
        returns,
        body: None,
    });
    storage.add_node(
        NodeKind::Definition(Definition::Function(function.clone())),
        parent_id,
    );
    Definition::Function(function)
}

fn build_impl_trait_type(_: &mut NodesStorage, item: &syn::TraitItemType, _: u32) -> Definition {
    // todo!("Soroban SDK does not support trait items of type TraitItemType yet. This is a placeholder for future implementation.");
    Definition::Plane(Rc::new(Plane {
        id: get_node_id(),
        location: location!(item),
        value: "TraitItemType".to_string(),
    }))
}

// fn build_type_alias_definition_for_trait_item_type(
//     storage: &mut NodesStorage,
//     item: &syn::TraitItemType,
//     visibility: Visibility,
//     parent_id: u32,
// ) -> Definition {
//     let id = get_node_id();
//     let location = location!(item);
//     let name = item.ident.to_string();
//     let type_alias = TypeAlias {
//         id,
//         location,
//         name: name.clone(),
//         visibility,
//         ty: Box::new(Type::Typename(name)),
//     };
//     let def = Definition::CustomType(Type::Alias(Rc::new(type_alias)));
//     storage.add_node(
//         NodeKind::Statement(Statement::Definition(def.clone())),
//         parent_id,
//     );
//     def
// }

fn build_macro_definition_for_trait_item_macro(
    storage: &mut NodesStorage,
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
    storage.add_node(NodeKind::Definition(macro_.clone()), parent_id);
    macro_
}

pub(crate) fn build_trait_definition(
    storage: &mut NodesStorage,
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
                build_const_definition_from_trait_item(storage, item, visibility.clone(), id)
            }
            syn::TraitItem::Fn(item) => {
                build_function_definition_for_trait_item_fn(storage, item, visibility.clone(), id)
            }
            syn::TraitItem::Type(item) => build_impl_trait_type(storage, item, id),
            syn::TraitItem::Macro(item) => {
                build_macro_definition_for_trait_item_macro(storage, item, id)
            }
            syn::TraitItem::Verbatim(token_stream) => {
                build_plane_definition(storage, token_stream, id)
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

    storage.add_node(NodeKind::Definition(trait_def.clone()), parent_id);
    trait_def
}

pub(crate) fn build_trait_alias_definition(
    storage: &mut NodesStorage,
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

    storage.add_node(NodeKind::Definition(trait_alias_def.clone()), parent_id);
    trait_alias_def
}

pub(crate) fn build_marco_definition_for_impl_item_macro(
    storage: &mut NodesStorage,
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
    storage.add_node(NodeKind::Definition(macro_.clone()), parent_id);
    macro_
}
