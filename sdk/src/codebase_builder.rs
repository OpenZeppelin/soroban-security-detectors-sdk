use crate::ast::build::build_function_call_expression;
use crate::ast::node_type::NodeKind;
use crate::build::{
    build_array_expression, build_assign_expresison, build_binary_expression,
    build_block_statement, build_break_expression, build_cast_expression,
    build_const_block_expression, build_const_definition, build_eblock_expression, build_enum,
    build_extern_crate_definition, build_for_loop_expression, build_identifier,
    build_if_expression, build_index_access_expression, build_let_guard_expression,
    build_let_statement, build_literal, build_macro_expression, build_macro_statement,
    build_match_expression, build_member_access_expression, build_method_call_expression,
    build_pattern, build_reference_expression, build_struct, build_unary_expression,
    build_unsafe_expression,
};
use crate::contract::Struct;
use crate::custom_type::Type;
use crate::definition::Definition;
use crate::errors::SDKErr;
use crate::expression::{
    Addr, Closure, Continue, EStruct, Expression, Identifier, Lit, Loop, Parenthesized, Range,
    Repeat, Return, Tuple, While,
};
use crate::file::File;
use crate::function::{FnParameter, Function};
use crate::node::Mutability;
use crate::node_type::{ContractType, FileChildType, TypeNode};
use crate::statement::Statement;
use crate::{location, source_code, Codebase, NodesStorage, OpenState, SealedState};
use quote::ToTokens;
use std::path::Path;
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use syn::PointerMutability;
use uuid::Uuid;

impl Codebase<OpenState> {
    #[allow(clippy::new_without_default)]
    #[must_use]
    pub fn new() -> Self {
        Codebase {
            storage: NodesStorage::default(),
            fname_ast_map: Some(HashMap::new()),
            _state: PhantomData,
        }
    }

    /// Parse the file and add it to the codebase.
    /// # Errors
    /// - `SDKErr::AddDuplicateItemError` If the file is already added.
    ///
    /// # Panics
    /// Panics if the internal `fname_ast_map` is `None`.
    pub fn parse_and_add_file(&mut self, file_path: &str, content: &mut str) -> Result<(), SDKErr> {
        if self.fname_ast_map.as_ref().unwrap().contains_key(file_path) {
            return Err(SDKErr::AddDuplicateItemError(file_path.to_string()));
        }
        let file = parse_file(file_path, content)?;
        self.fname_ast_map
            .as_mut()
            .unwrap()
            .insert(file_path.to_string(), file);
        Ok(())
    }

    fn add_node(&mut self, node: NodeKind, parent: u128) {
        self.storage.add_node(node, parent);
    }

    /// Builds the API from the codebase.
    ///
    /// # Panics
    /// Panics if the internal `fname_ast_map` is `None`.
    #[allow(clippy::too_many_lines)]
    pub fn build_api(rc: RefCell<Codebase<OpenState>>) -> RefCell<Codebase<SealedState>> {
        let mut codebase = rc.into_inner();
        let mut items_to_revisit: Vec<syn::Item> = Vec::new();
        let fname_ast_map = codebase.fname_ast_map.take().unwrap();
        for (file_path, ast) in fname_ast_map {
            let mut file_name = String::new();
            let path = Path::new(&file_path);
            if let Some(filename) = path.file_name() {
                file_name = filename.to_string_lossy().to_string();
            }
            let rc_file = Rc::new(File {
                id: Uuid::new_v4().as_u128(),
                children: RefCell::new(Vec::new()),
                name: file_name,
                path: file_path.to_string(),
                attributes: File::attributes_from_file_item(&ast),
                source_code: source_code!(ast),
            });
            let file_node = NodeKind::File(rc_file.clone());
            codebase.add_node(file_node, 0);
            for item in &ast.items {
                match item {
                    syn::Item::Struct(struct_item) => {
                        if let Definition::Struct(rc_struct) =
                            codebase.build_definition(item, rc_file.id)
                        {
                            if Struct::is_struct_contract(struct_item) {
                                let contract: ContractType =
                                    ContractType::Contract(rc_struct.clone());
                                rc_file
                                    .children
                                    .borrow_mut()
                                    .push(FileChildType::Definition(Definition::Contract(
                                        contract.clone(),
                                    )));
                                codebase.add_node(NodeKind::Contract(contract), rc_file.id);
                            } else if Struct::is_struct_contract_type(struct_item) {
                                let contract_type = ContractType::Struct(rc_struct.clone());
                                rc_file
                                    .children
                                    .borrow_mut()
                                    .push(FileChildType::Definition(Definition::Contract(
                                        contract_type.clone(),
                                    )));
                                codebase.add_node(NodeKind::Contract(contract_type), rc_file.id);
                            } else {
                                rc_file
                                    .children
                                    .borrow_mut()
                                    .push(FileChildType::Definition(Definition::Struct(rc_struct)));
                            }
                        }
                    }
                    syn::Item::Enum(_) => {
                        let enum_def: Definition = codebase.build_definition(item, rc_file.id);
                        let res = FileChildType::Definition(enum_def.clone());
                        rc_file.children.borrow_mut().push(res);
                    }
                    syn::Item::Impl(impl_item) => {
                        if impl_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contractimpl"))
                        {
                            if let Some((parent_id, functions)) =
                                codebase.handle_item_impl(impl_item)
                            {
                                for function in functions {
                                    codebase.add_node(
                                        NodeKind::Statement(Statement::Definition(
                                            Definition::Function(function.clone()),
                                        )),
                                        parent_id,
                                    );
                                }
                            } else {
                                items_to_revisit.push(syn::Item::Impl(impl_item.clone()));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        for item in items_to_revisit {
            if let syn::Item::Impl(impl_item) = item {
                if let Some((parent_id, rc_functions)) = codebase.handle_item_impl(&impl_item) {
                    for function in rc_functions {
                        codebase.add_node(
                            NodeKind::Statement(Statement::Definition(Definition::Function(
                                function.clone(),
                            ))),
                            parent_id,
                        );
                        //TODO here should be a proper parent id
                    }
                }
            }
        }
        codebase.storage.seal();
        RefCell::new(Codebase {
            fname_ast_map: None,
            storage: codebase.storage,
            _state: PhantomData,
        })
    }

    #[allow(clippy::match_wildcard_for_single_variants)]
    fn handle_item_impl(&mut self, impl_item: &syn::ItemImpl) -> Option<(u128, Vec<Rc<Function>>)> {
        let contract_name = get_impl_type_name(impl_item).unwrap_or_default();
        if contract_name.is_empty() {
            return None;
        }

        let contract = self.storage.nodes.iter().find_map(|item| match item {
            NodeKind::Contract(contract) => {
                if contract.name() == contract_name {
                    Some(contract.clone())
                } else {
                    None
                }
            }
            _ => None,
        });

        let contract = contract?;

        let mut functions = Vec::new();
        for item in &impl_item.items {
            if let syn::ImplItem::Fn(assoc_fn) = item {
                let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
                for arg in &assoc_fn.sig.inputs {
                    if let syn::FnArg::Typed(type_) = arg {
                        if let syn::Pat::Ident(pat_ident) = &*type_.pat {
                            let name = pat_ident.ident.to_string();
                            let is_self = name == "self";
                            let arg_type = *(type_.ty.clone());
                            fn_parameters.push(Rc::new(FnParameter {
                                id: Uuid::new_v4().as_u128(),
                                name,
                                location: location!(arg_type),
                                type_name: FnParameter::type_name_from_syn_item(&arg_type),
                                is_self,
                            }));
                        }
                    }
                }
                let mut returns: TypeNode = TypeNode::Empty;

                if let syn::ReturnType::Type(_, ty) = &assoc_fn.sig.output {
                    returns = TypeNode::from_syn_item(&ty.clone());
                }
                let block_statement = build_block_statement(self, &assoc_fn.block);
                let block = match block_statement.clone() {
                    Statement::Block(block) => Some(block),
                    _ => None,
                };

                let function = Rc::new(Function {
                    id: Uuid::new_v4().as_u128(),
                    location: location!(assoc_fn),
                    name: Function::function_name_from_syn_impl_item(assoc_fn),
                    visibility: Function::visibility_from_syn_impl_item(assoc_fn),
                    parameters: fn_parameters.clone(),
                    returns,
                    body: block,
                });

                for fn_parameter in fn_parameters {
                    self.add_node(NodeKind::FnParameter(fn_parameter.clone()), function.id);
                }
                self.add_node(NodeKind::Statement(block_statement), function.id);

                match &contract {
                    ContractType::Contract(struct_) | ContractType::Struct(struct_) => {
                        struct_.add_method(function.clone());
                    }
                    ContractType::Enum(_) => {}
                }
                functions.push(function);
            }
        }
        Some((contract.id(), functions))
    }

    #[allow(unused_variables)]
    pub(crate) fn build_definition(&mut self, item: &syn::Item, parent_id: u128) -> Definition {
        match item {
            syn::Item::Const(item_const) => {
                let id = Uuid::new_v4().as_u128();

                let value = self.build_expression(&item_const.expr, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(value.clone())),
                    id,
                );
                let def = build_const_definition(item_const, value, id);
                self.add_node(
                    NodeKind::Statement(Statement::Definition(def.clone())),
                    parent_id,
                );
                def
            }
            syn::Item::Enum(item_enum) => {
                let rc_enum = Rc::new(build_enum(item_enum));

                let def = Definition::Enum(rc_enum.clone());
                self.add_node(
                    NodeKind::Statement(Statement::Definition(def.clone())),
                    parent_id,
                );
                def
            }
            syn::Item::ExternCrate(item_extern_crate) => {
                let def = build_extern_crate_definition(item_extern_crate);
                self.add_node(
                    NodeKind::Statement(Statement::Definition(def.clone())),
                    parent_id,
                );
                def
            }
            syn::Item::Fn(item_fn) => todo!(),
            syn::Item::ForeignMod(item_foreign_mod) => todo!(),
            syn::Item::Impl(item_impl) => todo!(),
            syn::Item::Macro(item_macro) => todo!(),
            syn::Item::Mod(item_mod) => todo!(),
            syn::Item::Static(item_static) => todo!(),
            syn::Item::Struct(item_struct) => {
                let rc_struct = build_struct(item_struct);
                self.add_node(
                    NodeKind::Statement(Statement::Definition(Definition::Struct(
                        rc_struct.clone(),
                    ))),
                    parent_id,
                );
                Definition::Struct(rc_struct.clone())
            }
            syn::Item::Trait(item_trait) => todo!(),
            syn::Item::TraitAlias(item_trait_alias) => todo!(),
            syn::Item::Type(item_type) => todo!(),
            syn::Item::Union(item_union) => todo!(),
            syn::Item::Use(item_use) => todo!(),
            syn::Item::Verbatim(token_stream) => todo!(),
            _ => todo!(),
        }
    }

    pub(crate) fn build_statement(&mut self, stmt: &syn::Stmt, parent_id: u128) -> Statement {
        match stmt {
            syn::Stmt::Expr(stmt_expr, _) => {
                let expression = self.build_expression(stmt_expr, parent_id);
                Statement::Expression(expression)
            }
            syn::Stmt::Local(stmt_let) => {
                let id = Uuid::new_v4().as_u128();
                let mut initial_value = None;
                let mut initial_value_alternative = None;
                let ref_stmt = &stmt_let;
                if ref_stmt.init.is_some() {
                    let expr = self.build_expression(&stmt_let.init.as_ref().unwrap().expr, id);
                    initial_value = Some(expr.clone());
                    self.add_node(NodeKind::Statement(Statement::Expression(expr)), id);

                    if stmt_let.init.clone().unwrap().diverge.is_some() {
                        let expr = self.build_expression(
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
                        self.add_node(NodeKind::Statement(Statement::Expression(expr)), id);
                    }
                }

                let statement =
                    build_let_statement(stmt_let, initial_value, initial_value_alternative, id);
                self.add_node(NodeKind::Statement(statement.clone()), parent_id);
                statement
            }
            syn::Stmt::Macro(stmt_macro) => {
                let macro_ = build_macro_statement(stmt_macro);
                self.add_node(NodeKind::Statement(macro_.clone()), parent_id);
                macro_
            }
            syn::Stmt::Item(stmt_item) => {
                let def = self.build_definition(stmt_item, parent_id);
                Statement::Definition(def)
            }
        }
    }

    #[allow(clippy::too_many_lines, clippy::match_wildcard_for_single_variants)]
    pub(crate) fn build_expression(&mut self, expr: &syn::Expr, parent_id: u128) -> Expression {
        match expr {
            syn::Expr::Array(array_expr) => {
                let array = build_array_expression(self, array_expr);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(array.clone())),
                    parent_id,
                );
                array
            }
            syn::Expr::Assign(assign_expr) => {
                let assign = build_assign_expresison(self, assign_expr);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(assign.clone())),
                    parent_id,
                );
                assign
            }
            syn::Expr::Async(_) => {
                panic!("async expressions are not supported");
            }
            syn::Expr::Await(_) => {
                panic!("await expressions are not supported");
            }
            syn::Expr::Binary(expr_binary) => {
                let binary = build_binary_expression(self, expr_binary);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(binary.clone())),
                    parent_id,
                );
                binary
            }
            syn::Expr::Unary(expr_unary) => {
                let unary = build_unary_expression(self, expr_unary);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(unary.clone())),
                    parent_id,
                );
                unary
            }
            syn::Expr::Break(expr_break) => {
                let break_expr = build_break_expression(self, expr_break);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(break_expr.clone())),
                    parent_id,
                );
                break_expr
            }
            syn::Expr::Block(block_expr) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &block_expr.block);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let block = match block_statement {
                    Statement::Block(block) => build_eblock_expression(&block, id),
                    _ => panic!(
                        "Expected a block statement but got {:?}",
                        serde_json::to_string(&block_statement).unwrap()
                    ),
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(block.clone())),
                    parent_id,
                );
                block
            }
            syn::Expr::Call(expr_call) => {
                let function_call = build_function_call_expression(self, expr_call);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(function_call.clone())),
                    parent_id,
                );
                function_call
            }
            syn::Expr::Cast(expr_cast) => {
                let cast = build_cast_expression(self, expr_cast);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(cast.clone())),
                    parent_id,
                );
                cast
            }
            syn::Expr::Closure(expr_closure) => {
                let id = Uuid::new_v4().as_u128();
                let body = self.build_expression(expr_closure.body.as_ref(), id);
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
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(Expression::Identifier(
                                identifier.clone(),
                            ))),
                            id,
                        );
                        identifier
                    })
                    .collect::<Vec<_>>();
                let returns = Type::T(expr_closure.output.clone().into_token_stream().to_string());
                self.add_node(NodeKind::Statement(Statement::Expression(body.clone())), id);
                let closure = Expression::Closure(Rc::new(Closure {
                    id,
                    location: location!(expr_closure),
                    captures: captures.clone(),
                    returns,
                    body,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(closure.clone())),
                    parent_id,
                );
                closure
            }
            syn::Expr::Const(expr_const) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &expr_const.block);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let const_block = match block_statement {
                    Statement::Block(block) => build_const_block_expression(&block, id),
                    _ => panic!(
                        "Expected a block statement but got {:?}",
                        serde_json::to_string(&block_statement).unwrap()
                    ),
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(const_block.clone())),
                    parent_id,
                );
                const_block
            }
            syn::Expr::Continue(expr_continue) => {
                let cont_expr = Expression::Continue(Rc::new(Continue {
                    id: Uuid::new_v4().as_u128(),
                    location: location!(expr_continue),
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(cont_expr.clone())),
                    parent_id,
                );
                cont_expr
            }
            syn::Expr::ForLoop(expr_forloop) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &expr_forloop.body);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let for_loop = match block_statement {
                    Statement::Block(block) => {
                        build_for_loop_expression(self, expr_forloop, &block, id)
                    }
                    _ => panic!(
                        "Expected a block statement but got {:?}",
                        serde_json::to_string(&block_statement).unwrap()
                    ),
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(for_loop.clone())),
                    parent_id,
                );
                for_loop
            }
            syn::Expr::Field(field_expr) => {
                let reference = build_member_access_expression(self, field_expr);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(reference.clone())),
                    parent_id,
                );
                reference
            }
            syn::Expr::If(expr_if) => {
                let id = Uuid::new_v4().as_u128();
                let then_block = build_block_statement(self, &expr_if.then_branch);
                self.add_node(NodeKind::Statement(then_block.clone()), id);
                match then_block {
                    Statement::Block(block) => {
                        let if_expr = build_if_expression(self, expr_if, block.clone(), id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(if_expr.clone())),
                            parent_id,
                        );
                        if_expr
                    }
                    _ => panic!(
                        "Expected a block statement but got {:?}",
                        serde_json::to_string(&then_block).unwrap()
                    ),
                }
            }
            syn::Expr::Index(expr_index) => {
                let index_access = build_index_access_expression(self, expr_index);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(index_access.clone())),
                    parent_id,
                );
                index_access
            }
            syn::Expr::Infer(expr_indef) => {
                let identifier = Expression::Identifier(Rc::new(Identifier {
                    id: Uuid::new_v4().as_u128(),
                    location: location!(expr_indef),
                    name: "_".to_string(),
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(identifier.clone())),
                    parent_id,
                );
                identifier
            }
            syn::Expr::Let(expr_let) => {
                let id = Uuid::new_v4().as_u128();
                let guard = build_pattern(&expr_let.pat);
                self.add_node(NodeKind::Pattern(guard.clone()), id);
                let let_guard = build_let_guard_expression(self, expr_let, guard, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(let_guard.clone())),
                    parent_id,
                );
                let_guard
            }
            syn::Expr::Lit(expr_lit) => {
                let id = Uuid::new_v4().as_u128();
                let literal = build_literal(&expr_lit.lit);
                self.add_node(NodeKind::Literal(literal.clone()), id);
                let expression = Expression::Lit(Rc::new(Lit {
                    id,
                    location: location!(expr_lit),
                    value: literal,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(expression.clone())),
                    parent_id,
                );
                expression
            }
            syn::Expr::Loop(expr_loop) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &expr_loop.body);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let eloop = Loop {
                    id,
                    location: location!(expr_loop),
                    block: match block_statement {
                        Statement::Block(ref block) => block.clone(),
                        _ => panic!("Expected a block statement"),
                    },
                };
                Expression::Loop(Rc::new(eloop))
            }
            syn::Expr::Macro(expr_macro) => {
                let emacro = build_macro_expression(expr_macro);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(emacro.clone())),
                    parent_id,
                );
                emacro
            }
            syn::Expr::Match(expr_match) => {
                let ematch = build_match_expression(self, expr_match);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(ematch.clone())),
                    parent_id,
                );
                ematch
            }
            syn::Expr::MethodCall(method_call) => {
                let method_call = build_method_call_expression(self, method_call);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(method_call.clone())),
                    parent_id,
                );
                method_call
            }
            syn::Expr::Paren(expr_paren) => {
                let id = Uuid::new_v4().as_u128();
                let expression = self.build_expression(&expr_paren.expr, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(expression.clone())),
                    parent_id,
                );
                let parenthesized = Rc::new(Parenthesized {
                    id,
                    location: location!(expr_paren),
                    expression,
                });
                self.add_node(
                    NodeKind::Statement(Statement::Expression(Expression::Parenthesized(
                        parenthesized.clone(),
                    ))),
                    parent_id,
                );
                Expression::Parenthesized(parenthesized)
            }
            syn::Expr::Path(expr_path) => {
                let identifier = build_identifier(expr_path);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(identifier.clone())),
                    parent_id,
                );
                identifier
            }
            syn::Expr::Range(expr_range) => {
                let id = Uuid::new_v4().as_u128();
                let start = match &expr_range.start {
                    Some(start) => {
                        let expr = self.build_expression(start.as_ref(), id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(expr.clone())),
                            parent_id,
                        );
                        Some(expr)
                    }
                    None => None,
                };
                let end = match &expr_range.end {
                    Some(end) => {
                        let expr = self.build_expression(end.as_ref(), id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(expr.clone())),
                            parent_id,
                        );
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
                self.add_node(
                    NodeKind::Statement(Statement::Expression(range.clone())),
                    parent_id,
                );
                range
            }
            syn::Expr::RawAddr(expr_raddr) => {
                let id = Uuid::new_v4().as_u128();
                let expression = self.build_expression(&expr_raddr.expr, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(expression.clone())),
                    parent_id,
                );
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
                self.add_node(
                    NodeKind::Statement(Statement::Expression(addr.clone())),
                    parent_id,
                );
                addr
            }
            syn::Expr::Reference(expr_ref) => {
                let reference = build_reference_expression(self, expr_ref);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(reference.clone())),
                    parent_id,
                );
                reference
            }
            syn::Expr::Repeat(expr_repeat) => {
                let id = Uuid::new_v4().as_u128();
                let expression = self.build_expression(&expr_repeat.expr, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(expression.clone())),
                    parent_id,
                );
                let count = self.build_expression(&expr_repeat.len, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(count.clone())),
                    parent_id,
                );
                let repeat = Expression::Repeat(Rc::new(Repeat {
                    id,
                    location: location!(expr_repeat),
                    expression,
                    count,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(repeat.clone())),
                    parent_id,
                );
                repeat
            }
            syn::Expr::Return(expr_return) => {
                let id = Uuid::new_v4().as_u128();
                let expression = match &expr_return.expr {
                    Some(expr) => {
                        let expr = self.build_expression(expr, id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(expr.clone())),
                            parent_id,
                        );
                        Some(expr)
                    }
                    None => None,
                };
                let returns = Expression::Return(Rc::new(Return {
                    id,
                    location: location!(expr_return),
                    expression,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(returns.clone())),
                    parent_id,
                );
                returns
            }
            syn::Expr::Struct(expr_struct) => {
                let name = expr_struct.path.segments.last().unwrap().ident.to_string();
                let fields = expr_struct
                    .fields
                    .iter()
                    .map(|field| {
                        let name = match field.member {
                            syn::Member::Named(ref ident) => ident.to_string(),
                            syn::Member::Unnamed(_) => String::new(),
                        };
                        let value = self.build_expression(&field.expr, parent_id);
                        (name, value)
                    })
                    .collect::<Vec<_>>();
                let rest_dots = match &expr_struct.rest {
                    Some(expr) => {
                        let value = self.build_expression(expr, parent_id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(value.clone())),
                            parent_id,
                        );
                        Some(value)
                    }
                    None => None,
                };
                let estruct = Expression::EStruct(Rc::new(EStruct {
                    id: Uuid::new_v4().as_u128(),
                    location: location!(expr_struct),
                    name,
                    fields,
                    rest_dots,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(estruct.clone())),
                    parent_id,
                );
                estruct
            }
            syn::Expr::Try(expr_try) => self.build_expression(&expr_try.expr, parent_id),
            syn::Expr::TryBlock(expr_try_block) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &expr_try_block.block);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let try_block = match block_statement {
                    Statement::Block(block) => build_eblock_expression(&block, id),
                    _ => panic!(
                        "Expected a block statement but got {}",
                        serde_json::to_string(&block_statement).unwrap()
                    ),
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(try_block.clone())),
                    parent_id,
                );
                try_block
            }
            syn::Expr::Tuple(expr_tuple) => {
                let id = Uuid::new_v4().as_u128();
                let elements = expr_tuple
                    .elems
                    .iter()
                    .map(|expr| {
                        let e = self.build_expression(expr, id);
                        self.add_node(
                            NodeKind::Statement(Statement::Expression(e.clone())),
                            parent_id,
                        );
                        e
                    })
                    .collect::<Vec<_>>();
                let tuple = Expression::Tuple(Rc::new(Tuple {
                    id,
                    location: location!(expr_tuple),
                    elements,
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(tuple.clone())),
                    parent_id,
                );
                tuple
            }
            syn::Expr::Unsafe(expr_unsafe) => {
                let id = Uuid::new_v4().as_u128();
                let block_statement = build_block_statement(self, &expr_unsafe.block);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let unsafe_block = match block_statement {
                    Statement::Block(block) => build_unsafe_expression(&block, id),
                    _ => panic!(
                        "Expected a block statement but got {}",
                        serde_json::to_string(&block_statement).unwrap()
                    ),
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(unsafe_block.clone())),
                    parent_id,
                );
                unsafe_block
            }
            syn::Expr::While(expr_while) => {
                let id = Uuid::new_v4().as_u128();
                let label = expr_while
                    .label
                    .as_ref()
                    .map(|label| label.to_token_stream().to_string());
                let block_statement = build_block_statement(self, &expr_while.body);
                self.add_node(NodeKind::Statement(block_statement.clone()), id);
                let condition = self.build_expression(&expr_while.cond, id);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(condition.clone())),
                    parent_id,
                );
                let ewhile = Expression::While(Rc::new(While {
                    id,
                    location: location!(expr_while),
                    label: label.clone(),
                    condition,
                    block: match block_statement {
                        Statement::Block(block) => block.clone(),
                        _ => panic!("Expected a block statement"),
                    },
                }));
                self.add_node(
                    NodeKind::Statement(Statement::Expression(ewhile.clone())),
                    parent_id,
                );
                ewhile
            }
            _ => panic!("Unsupported expression type {}", expr.into_token_stream()),
        }
    }

    #[must_use]
    pub fn get_item_by_id(&self, id: u128) -> Option<NodeKind> {
        self.storage.find_node(id)
    }
}

fn get_impl_type_name(item_impl: &syn::ItemImpl) -> Option<String> {
    if let syn::Type::Path(type_path) = &*item_impl.self_ty {
        if let Some(segment) = type_path.path.segments.last() {
            return Some(segment.ident.to_string());
        }
    }
    None
}

fn parse_file(file_name: &str, content: &mut str) -> Result<syn::File, SDKErr> {
    if let Ok(ast) = syn::parse_file(content) {
        Ok(ast)
    } else {
        Err(SDKErr::AstParseError(file_name.to_string()))
    }
}

#[cfg(test)]
mod tests {

    use crate::expression::Expression;

    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_parse_file() {
        let (file_name, mut content) = get_file_content("account.rs");
        let res = parse_file(&file_name, &mut content);
        assert!(res.is_ok());
    }

    // #[test]
    // fn test_codebase_parse_and_add_file() {
    //     let (file_name, mut content) = get_file_content("account.rs");
    //     let codebase = RefCell::new(Codebase::new());
    //     codebase
    //         .borrow_mut()
    //         .parse_and_add_file(&file_name, &mut content)
    //         .unwrap();
    //     assert_eq!(codebase.borrow().fname_ast_map.len(), 1);
    //     let new_file_name = "new_file.rs";
    //     codebase
    //         .borrow_mut()
    //         .parse_and_add_file(new_file_name, &mut content)
    //         .unwrap();
    //     assert_eq!(codebase.borrow().fname_ast_map.len(), 2);
    //     assert!(codebase.borrow().fname_ast_map.contains_key(&file_name));
    //     assert!(codebase.borrow().fname_ast_map.contains_key(new_file_name));
    // }

    #[test]
    fn test_parse_contracts_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let contracts = codebase.borrow().contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 1);

        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let new_file_name = "new_file.rs";
        codebase
            .borrow_mut()
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let contracts = codebase.borrow().contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_parse_contract_functions_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase.borrow();
        let contract = binding
            .storage
            .nodes
            .iter()
            .find(|item| {
                if let NodeKind::Contract(contract) = item {
                    return contract.name() == "AccountContract";
                }
                false
            })
            .unwrap();
        if let NodeKind::Contract(contract) = contract {
            let contract_functions = contract.get_methods().collect::<Vec<_>>();
            assert_eq!(contract_functions.len(), 3);
            assert_eq!(contract_functions[0].name, "init");
            assert_eq!(contract_functions[1].name, "add_limit");
            assert_eq!(contract_functions[2].name, "__check_auth");
        }
    }

    #[test]
    fn test_parse_function_parameters() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase.borrow();
        let contract = binding
            .storage
            .nodes
            .iter()
            .find(|item| {
                if let NodeKind::Contract(contract) = item {
                    return contract.name() == "AccountContract";
                }
                false
            })
            .unwrap();
        if let NodeKind::Contract(contract) = contract {
            let contract_functions = contract.get_methods().collect::<Vec<_>>();
            let function = contract_functions
                .iter()
                .find(|f| f.name == "add_limit")
                .unwrap();
            let function_parameters = function.parameters().collect::<Vec<_>>();
            assert_eq!(function_parameters.len(), 3);

            assert_eq!(function_parameters[0].name, "env");
            assert!(!function_parameters[0].is_self);
            assert_eq!(function_parameters[0].type_name, "Env");

            assert_eq!(function_parameters[1].name, "token");
            assert!(!function_parameters[1].is_self);
            assert_eq!(function_parameters[1].type_name, "Address");

            assert_eq!(function_parameters[2].name, "limit");
            assert!(!function_parameters[2].is_self);
            assert_eq!(function_parameters[2].type_name, "i128");
        }
    }

    #[test]
    fn test_parse_function_calls() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase.borrow();
        let contract = binding
            .storage
            .nodes
            .iter()
            .find(|item| {
                if let NodeKind::Contract(contract) = item {
                    return contract.name() == "AccountContract";
                }
                false
            })
            .unwrap();
        if let NodeKind::Contract(contract) = contract {
            let contract_functions = contract.get_methods().collect::<Vec<_>>();
            let function = contract_functions
                .iter()
                .find(|f| f.name == "__check_auth")
                .unwrap();

            let function_body = function.body.as_ref().unwrap();
            let function_calls = function_body
                .statements
                .iter()
                .filter(|child| matches!(child, Statement::Expression(Expression::FunctionCall(_))))
                .collect::<Vec<_>>();
            assert_eq!(function_calls.len(), 2);
            if let Statement::Expression(Expression::FunctionCall(function_call)) =
                &function_calls[0]
            {
                assert_eq!(function_call.function_name, "authenticate");
            }
            if let Statement::Expression(Expression::FunctionCall(function_call)) =
                &function_calls[1]
            {
                assert_eq!(function_call.function_name, "Ok");
            }
        }
    }

    #[test]
    fn test_files() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let files = codebase.borrow().files().collect::<Vec<_>>();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name, "account.rs");

        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let new_file_name = "new_file.rs";
        codebase
            .borrow_mut()
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let files = codebase.borrow().files().collect::<Vec<_>>();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_file_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let files = codebase.borrow().files().collect::<Vec<_>>();
        let dump = serde_json::to_string(&files[0]).unwrap();
        std::fs::write("account.json", dump.clone()).unwrap();
        let t_file = serde_json::from_str::<File>(&dump).unwrap();
        let t_dump = serde_json::to_string(&t_file).unwrap();
        assert_eq!(dump, t_dump);
    }

    #[test]
    fn test_codebase_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::new());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let dump = serde_json::to_string(&codebase).unwrap();
        std::fs::write("codebase.json", dump.clone()).unwrap();
        let t_codebase = serde_json::from_str::<Codebase<SealedState>>(&dump).unwrap();
        let t_dump = serde_json::to_string(&t_codebase).unwrap();
        assert_eq!(dump, t_dump);
    }

    fn get_tests_dir_path() -> PathBuf {
        let current_dir = std::env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        current_dir.join("tests")
    }

    fn get_file_content(test_file_name: &str) -> (String, String) {
        let current_dir = get_tests_dir_path();
        let file = current_dir
            .join(test_file_name)
            .to_str()
            .unwrap()
            .to_string();
        let content = std::fs::read_to_string(&file).unwrap();
        (file, content)
    }
}
