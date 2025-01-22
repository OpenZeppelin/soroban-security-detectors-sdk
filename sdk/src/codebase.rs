#![warn(clippy::pedantic)]
use crate::ast::build::build_function_call_expression;
use crate::ast::contract::Contract;
use crate::ast::node_type::NodeKind;
use crate::build::{
    build_array_expression, build_assign_expresison, build_binary_expression,
    build_block_statement, build_break_expression, build_cast_expression,
    build_const_block_expression, build_contract, build_eblock_expression, build_enum_custom_type,
    build_for_loop_expression, build_identifier, build_if_expression,
    build_index_access_expression, build_member_access_expression, build_method_call_expression,
    build_reference_expression, build_struct_custom_type,
};
use crate::custom_type::Type;
use crate::errors::SDKErr;
use crate::expression::{Closure, Continue, Expression, Identifier};
use crate::file::File;
use crate::function::{FnParameter, Function};
use crate::node_type::{FileChildType, TypeNode};
use crate::statement::Statement;
use crate::{location, source_code, NodesStorage};
use quote::ToTokens;
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use uuid::Uuid;

fn parse_file(file_name: &str, content: &mut str) -> Result<syn::File, SDKErr> {
    if let Ok(ast) = syn::parse_file(content) {
        Ok(ast)
    } else {
        Err(SDKErr::AstParseError(file_name.to_string()))
    }
}

#[allow(dead_code)]
trait CodebaseOpen {}
#[allow(dead_code)]
trait CodebaseSealed {}

pub struct OpenState;
impl CodebaseOpen for OpenState {}

pub struct SealedState;
impl CodebaseSealed for SealedState {}

#[derive(Serialize, Deserialize)]
pub struct Codebase<S> {
    storage: NodesStorage,
    #[serde(skip)]
    fname_ast_map: Option<HashMap<String, syn::File>>,
    _state: PhantomData<S>,
}

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
                        if struct_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contract"))
                        {
                            let rc_contract = build_contract(struct_item);
                            rc_file
                                .children
                                .borrow_mut()
                                .push(FileChildType::Contract(rc_contract.clone()));
                            codebase.add_node(NodeKind::Contract(rc_contract.clone()), rc_file.id);
                        } else if struct_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contracttype"))
                        {
                            let rc_custom_type = build_struct_custom_type(struct_item);
                            rc_file
                                .children
                                .borrow_mut()
                                .push(FileChildType::CustomType(rc_custom_type.clone()));
                            codebase
                                .add_node(NodeKind::CustomType(rc_custom_type.clone()), rc_file.id);
                        }
                    }
                    syn::Item::Enum(enum_item) => {
                        if enum_item
                            .attrs
                            .iter()
                            .any(|attr| attr.path().is_ident("contracttype"))
                        {
                            let rc_custom_type = build_enum_custom_type(enum_item);
                            rc_file
                                .children
                                .borrow_mut()
                                .push(FileChildType::CustomType(rc_custom_type.clone()));
                            codebase
                                .add_node(NodeKind::CustomType(rc_custom_type.clone()), rc_file.id);
                        }
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
                                    codebase
                                        .add_node(NodeKind::Function(function.clone()), parent_id);
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
                        codebase.add_node(NodeKind::Function(function.clone()), parent_id);
                        //TODO here should be proper parent id
                    }
                }
            }
        }
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
                if contract.name == contract_name {
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

                contract.add_method(function.clone());
                functions.push(function);
            }
        }
        Some((contract.id, functions))
    }

    pub(crate) fn build_statement(&mut self, stmt: &syn::Stmt, parent_id: u128) -> Statement {
        match stmt {
            syn::Stmt::Expr(expr, _) => {
                let expression = self.build_expression(expr, parent_id, false);
                Statement::Expression(expression)
            }
            _ => Statement::Expression(Expression::Empty),
        }
    }

    #[allow(clippy::too_many_lines, clippy::match_wildcard_for_single_variants)]
    pub(crate) fn build_expression(
        &mut self,
        expr: &syn::Expr,
        parent_id: u128,
        is_tried: bool,
    ) -> Expression {
        match expr {
            syn::Expr::Array(array_expr) => {
                let array = build_array_expression(self, array_expr, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(array.clone())),
                    parent_id,
                );
                array
            }
            syn::Expr::Assign(assign_expr) => {
                let assign = build_assign_expresison(self, assign_expr, false);
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
                let binary = build_binary_expression(self, expr_binary, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(binary.clone())),
                    parent_id,
                );
                binary
            }
            syn::Expr::Break(expr_break) => {
                let break_expr = build_break_expression(self, expr_break, is_tried);
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
                    _ => Expression::Empty,
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(block.clone())),
                    parent_id,
                );
                block
            }
            syn::Expr::Call(expr_call) => {
                let function_call = build_function_call_expression(self, expr_call, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(function_call.clone())),
                    parent_id,
                );
                function_call
            }
            syn::Expr::Cast(expr_cast) => {
                let cast = build_cast_expression(self, expr_cast, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(cast.clone())),
                    parent_id,
                );
                cast
            }
            syn::Expr::Closure(expr_closure) => {
                let id = Uuid::new_v4().as_u128();
                let body = self.build_expression(expr_closure.body.as_ref(), id, is_tried);
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
                let returns = Type::T(
                    <syn::ReturnType as Clone>::clone(&expr_closure.output)
                        .into_token_stream()
                        .to_string(),
                );
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
                    _ => Expression::Empty,
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
                    _ => Expression::Empty,
                };
                self.add_node(
                    NodeKind::Statement(Statement::Expression(for_loop.clone())),
                    parent_id,
                );
                for_loop
            }
            syn::Expr::Field(field_expr) => {
                let reference = build_member_access_expression(self, field_expr, is_tried);
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
                    _ => Expression::Empty,
                }
            }
            syn::Expr::Index(expr_index) => {
                let index_access = build_index_access_expression(self, expr_index, is_tried);
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
            syn::Expr::Try(expr_try) => self.build_expression(&expr_try.expr, parent_id, true),
            syn::Expr::MethodCall(method_call) => {
                let method_call = build_method_call_expression(self, method_call, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(method_call.clone())),
                    parent_id,
                );
                method_call
            }
            syn::Expr::Reference(expr_ref) => {
                let reference = build_reference_expression(self, expr_ref, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(reference.clone())),
                    parent_id,
                );
                reference
            }
            syn::Expr::Path(expr_path) => {
                let identifier = build_identifier(expr_path);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(identifier.clone())),
                    parent_id,
                );
                identifier
            }
            _ => Expression::Empty,
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

impl Codebase<SealedState> {
    pub fn files(&self) -> impl Iterator<Item = Rc<File>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::File(file) = item {
                res.push(file.clone());
            }
        }
        res.into_iter()
    }

    pub fn contracts(&self) -> impl Iterator<Item = Rc<Contract>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Contract(contract) = item {
                res.push(contract.clone());
            }
        }
        res.into_iter()
    }

    #[must_use = "Use this method to get `Node` source code"]
    pub fn get_node_source_code(&self, node_id: u128) -> Option<String> {
        self.storage.get_node_source_code(node_id)
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
                    return contract.name == "AccountContract";
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
                    return contract.name == "AccountContract";
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
                    return contract.name == "AccountContract";
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
