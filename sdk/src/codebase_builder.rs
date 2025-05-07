use crate::ast::node_type::NodeKind;
use crate::ast_types_builder::{
    build_addr_expression, build_array_expression, build_assign_expresison,
    build_binary_expression, build_break_expression, build_cast_expression,
    build_closure_expression, build_const_block_expression, build_const_definition,
    build_const_definition_for_impl_item_const, build_continue_expression,
    build_discarded_identifier, build_enum, build_extern_crate_definition,
    build_for_loop_expression, build_function_from_impl_item_fn, build_function_from_item_fn,
    build_identifier, build_if_expression, build_index_access_expression,
    build_let_guard_expression, build_let_statement, build_literal_expression,
    build_loop_expression, build_macro_definition, build_macro_expression, build_macro_statement,
    build_marco_definition_for_impl_item_macro, build_match_expression,
    build_member_access_expression, build_method_call_expression, build_mod_definition,
    build_parenthesied_expression, build_plane_definition, build_range_expression,
    build_reference_expression, build_repeat_expression, build_return_expression,
    build_static_definition, build_struct, build_struct_expression, build_trait_alias_definition,
    build_trait_definition, build_try_block_expression, build_try_expression,
    build_tuple_expression, build_type_alias_from_impl_item_type, build_type_definition,
    build_unary_expression, build_union_definition, build_unsafe_expression, build_use_directive,
    build_while_expression, process_item_impl,
};
use crate::ast_types_builder::{build_block_expression, build_function_call_expression};
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
use crate::node::{Mutability, Visibility};
use crate::node_type::{ContractType, FileChildType, TypeNode};
use crate::statement::Statement;
use crate::{location, source_code, Codebase, NodesStorage, OpenState, SealedState};
use quote::ToTokens;
use std::path::Path;
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use syn::PointerMutability;
use uuid::Uuid;

impl Codebase<OpenState> {
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

    pub(crate) fn add_node(&mut self, node: NodeKind, parent: u128) {
        self.storage.add_node(node, parent);
    }

    /// Builds the API from the codebase.
    ///
    /// # Panics
    /// Panics if the internal `fname_ast_map` is `None`.
    #[allow(clippy::too_many_lines)]
    pub fn build_api(rc: RefCell<Codebase<OpenState>>) -> Box<Codebase<SealedState>> {
        let mut codebase = rc.into_inner();
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
                let definition = codebase.build_definition(item, rc_file.id);
                // if matches!(definition, Definition::Empty) {
                //     items_to_revisit.push((rc_file.id, item.clone()));
                //     continue;
                // }
                rc_file
                    .children
                    .borrow_mut()
                    .push(FileChildType::Definition(definition));
            }
        }
        // for (_, item) in items_to_revisit {
        //     if let syn::Item::Impl(impl_item) = item {
        //         codebase.process_item_impl(&impl_item);
        //     }
        // }
        codebase.storage.seal();
        Box::new(Codebase::new(codebase.storage))
    }

    #[allow(unused_variables, clippy::too_many_lines)]
    pub(crate) fn build_definition(&mut self, item: &syn::Item, parent_id: u128) -> Definition {
        match item {
            syn::Item::Const(item_const) => build_const_definition(self, item_const, parent_id),
            syn::Item::Enum(item_enum) => Definition::Enum(build_enum(self, item_enum, parent_id)),
            syn::Item::ExternCrate(item_extern_crate) => {
                build_extern_crate_definition(self, item_extern_crate, parent_id)
            }
            syn::Item::Fn(item_fn) => {
                Definition::Function(build_function_from_item_fn(self, item_fn, parent_id))
            }
            syn::Item::ForeignMod(_) => todo!("Should not appear"),
            syn::Item::Impl(item_impl) => process_item_impl(self, item_impl, parent_id),
            syn::Item::Macro(item_macro) => build_macro_definition(self, item_macro, parent_id),
            syn::Item::Mod(item_mod) => build_mod_definition(self, item_mod, parent_id),
            syn::Item::Static(item_static) => build_static_definition(self, item_static, parent_id),
            syn::Item::Struct(item_struct) => {
                Definition::Struct(build_struct(self, item_struct, parent_id))
            }
            syn::Item::Trait(item_trait) => build_trait_definition(self, item_trait, parent_id),
            syn::Item::TraitAlias(item_trait_alias) => {
                build_trait_alias_definition(self, item_trait_alias, parent_id)
            }
            syn::Item::Type(item_type) => build_type_definition(self, item_type, parent_id),
            syn::Item::Union(item_union) => build_union_definition(self, item_union, parent_id),
            syn::Item::Use(item_use) => {
                Definition::Directive(build_use_directive(self, item_use, parent_id))
            }
            syn::Item::Verbatim(token_stream) => {
                build_plane_definition(self, token_stream, parent_id)
            }
            _ => todo!(),
        }
    }

    pub(crate) fn build_statement(&mut self, stmt: &syn::Stmt, parent_id: u128) -> Statement {
        match stmt {
            syn::Stmt::Expr(stmt_expr, _) => {
                Statement::Expression(self.build_expression(stmt_expr, parent_id))
            }
            syn::Stmt::Local(stmt_let) => build_let_statement(self, stmt_let, parent_id),
            syn::Stmt::Macro(stmt_macro) => build_macro_statement(self, stmt_macro, parent_id),
            syn::Stmt::Item(stmt_item) => {
                Statement::Definition(self.build_definition(stmt_item, parent_id))
            }
        }
    }

    #[allow(clippy::too_many_lines, clippy::match_wildcard_for_single_variants)]
    pub(crate) fn build_expression(&mut self, expr: &syn::Expr, parent_id: u128) -> Expression {
        match expr {
            syn::Expr::Array(array_expr) => build_array_expression(self, array_expr, parent_id),
            syn::Expr::Assign(assign_expr) => build_assign_expresison(self, assign_expr, parent_id),
            syn::Expr::Async(_) => {
                panic!("async expressions are not supported");
            }
            syn::Expr::Await(_) => {
                panic!("await expressions are not supported");
            }
            syn::Expr::Binary(expr_binary) => build_binary_expression(self, expr_binary, parent_id),
            syn::Expr::Unary(expr_unary) => build_unary_expression(self, expr_unary, parent_id),
            syn::Expr::Break(expr_break) => build_break_expression(self, expr_break, parent_id),
            syn::Expr::Block(block_expr) => build_block_expression(self, block_expr, parent_id),
            syn::Expr::Call(expr_call) => {
                build_function_call_expression(self, expr_call, parent_id)
            }
            syn::Expr::Cast(expr_cast) => build_cast_expression(self, expr_cast, parent_id),
            syn::Expr::Closure(expr_closure) => {
                build_closure_expression(self, expr_closure, parent_id)
            }
            syn::Expr::Const(expr_const) => {
                build_const_block_expression(self, expr_const, parent_id)
            }
            syn::Expr::Continue(expr_continue) => {
                build_continue_expression(self, expr_continue, parent_id)
            }
            syn::Expr::ForLoop(expr_forloop) => {
                build_for_loop_expression(self, expr_forloop, parent_id)
            }
            syn::Expr::Field(field_expr) => {
                build_member_access_expression(self, field_expr, parent_id)
            }
            syn::Expr::If(expr_if) => build_if_expression(self, expr_if, parent_id),
            syn::Expr::Index(expr_index) => {
                build_index_access_expression(self, expr_index, parent_id)
            }
            syn::Expr::Infer(_) => build_discarded_identifier(self, expr, parent_id),
            syn::Expr::Let(expr_let) => build_let_guard_expression(self, expr_let, parent_id),
            syn::Expr::Lit(expr_lit) => build_literal_expression(self, expr_lit, parent_id),
            syn::Expr::Loop(expr_loop) => build_loop_expression(self, expr_loop, parent_id),
            syn::Expr::Macro(expr_macro) => build_macro_expression(self, expr_macro, parent_id),
            syn::Expr::Match(expr_match) => build_match_expression(self, expr_match, parent_id),
            syn::Expr::MethodCall(method_call) => {
                build_method_call_expression(self, method_call, parent_id)
            }
            syn::Expr::Paren(expr_paren) => {
                build_parenthesied_expression(self, expr_paren, parent_id)
            }
            syn::Expr::Path(expr_path) => build_identifier(self, expr_path, parent_id),
            syn::Expr::Range(expr_range) => build_range_expression(self, expr_range, parent_id),
            syn::Expr::RawAddr(expr_raddr) => build_addr_expression(self, expr_raddr, parent_id),
            syn::Expr::Reference(expr_ref) => build_reference_expression(self, expr_ref, parent_id),
            syn::Expr::Repeat(expr_repeat) => build_repeat_expression(self, expr_repeat, parent_id),
            syn::Expr::Return(expr_return) => build_return_expression(self, expr_return, parent_id),
            syn::Expr::Struct(expr_struct) => build_struct_expression(self, expr_struct, parent_id),
            syn::Expr::Try(expr_try) => build_try_expression(self, expr_try, parent_id),
            syn::Expr::TryBlock(expr_try_block) => {
                build_try_block_expression(self, expr_try_block, parent_id)
            }
            syn::Expr::Tuple(expr_tuple) => build_tuple_expression(self, expr_tuple, parent_id),
            syn::Expr::Unsafe(expr_unsafe) => build_unsafe_expression(self, expr_unsafe, parent_id),
            syn::Expr::While(expr_while) => build_while_expression(self, expr_while, parent_id),
            _ => panic!("Unsupported expression type {}", expr.into_token_stream()),
        }
    }

    #[must_use]
    pub fn get_item_by_id(&self, id: u128) -> Option<NodeKind> {
        self.storage.find_node(id)
    }
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
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 1);

        let codebase = RefCell::new(Codebase::default());
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
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_parse_contract_functions_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase;
        let contract = binding
            .contracts()
            .find(|item| item.name == "AccountContract")
            .unwrap();

        assert_eq!(contract.functions.borrow().len(), 3);
        assert_eq!(contract.functions.borrow()[0].name, "init");
        assert_eq!(contract.functions.borrow()[1].name, "add_limit");
        assert_eq!(contract.functions.borrow()[2].name, "__check_auth");
    }

    #[test]
    fn test_parse_function_parameters() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase;
        let contract = binding
            .contracts()
            .find(|item| item.name == "AccountContract")
            .unwrap();

        let contract_functions = contract.functions.borrow();
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

    #[test]
    fn test_parse_function_calls() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let binding = codebase;
        let contract = binding
            .contracts()
            .find(|item| item.name == "AccountContract")
            .unwrap();
        let contract_functions = contract.functions.borrow();
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
        assert_eq!(function_calls.len(), 1);
        // if let Statement::Expression(Expression::FunctionCall(function_call)) =
        //     &function_calls[0]
        // {
        //     assert_eq!(function_call.function_name, "authenticate");
        // }
        if let Statement::Expression(Expression::FunctionCall(function_call)) = &function_calls[0] {
            assert_eq!(function_call.function_name, "Ok");
        }
    }

    #[test]
    fn test_files() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name, "account.rs");

        let codebase = RefCell::new(Codebase::default());
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
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_file_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
        codebase
            .borrow_mut()
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = Codebase::build_api(codebase);
        let files = codebase.files().collect::<Vec<_>>();
        let dump = serde_json::to_string(&files[0]).unwrap();
        std::fs::write("account.json", dump.clone()).unwrap();
        let t_file = serde_json::from_str::<File>(&dump).unwrap();
        let t_dump = serde_json::to_string(&t_file).unwrap();
        assert_eq!(dump, t_dump);
    }

    #[test]
    fn test_codebase_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let codebase = RefCell::new(Codebase::default());
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
