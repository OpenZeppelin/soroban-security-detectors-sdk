#![warn(clippy::pedantic)]
use crate::ast::build::build_function_call_expression;
use crate::ast::contract::Contract;
use crate::ast::node_type::NodeKind;
use crate::build::{
    build_array_expression, build_identifier, build_member_access, build_method_call,
    build_reference,
};
use crate::errors::SDKErr;
use crate::expression::Expression;
use crate::file::File;
use crate::function::{FnParameter, Function};
use crate::node_type::{ContractChildType, FileChildType, FunctionChildType, TypeNode};
use crate::statement::Statement;
use crate::{location, source_code, NodesStorage};
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
                            let contract = Contract {
                                id: Uuid::new_v4().as_u128(),
                                location: location!(struct_item),
                                name: Contract::contract_name_from_syn_item(struct_item),
                                children: RefCell::new(Vec::new()),
                            };
                            let rc_contract = Rc::new(contract);
                            rc_file
                                .children
                                .borrow_mut()
                                .push(FileChildType::Contract(rc_contract.clone()));
                            codebase.add_node(NodeKind::Contract(rc_contract.clone()), rc_file.id);
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

                let function = Rc::new(Function {
                    id: Uuid::new_v4().as_u128(),
                    location: location!(assoc_fn),
                    name: Function::function_name_from_syn_impl_item(assoc_fn),
                    visibility: Function::visibility_from_syn_impl_item(assoc_fn),
                    children: RefCell::new(Vec::new()),
                    returns,
                });
                for fn_parameter in fn_parameters {
                    self.add_node(NodeKind::FnParameter(fn_parameter.clone()), function.id);
                    function
                        .children
                        .borrow_mut()
                        .push(FunctionChildType::Parameter(fn_parameter));
                }
                let statements = self.handle_function_body(&assoc_fn.block, function.id);
                for statement in statements {
                    self.add_node(NodeKind::Statement(statement.clone()), function.id);
                    function
                        .children
                        .borrow_mut()
                        .push(FunctionChildType::Statement(statement));
                }
                contract
                    .children
                    .borrow_mut()
                    .push(ContractChildType::Function(function.clone()));
                functions.push(function);
            }
        }
        Some((contract.id, functions))
    }

    #[allow(clippy::single_match)]
    fn handle_function_body(&mut self, block: &syn::Block, parent_id: u128) -> Vec<Statement> {
        let mut result = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                syn::Stmt::Expr(expr, _) => {
                    let expression = self.build_expression(expr, parent_id, false);
                    match expression {
                        Expression::Empty => {}
                        _ => result.push(Statement::Expression(expression)),
                    }
                }
                _ => {}
            }
        }
        result
    }

    #[allow(clippy::match_wildcard_for_single_variants)]
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
            syn::Expr::Call(expr_call) => {
                let function_call = build_function_call_expression(self, expr_call, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(function_call.clone())),
                    parent_id,
                );
                function_call
            }
            syn::Expr::Try(expr_try) => self.build_expression(&expr_try.expr, parent_id, true),
            syn::Expr::MethodCall(method_call) => {
                let method_call = build_method_call(self, method_call, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(method_call.clone())),
                    parent_id,
                );
                method_call
            }
            syn::Expr::Field(field_expr) => {
                let reference = build_member_access(self, field_expr, is_tried);
                self.add_node(
                    NodeKind::Statement(Statement::Expression(reference.clone())),
                    parent_id,
                );
                reference
            }
            syn::Expr::Reference(expr_ref) => {
                let reference = build_reference(self, expr_ref, is_tried);
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

    use crate::{expression::Expression, node::Node};

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
            let contract_functions = contract.functions().collect::<Vec<_>>();
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
            let contract_functions = contract.functions().collect::<Vec<_>>();
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
            let contract_functions = contract.functions().collect::<Vec<_>>();
            let function = contract_functions
                .iter()
                .find(|f| f.name == "__check_auth")
                .unwrap();

            let function_calls = function
                .children()
                .filter(|child| {
                    matches!(
                        child,
                        FunctionChildType::Statement(Statement::Expression(_))
                    )
                })
                .collect::<Vec<_>>();
            assert_eq!(function_calls.len(), 2);
            if let FunctionChildType::Statement(Statement::Expression(Expression::FunctionCall(
                function_call,
            ))) = &function_calls[0]
            {
                assert_eq!(function_call.function_name, "authenticate");
            }
            if let FunctionChildType::Statement(Statement::Expression(Expression::FunctionCall(
                function_call,
            ))) = &function_calls[1]
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
