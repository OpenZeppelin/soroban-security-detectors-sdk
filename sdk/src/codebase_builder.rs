use crate::ast::node_type::NodeKind;
use crate::ast_types_builder::ParserCtx;
use crate::errors::SDKErr;
use crate::extern_prelude::ExternPrelude;
use crate::symbol_table::{fixpoint_resolver, Scope};
use crate::utils::project::{find_user_crate_root, FileProvider, MemoryFS};
use crate::{Codebase, NodesStorage, OpenState, SealedState, SymbolTable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Write;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::rc::Rc;

impl Codebase<OpenState> {
    #[must_use]
    pub(crate) fn new(
        storage: NodesStorage,
        symbol_table: SymbolTable,
        extern_prelude: ExternPrelude,
    ) -> Self {
        Self {
            storage,
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table,
            extern_prelude,
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
        if self.syn_files.contains_key(file_path) {
            return Err(SDKErr::AddDuplicateItemError(file_path.to_string()));
        }
        let syn_file = parse_file(file_path, content)?;
        self.syn_files.insert(file_path.to_string(), syn_file);
        Ok(())
    }

    /// Builds the API from the codebase.
    ///
    /// # Errors
    /// Returns an error if the user crate root cannot be found or if parsing fails.
    ///
    /// # Panics
    /// Panics if the internal `fname_ast_map` is `None`.
    #[allow(clippy::too_many_lines, clippy::cast_possible_truncation)]
    pub fn build_api(
        &mut self,
        files: &HashMap<String, String>,
    ) -> anyhow::Result<Box<Codebase<SealedState>>> {
        let files_vec: Rc<RefCell<Vec<(PathBuf, String)>>> = Rc::new(RefCell::new(
            files
                .iter()
                .map(|(k, v)| (PathBuf::from(k), v.clone()))
                .collect(),
        ));
        let loader = FileProvider::Mem(Box::new(MemoryFS {
            files: files_vec.clone(),
        }));
        let user_root = find_user_crate_root(&files_vec, &loader)?;
        if user_root.ends_with("synthetic_root.rs") {
            files_vec.borrow_mut().retain(|(path, _)| {
                path.file_name().and_then(|s| s.to_str()) != Some("synthetic_root.rs")
            });
            let mut synthetic_root_content = String::new();
            for (path, _) in files_vec.borrow().iter() {
                let f_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
                if !f_name.is_empty() {
                    let _ = writeln!(
                        synthetic_root_content,
                        "pub mod {};",
                        f_name.replace(".rs", "")
                    );
                }
            }
            files_vec
                .borrow_mut()
                .push((user_root.clone(), synthetic_root_content));
        }
        let node_id_seed = 1_000_000_u32;
        let scope = Scope::new(
            node_id_seed,
            "soroban_security_detectors_sdk".to_string(),
            None,
        );

        self.symbol_table.insert_scope(scope.clone());
        let mut parser = ParserCtx::new(
            node_id_seed,
            loader,
            scope.clone(),
            &mut self.storage,
            &mut self.symbol_table,
            user_root,
        );
        parser.parse();
        drop(parser);
        fixpoint_resolver(&mut self.symbol_table, &mut self.extern_prelude);
        self.storage.seal();
        let mut codebase = Codebase::<SealedState> {
            storage: self.storage.clone(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table: self.symbol_table.clone(),
            extern_prelude: self.extern_prelude.clone(),
            _state: PhantomData,
        };
        for contract in codebase.contracts() {
            let struct_id = contract.id;
            for func in contract
                .methods
                .borrow()
                .iter()
                .chain(contract.functions.borrow().iter())
            {
                codebase.storage.add_route_child(struct_id, func.id);
            }
        }
        Ok(Box::new(codebase))
    }

    #[must_use]
    pub fn get_item_by_id(&self, id: u32) -> Option<NodeKind> {
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
    use crate::file::File;
    use crate::statement::Statement;

    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_parse_file() {
        let (file_name, mut content) = get_file_content("account.rs");
        let res = parse_file(&file_name, &mut content);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_contracts_count() {
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content.clone());
        let codebase = codebase.build_api(&data).unwrap();
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 1);

        let mut codebase = Codebase::default();
        let new_file_name = file_name.replace("account.rs", "new_file.rs");
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content.clone());
        data.insert(new_file_name.to_string(), content);
        let codebase = codebase.build_api(&data).unwrap();
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_parse_contract_functions_count() {
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content);
        let codebase = codebase.build_api(&data).unwrap();
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
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content);
        let codebase = codebase.build_api(&data).unwrap();
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
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content);
        let codebase = codebase.build_api(&data).unwrap();
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
        if let Statement::Expression(Expression::FunctionCall(function_call)) = &function_calls[0] {
            assert_eq!(function_call.function_name, "Ok");
        }
    }

    #[test]
    fn test_files() {
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content.clone());
        let codebase = codebase.build_api(&data).unwrap();
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name, "account.rs");

        let mut codebase = Codebase::default();
        let new_file_name = file_name.replace("account.rs", "new_file.rs");
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content.clone());
        data.insert(new_file_name.to_string(), content.clone());
        let codebase = codebase.build_api(&data).unwrap();
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_file_serialize() {
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content);
        let codebase = codebase.build_api(&data).unwrap();
        let files = codebase.files().collect::<Vec<_>>();
        let dump = serde_json::to_string(&files[0]).unwrap();
        let t_file = serde_json::from_str::<File>(&dump).unwrap();
        let t_dump = serde_json::to_string(&t_file).unwrap();
        assert_eq!(dump, t_dump);
    }

    #[test]
    fn test_codebase_serialize() {
        let (file_name, content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        let mut data = HashMap::new();
        data.insert(file_name.clone(), content);
        let codebase = codebase.build_api(&data).unwrap();
        let dump = serde_json::to_string(&codebase).unwrap();
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
