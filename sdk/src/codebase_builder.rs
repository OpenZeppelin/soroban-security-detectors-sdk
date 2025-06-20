use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;

use crate::ast::node_type::NodeKind;
use crate::errors::SDKErr;
use crate::prelude::ExternPrelude;
use crate::symbol_table::fixpoint_resolver;
use crate::{Codebase, NodesStorage, OpenState, SealedState, SymbolTable};

impl Codebase<OpenState> {
    #[must_use]
    pub fn new(
        storage: NodesStorage,
        symbol_table: SymbolTable,
        extern_prelude: ExternPrelude,
    ) -> Self {
        Self {
            storage,
            files: Vec::new(),
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
    /// # Panics
    /// Panics if the internal `fname_ast_map` is `None`.
    #[must_use]
    #[allow(clippy::too_many_lines, clippy::cast_possible_truncation)]
    pub fn build_api(mut self) -> Box<Codebase<SealedState>> {
        let mut syn_files_snapshot: Vec<_> = self.syn_files.drain().collect();
        syn_files_snapshot.sort_by(|(path_a, _), (path_b, _)| path_a.cmp(path_b));
        for (file_path, ast) in syn_files_snapshot {
            // let rc_file = build_file(&mut self.storage, file_path, ast);
            // self.files.push(rc_file.clone());
            // let mut file_name = String::new();
            // let path = Path::new(&file_path);
            // if let Some(filename) = path.file_name() {
            //     file_name = filename.to_string_lossy().to_string();
            // }
            // let rc_file = Rc::new(File {
            //     id: Uuid::new_v4().as_u128() as u32,
            //     children: RefCell::new(Vec::new()),
            //     name: file_name.clone(),
            //     path: file_path.to_string(),
            //     attributes: File::attributes_from_file_item(&ast),
            //     source_code: source_code!(ast),
            //     location: location!(ast),
            // });
            // let file_mod = rc_file.file_module_name();
            // self.files.push(rc_file.clone());
            // let file_node = NodeKind::File(rc_file.clone());
            // self.add_node(file_node, 0);
            // for item in &ast.items {
            //     if let syn::Item::Use(item_use) = item {
            //         let directive = build_use_directive(&mut self, item_use, rc_file.id, &file_mod);
            //         rc_file
            //             .children
            //             .borrow_mut()
            //             .push(NodeKind::Directive(directive));
            //     } else {
            //         let definition = self.build_definition(item, rc_file.id);
            //         rc_file
            //             .children
            //             .borrow_mut()
            //             .push(NodeKind::Definition(definition));
            //     }
            // }
        }
        fixpoint_resolver(&mut self.symbol_table, &mut self.extern_prelude);
        self.storage.seal();
        let mut codebase = Codebase::<SealedState> {
            storage: self.storage,
            files: self.files,
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table: self.symbol_table,
            extern_prelude: self.extern_prelude,
            _state: PhantomData,
        };
        // codebase.symbol_table = Some(SymbolTable::from_codebase(&codebase));
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
        Box::new(codebase)
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
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 1);

        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let new_file_name = "new_file.rs";
        codebase
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
        let contracts = codebase.contracts().collect::<Vec<_>>();
        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_parse_contract_functions_count() {
        let (file_name, mut content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
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
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
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
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
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
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name, "account.rs");

        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let new_file_name = "new_file.rs";
        codebase
            .parse_and_add_file(new_file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
        let files = codebase.files().collect::<Vec<_>>();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_file_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
        let files = codebase.files().collect::<Vec<_>>();
        let dump = serde_json::to_string(&files[0]).unwrap();
        std::fs::write("account.json", dump.clone()).unwrap();
        let t_file = serde_json::from_str::<File>(&dump).unwrap();
        let t_dump = serde_json::to_string(&t_file).unwrap();
        // Debug roundtrip JSON mismatch
        assert_eq!(dump, t_dump);
    }

    #[test]
    fn test_codebase_serialize() {
        let (file_name, mut content) = get_file_content("account.rs");
        let mut codebase = Codebase::default();
        codebase
            .parse_and_add_file(&file_name, &mut content)
            .unwrap();
        let codebase = codebase.build_api();
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
