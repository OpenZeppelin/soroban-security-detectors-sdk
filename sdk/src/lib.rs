#![warn(clippy::pedantic)]
use std::fmt::Write;
use std::{collections::HashMap, fs, io, path::Path};

mod ast;
pub use ast::*;
mod ast_types_builder;
mod codebase_builder;

mod codebase;
pub use codebase::*;

pub mod detector;
pub use detector::*;

mod storage;
pub use storage::*;

pub mod errors;
mod prelude;
mod symbol_table;
pub(crate) mod utils;
use symbol_table::SymbolTable;

use crate::{
    ast_types_builder::ParserCtx,
    prelude::{insert_into_extern_prelude, ExternPrelude},
    symbol_table::{fixpoint_resolver, Scope},
    utils::project::find_user_crate_root,
};

/// Build a code model from the given `HashMap` { "file path" : "file content" }.
/// # Errors
/// - `SDKErr::AstParseError` If the file content cannot be parsed.
pub fn build_codebase<H: std::hash::BuildHasher>(
    files: &HashMap<String, String, H>,
) -> anyhow::Result<Box<Codebase<SealedState>>> {
    let mut storage = NodesStorage::default();
    let mut table = SymbolTable::new();
    let mut extern_prelude = ExternPrelude::new();
    // let mut root_scope = Scope::new(0, "root".to_string(), None);
    let mut external_crate_id: u32 = 0;
    if let Some(sdk_dirs) = utils::sdk_resolver::find_soroban_sdk_files() {
        // for (file, content) in sdk_files {
        //     codebase.parse_and_add_file(file.as_str(), &mut content.clone())?;
        // }
        let mut sdk_vec: Vec<_> = sdk_dirs.iter().map(|(k, v)| (k, v.1.clone())).collect();
        sdk_vec.sort_by(|a, b| b.0.cmp(a.0));
        for (name, path) in sdk_vec {
            // let parser = ParserCtx::new(0, &mut storage, &mut table, &mut root_scope, path.clone());
            // let _ = collect_files_in_dir(&path, &mut files_content_map);
            insert_into_extern_prelude(
                &path,
                name,
                &mut extern_prelude,
                &mut external_crate_id,
                &mut table,
                &mut storage,
            );
        }
        // for sc in table.scopes.values() {
        //     eprintln!("SCOPE {} contains defs:", sc.borrow().name);
        //     for k in sc.borrow().definitions.keys() {
        //         eprintln!("  - {}", k);
        //     }
        // }
        // fixpoint_resolver(&mut table, &mut extern_prelude);
    }
    storage.seal();
    let mut codebase = Codebase::new(storage, table, extern_prelude);
    let files_default: HashMap<_, _, std::collections::hash_map::RandomState> =
        files.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    codebase.build_api(&files_default)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_code_model() {
        let current_dir = get_tests_dir_path();
        let files_map = get_files_map(vec![current_dir
            .join("account.rs")
            .to_str()
            .unwrap()
            .to_string()]);
        let result = build_codebase(&files_map);
        assert!(result.is_ok());
    }

    #[test]
    fn test_contracts_parsing() {
        let current_dir = get_tests_dir_path();
        let files_map = get_files_map(vec![current_dir
            .join("account.rs")
            .to_str()
            .unwrap()
            .to_string()]);
        let codebase = build_codebase(&files_map).unwrap();
        assert_eq!(codebase.contracts().count(), 1);

        let files_map = get_files_map(vec![current_dir
            .join("multiple_contracts.rs")
            .to_str()
            .unwrap()
            .to_string()]);
        let codebase = build_codebase(&files_map).unwrap();
        assert_eq!(codebase.contracts().count(), 2);
    }

    #[test]
    fn test_codebase_with_multiple_files() {
        let current_dir = get_tests_dir_path();
        let files_map = get_files_map(vec![
            current_dir.join("account.rs").to_str().unwrap().to_string(),
            current_dir
                .join("multiple_contracts.rs")
                .to_str()
                .unwrap()
                .to_string(),
        ]);
        let codebase = build_codebase(&files_map).unwrap();
        assert_eq!(codebase.contracts().count(), 3);
    }

    fn get_tests_dir_path() -> std::path::PathBuf {
        let current_dir = std::env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        current_dir.join("tests")
    }

    fn get_files_map(files: Vec<String>) -> HashMap<String, String> {
        files
            .into_iter()
            .map(|file| {
                let content = std::fs::read_to_string(file.clone()).unwrap();
                (file, content)
            })
            .collect::<HashMap<String, String>>()
    }
}
