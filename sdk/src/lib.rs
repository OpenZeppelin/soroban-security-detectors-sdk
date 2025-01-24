#![warn(clippy::pedantic)]
use errors::SDKErr;
use std::hash::{BuildHasher, Hash};
use std::{cell::RefCell, collections::HashMap};

mod ast;
pub use ast::*;

mod codebase;
pub use codebase::*;
mod codebase_builder;

mod storage;
pub use storage::*;

pub mod errors;

pub(crate) mod utils;

struct SerializableHashMap<K, V, S>(HashMap<K, V, S>);

impl<K: Hash + Eq, V, S: BuildHasher> From<HashMap<K, V, S>> for SerializableHashMap<K, V, S> {
    fn from(map: HashMap<K, V, S>) -> Self {
        SerializableHashMap(map)
    }
}

/// Build a code model from the given `HashMap` { "file path" : "file content" }.
/// # Errors
/// - `SDKErr::AstParseError` If the file content cannot be parsed.
pub fn build_codebase<S: BuildHasher>(
    files: HashMap<String, String, S>,
) -> Result<RefCell<Codebase<SealedState>>, SDKErr> {
    let codebase = RefCell::new(Codebase::new());
    for (file, mut content) in files {
        codebase
            .borrow_mut()
            .parse_and_add_file(&file, &mut content)?;
    }
    let codebase = Codebase::build_api(codebase);
    Ok(codebase)
}

pub trait Rule {
    fn check(
        &self,
        codebase: &RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>>;

    fn name(&self) -> String;
    fn description(&self) -> String;
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
        let result = build_codebase(files_map);
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
        let codebase = build_codebase(files_map).unwrap().into_inner();
        assert_eq!(codebase.contracts().count(), 1);

        let files_map = get_files_map(vec![current_dir
            .join("multiple_contracts.rs")
            .to_str()
            .unwrap()
            .to_string()]);
        let codebase = build_codebase(files_map).unwrap().into_inner();
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
        let codebase = build_codebase(files_map).unwrap().into_inner();
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
