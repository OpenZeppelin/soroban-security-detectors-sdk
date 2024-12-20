#![warn(clippy::pedantic)]
use errors::SDKErr;
use serde::Serialize;
use std::hash::{BuildHasher, Hash};
use std::{cell::RefCell, collections::HashMap};

mod ast;
pub use ast::*;

mod codebase;
pub use codebase::*;

pub mod errors;

#[derive(Serialize)]
struct SerializableHashMap<K, V, S>(HashMap<K, V, S>);

impl<K: Hash + Eq, V, S: BuildHasher> From<HashMap<K, V, S>> for SerializableHashMap<K, V, S> {
    fn from(map: HashMap<K, V, S>) -> Self {
        SerializableHashMap(map)
    }
}

/// Build the code model from the given `HashMap` { "file path" : "file content" }.
/// # Errors
/// - `SDKErr::AstParseError` If the file content cannot be parsed.
pub fn build_code_model<S: BuildHasher>(
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
        let result = build_code_model(files_map);
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
        let codebase = build_code_model(files_map).unwrap().into_inner();
        assert_eq!(codebase.contracts().count(), 1);

        let files_map = get_files_map(vec![current_dir
            .join("multiple_contracts.rs")
            .to_str()
            .unwrap()
            .to_string()]);
        let codebase = build_code_model(files_map).unwrap().into_inner();
        assert_eq!(codebase.contracts().count(), 2);
    }

    fn get_tests_dir_path() -> std::path::PathBuf {
        let current_dir = std::env::current_dir().unwrap();
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
