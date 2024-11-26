#![warn(clippy::pedantic)]
use codebase::{Codebase, SealedState};
use errors::SDKErr;
use std::{cell::RefCell, path::Path};

pub mod ast;
pub mod codebase;
pub mod errors;
/// Build the code model from the given files.
/// # Errors
/// - `SDKErr::SrcFileNotFound` If the file is not found.
/// - `std::io::Error` If there is an error reading the file.
/// - `SDKErr::AstParseError` If there is an error parsing the AST.
pub fn build_code_model(files: Vec<String>) -> Result<RefCell<Codebase<SealedState>>, SDKErr> {
    let codebase = RefCell::new(Codebase::new());
    for file in files {
        let path = Path::new(&file);
        if !path.exists() {
            return Err(SDKErr::SrcFileNotFound(file));
        }
        let mut content = std::fs::read_to_string(path)?;
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
        let files = vec![current_dir.join("account.rs").to_str().unwrap().to_string()];
        let result = build_code_model(files);
        assert!(result.is_ok());
    }

    #[test]
    fn test_build_code_model_file_not_found() {
        let current_dir = get_tests_dir_path();
        let files = vec![current_dir.join("AAA.rs").to_str().unwrap().to_string()];
        let result = build_code_model(files);
        assert!(result.is_err());
    }

    fn get_tests_dir_path() -> std::path::PathBuf {
        let current_dir = std::env::current_dir().unwrap();
        current_dir.join("tests")
    }
}
