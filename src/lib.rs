#![warn(clippy::pedantic)]
use std::path::Path;

pub struct SDKErr {
    pub message: String,
}

/// Build the code model from the given files.
/// # Errors
/// - If the file is not found.
pub fn build_code_model(files: Vec<String>) -> Result<bool, SDKErr> {
    for file in files {
        let path = Path::new(&file);
        if !path.exists() {
            return Err(SDKErr {
                message: format!("File not found: {file}"),
            });
        }
    }
    Ok(true)
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
