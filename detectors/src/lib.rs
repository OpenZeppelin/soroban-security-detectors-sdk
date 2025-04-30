use std::{cell::RefCell, collections::HashMap};

use soroban_security_detectors_sdk::{node::TLocation, Codebase, Detector, SealedState};

pub struct FileWithoutNoStd;

impl Detector for FileWithoutNoStd {
    fn check(
        &self,
        codebase: &RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>> {
        let codebase = codebase.borrow();
        let mut errors = HashMap::new();
        for file in codebase.files() {
            if !file.has_no_std() {
                errors.insert(file.name.to_string(), vec![(0, 0)]);
            }
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    fn name(&self) -> String {
        "FileWithoutNoStd".to_string()
    }

    fn description(&self) -> String {
        "File must have #[no_std] attribute".to_string()
    }
}

pub struct ContractWithoutFunctions;

impl Detector for ContractWithoutFunctions {
    fn check(
        &self,
        codebase: &RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>> {
        let codebase = codebase.borrow();
        let mut errors = HashMap::new();
        for contract in codebase.contracts() {
            if contract.methods.borrow().is_empty() {
                errors.insert(
                    contract.as_ref().name.clone(),
                    vec![(contract.start_line(), contract.start_col())],
                );
            }
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    //TODO think of adding a macro to generate this automatically
    fn name(&self) -> String {
        "ContractWithoutFunctions".to_string()
    }

    fn description(&self) -> String {
        "Contract should have at least one function".to_string()
    }
}

pub fn all_detectors() -> Vec<Box<dyn Detector>> {
    vec![
        Box::new(FileWithoutNoStd),
        Box::new(ContractWithoutFunctions),
    ]
}

#[cfg(test)]
mod tests {
    use soroban_security_detectors_sdk::build_codebase;

    use super::*;

    #[test]
    fn test_file_without_no_std() {
        let detector = FileWithoutNoStd;
        let contract_content = r#"
            #[contract]
            struct AccountContract;
        "#;

        let mut data = HashMap::new();
        data.insert(
            "contract_without_functions.rs".to_string(),
            contract_content.to_string(),
        );
        let codebase = build_codebase(data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_contract_without_functions() {
        let detector = ContractWithoutFunctions;
        let contract_content = r#"
            #[contract]
            struct AccountContract;
        "#;

        let mut data = HashMap::new();
        data.insert(
            "contract_without_functions.rs".to_string(),
            contract_content.to_string(),
        );
        let codebase = build_codebase(data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_file_without_no_std_name() {
        let detector = FileWithoutNoStd;
        assert_eq!(detector.name(), "FileWithoutNoStd");
    }

    #[test]
    fn test_file_without_no_std_description() {
        let detector = FileWithoutNoStd;
        assert_eq!(detector.description(), "File must have #[no_std] attribute");
    }

    #[test]
    fn test_contract_without_functions_name() {
        let detector = ContractWithoutFunctions;
        assert_eq!(detector.name(), "ContractWithoutFunctions");
    }

    #[test]
    fn test_contract_without_functions_description() {
        let detector = ContractWithoutFunctions;
        assert_eq!(
            detector.description(),
            "Contract should have at least one function"
        );
    }

    #[test]
    fn test_all_detectors() {
        let detectors = all_detectors();
        assert_eq!(detectors.len(), 2);
    }
}
