use std::{cell::RefCell, collections::HashMap};

use soroban_security_rules_sdk::{node::TLocation, Codebase, Rule, SealedState};

pub struct FileWithoutNoStd;

impl Rule for FileWithoutNoStd {
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

impl Rule for ContractWithoutFunctions {
    fn check(
        &self,
        codebase: &RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>> {
        let codebase = codebase.borrow();
        let mut errors = HashMap::new();
        for contract in codebase.contracts() {
            if contract.methods.borrow().is_empty() {
                errors.insert(
                    contract.name.to_string(),
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

pub fn all_rules() -> Vec<Box<dyn Rule>> {
    vec![
        Box::new(FileWithoutNoStd),
        Box::new(ContractWithoutFunctions),
    ]
}
