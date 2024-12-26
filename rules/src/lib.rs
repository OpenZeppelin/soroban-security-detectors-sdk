use std::{cell::RefCell, collections::HashMap, rc::Rc};

use soroban_security_rules_sdk::{function::Function, node::Location, Codebase, SealedState};

pub trait Rule {
    fn check(
        &self,
        codebase: RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>>;
}

pub struct FileWithoutNoStd;

impl Rule for FileWithoutNoStd {
    fn check(
        &self,
        codebase: RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>> {
        let codebase = codebase.borrow();
        let mut errors = HashMap::new();
        for file in codebase.files() {
            if !file.has_no_std() {
                errors.insert(file.name().to_string(), vec![(0, 0)]);
            }
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }
}

pub struct ContractWithoutFunctions;

impl Rule for ContractWithoutFunctions {
    fn check(
        &self,
        codebase: RefCell<Codebase<SealedState>>,
    ) -> Option<HashMap<String, Vec<(usize, usize)>>> {
        let codebase = codebase.borrow();
        let mut errors = HashMap::new();
        for contract in codebase.contracts() {
            if contract
                .functions()
                .collect::<Vec<Rc<Function>>>()
                .is_empty()
            {
                errors.insert(
                    contract.name().to_string(),
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
}

pub fn all_rules() -> Vec<Box<dyn Rule>> {
    vec![
        Box::new(FileWithoutNoStd),
        Box::new(ContractWithoutFunctions),
    ]
}
