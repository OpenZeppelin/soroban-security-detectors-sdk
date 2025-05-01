use soroban_security_detectors_sdk::{Codebase, Detector, DetectorResult, SealedState};

pub struct FileWithoutNoStd;

impl Detector<Codebase<SealedState>> for FileWithoutNoStd {
    fn check(&self, codebase: &Codebase<SealedState>) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for file in codebase.files() {
            if !file.has_no_std() {
                errors.push(DetectorResult {
                    file_path: file.path.clone(),
                    offset_start: 0,
                    offset_end: 0,
                    extra: None,
                });
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

impl Detector<Codebase<SealedState>> for ContractWithoutFunctions {
    fn check(&self, codebase: &Codebase<SealedState>) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for contract in codebase.contracts() {
            if contract.methods.borrow().is_empty() {
                errors.push(DetectorResult {
                    file_path: contract.name.clone(),
                    offset_start: contract.location.offset_start,
                    offset_end: contract.location.offset_end,
                    extra: None,
                });
            }
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }
}

pub fn all_detectors() -> Vec<Box<dyn Detector<Codebase<SealedState>>>> {
    vec![
        Box::new(FileWithoutNoStd),
        Box::new(ContractWithoutFunctions),
    ]
}
