/// Detector for contracts without any methods.
///
/// Flags Soroban contract types that define no public methods, indicating potentially
/// empty or incomplete contract implementations.
use soroban_security_detectors_sdk::{DetectorResult, SealedCodebase};

soroban_security_detectors_sdk::detector! {
    #[type_name = ContractWithoutFunctions]
    fn contract_without_functions<SealedCodebase>(
        codebase: &SealedCodebase,
    ) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for contract in codebase.contracts() {
            if contract.methods.borrow().is_empty() {
                errors.push(DetectorResult {
                    file_path: codebase.find_node_file(contract.id).unwrap().path.clone(),
                    offset_start: contract.location.offset_start,
                    offset_end: contract.location.offset_end,
                    extra: {
                        let mut map = std::collections::HashMap::new();
                        map.insert("CONTRACT_NAME".to_string(), contract.name.clone());
                        Some(map)
                    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use soroban_security_detectors_sdk::build_codebase;
    use std::collections::HashMap;

    #[test]
    fn test_contract_without_functions_1() {
        let detector = ContractWithoutFunctions;
        let src = "#![no_std]
#![allow(clippy::all)]

use soroban_sdk::contract;

#[contract]
#[allow(dead_code)]
struct Contract1;";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 63);
        assert_eq!(detector_result.offset_end, 112);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract1".to_string());
            Some(map)
        });
    }

    #[test]
    fn test_contract_without_functions_2() {
        let detector = ContractWithoutFunctions;
        let src = "#![no_std]
use soroban_sdk::contract;

#[contract]
struct Contract1 {
    field: u32,
}

impl Contract1 {
    fn get_field(&self) -> Self {
        self.field
    }
}
";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_none());
    }
}
