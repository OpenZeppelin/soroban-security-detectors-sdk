use std::collections::HashMap;

use soroban_security_detectors::all_detectors;
use soroban_security_detectors_sdk::{build_codebase, Codebase, Detector, SealedState};

fn main() {
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
    let mut detectors = all_detectors();
    detectors.extend(custom_detectors());
    for detector in detectors {
        let detector_result = detector.check(&codebase);
        if let Some(errors) = detector_result {}
    }
}

#[allow(clippy::let_and_return, unused_mut)]
fn custom_detectors<T>() -> Vec<Box<dyn Detector<T>>> {
    let mut detectors: Vec<Box<dyn Detector<T>>> = Vec::new();
    //Import and add your detectors here
    detectors
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main() {
        main();
    }
}
