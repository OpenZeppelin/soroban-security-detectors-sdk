use std::collections::HashMap;

use rules::{ContractWithoutFunctions, Rule};
use soroban_security_rules_sdk::build_code_model;

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
    let codebase = build_code_model(data).unwrap();
    let res = ContractWithoutFunctions.check(codebase);
    if let Some(errors) = res {
        for (contract_name, locations) in errors.iter() {
            for (line, col) in locations.iter() {
                println!(
                    "Contract {} has no functions defined at line {} and column {}",
                    contract_name, line, col
                );
            }
        }
    } else {
        println!("All contracts have functions defined");
    }
}
