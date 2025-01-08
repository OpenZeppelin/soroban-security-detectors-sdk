use std::collections::HashMap;

use soroban_security_rules::all_rules;
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
    for rule in all_rules() {
        let rule_result = rule.check(&codebase);
        if let Some(errors) = rule_result {
            for (contract_name, locations) in errors.iter() {
                for (line, col) in locations.iter() {
                    println!(
                        "In {contract_name} rule: {} detected an error at [{line}:{col}]",
                        rule.name()
                    );
                }
            }
        }
    }
}
