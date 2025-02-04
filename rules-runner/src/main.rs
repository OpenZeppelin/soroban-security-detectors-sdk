use std::collections::HashMap;

use soroban_security_rules::all_rules;
use soroban_security_rules_sdk::{build_codebase, Rule};

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
    let mut rules = all_rules();
    rules.extend(custom_rules());
    for rule in rules {
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

#[allow(clippy::let_and_return, unused_mut)]
fn custom_rules() -> Vec<Box<dyn Rule>> {
    let mut rules: Vec<Box<dyn Rule>> = Vec::new();
    //Import and add your rules here
    rules
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main() {
        main();
    }
}