use std::rc::Rc;

use soroban_security_detectors_sdk::{
    definition::Definition,
    expression::{Assign, Expression, Identifier, MethodCall},
    node_type::NodeKind,
    statement::{Let, Statement},
    DetectorResult, SealedCodebase,
};

soroban_security_detectors_sdk::detector! {
    #[type_name = ExtendTtlWithMaxTtl]
    fn extend_ttl_with_max_ttl<SealedCodebase>(codebase: &SealedCodebase) -> Option<Vec<DetectorResult>> {
    let mut errors = Vec::new();
    for contract in codebase.contracts() {
        for function in contract
            .methods
            .borrow()
            .iter()
            .chain(contract.functions.borrow().iter())
        {
            for method_call in
                codebase.get_children_cmp_cast::<_, Rc<MethodCall>>(function.id, |n| {
                    if let NodeKind::Statement(Statement::Expression(Expression::MethodCall(mc))) = n {
                        if mc.method_name == "extend_ttl" {
                            return codebase.get_expression_type(mc.base.id()).name().starts_with("soroban_sdk::storage");
                        }
                    }
                    false
                })
            {
                if let Some(extend_to_param) = method_call.parameters.get(2) {
                    match extend_to_param {
                        Expression::Identifier(id) => {
                            if check_identifier(codebase, id) {
                                errors.push(DetectorResult {
                                    file_path: codebase
                                        .find_node_file(contract.id)
                                        .expect("Failed to find source file for the given contract ID")
                                        .path
                                        .clone(),
                                    offset_start: method_call.location.offset_start,
                                    offset_end: method_call.location.offset_end,
                                    extra: {
                                        let mut map = std::collections::HashMap::new();
                                        map.insert("CONTRACT_NAME".to_string(), contract.name.clone());
                                        map.insert("FUNCTION_NAME".to_string(), function.name.clone());
                                        Some(map)
                                    },
                                });
                            }
                        }
                        Expression::MethodCall(mc) => {
                            if check_method_call(codebase, mc) {
                                errors.push(DetectorResult {
                                    file_path: codebase
                                        .find_node_file(contract.id)
                                        .expect("Failed to find source file for the given contract ID")
                                        .path
                                        .clone(),
                                    offset_start: method_call.location.offset_start,
                                    offset_end: method_call.location.offset_end,
                                    extra: {
                                        let mut map = std::collections::HashMap::new();
                                        map.insert("CONTRACT_NAME".to_string(), contract.name.clone());
                                        map.insert("FUNCTION_NAME".to_string(), function.name.clone());
                                        Some(map)
                                    },
                                });
                            }
                        }
                        _ => {}
                    }
                } else {
                    panic!("Expected 'extend_ttl' to have 3 parameters.");
                }
            }
        }
    }
    if errors.is_empty() {
        None
    } else {
        Some(errors)
    }
}
}

fn check_identifier(codebase: &SealedCodebase, id: &Rc<Identifier>) -> bool {
    let scope_container = codebase
        .get_parent_container(id.id)
        .expect("Identifier has no scope container");

    match scope_container {
        NodeKind::Definition(Definition::Function(function))
        | NodeKind::Statement(Statement::Definition(Definition::Function(function))) => {
            // identifier can be defined as a variable in the scope
            let let_stmts: Vec<_> = codebase
                .get_children_cmp_cast::<_, Rc<Let>>(function.id, |n| {
                    matches!(n, NodeKind::Statement(Statement::Let(_)))
                })
                .into_iter()
                .filter(|l| l.name == id.name)
                .collect();
            for let_stmt in &let_stmts {
                if let Some(initial_value) = &let_stmt.initial_value {
                    match initial_value {
                        Expression::MethodCall(method_call) => {
                            if check_method_call(codebase, method_call) {
                                return true;
                            }
                        }
                        Expression::Identifier(id) => {
                            if check_identifier(codebase, id) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
            }

            // identifier can be re-assigned in the scope
            let assign_exprs = codebase
                .get_children_cmp_cast::<_, Rc<Assign>>(function.id, |n| {
                    matches!(n, NodeKind::Expression(Expression::Assign(_)))
                })
                .into_iter()
                .filter(|a| {
                    if let Expression::Identifier(ident) = &a.left {
                        ident.name == id.name
                            && matches!(
                                &a.right,
                                Expression::MethodCall(m) if m.method_name == "max_ttl"
                            )
                    } else {
                        false
                    }
                })
                .collect::<Vec<_>>();
            for assign in &assign_exprs {
                if let Expression::MethodCall(method_call) = &assign.right {
                    if !check_method_call(codebase, method_call) {
                        return false;
                    }
                } else if let Expression::Identifier(id) = &assign.right {
                    if !check_identifier(codebase, id) {
                        return false;
                    }
                }
            }
        }
        _ => {}
    }
    false
}

fn check_method_call(codebase: &SealedCodebase, method_call: &Rc<MethodCall>) -> bool {
    if method_call.method_name == "max_ttl"
        && codebase
            .get_expression_type(method_call.base.id())
            .name()
            .starts_with("soroban_sdk::storage::")
    {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use soroban_security_detectors_sdk::build_codebase;
    use std::collections::HashMap;

    #[test]
    fn test_extend_ttl_with_max_ttl_1() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Address, Env, Symbol, Vec};
        #[contract]
        pub struct TTLExamples;

        #[contractimpl]
        impl TTLExamples {
            
            // 1. BASIC DATA TTL EXTENSION
            // Use: Standard data storage with regular access patterns
            pub fn basic_extend_data(env: Env, key: Symbol, days: u32) {
                let ledgers = days * 17280; // ~17280 ledgers per day
                let max_ttl = env.storage().persistent().max_ttl();
                let extension = ledgers.min(max_ttl);
                
                env.storage().persistent().extend_ttl(
                    &key,
                    extension,
                    max_ttl
                );
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some(), "Expected some results, but got none");
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 599);
        assert_eq!(detector_result.offset_end, 740);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "TTLExamples".to_string());
                map.insert("FUNCTION_NAME".to_string(), "basic_extend_data".to_string());
                map
            })
        );
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_2() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Address, Env, Symbol, Vec};
        #[contract]
        pub struct TTLExamples;

        #[contractimpl]
        impl TTLExamples {
            
            pub fn extend_data(env: Env, key: Symbol, days: u32) {
                let ledgers = days * 17280;
                let max_ttl = env.storage().persistent().max_ttl();
                let extension = ledgers.min(max_ttl);
                
                env.storage().persistent().extend_ttl(
                    &key,
                    extension,
                    env.storage().persistent().max_ttl()
                );
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some(), "Expected some results, but got none");
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 453);
        assert_eq!(detector_result.offset_end, 623);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "TTLExamples".to_string());
                map.insert("FUNCTION_NAME".to_string(), "extend_data".to_string());
                map
            })
        );
    }
}
