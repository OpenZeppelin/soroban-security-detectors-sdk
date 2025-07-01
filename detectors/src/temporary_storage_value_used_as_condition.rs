use std::rc::Rc;

use soroban_security_detectors_sdk::{
    definition::Definition,
    expression::{Expression, FunctionCall, Identifier, If, MethodCall},
    node_type::NodeKind,
    statement::Statement,
    DetectorResult, SealedCodebase,
};
soroban_security_detectors_sdk::detector! {
    #[type_name = TemporaryStorageValueUsedAsCondition]
    fn temporary_storage_value_used_as_condition<SealedCodebase>(
        codebase: &SealedCodebase,
    ) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for contract in codebase.contracts() {
            for function in contract
                .methods
                .borrow()
                .iter()
                .chain(contract.functions.borrow().iter())
            {
                for if_cond in codebase.get_children_cmp_cast::<_, Rc<If>>(function.id, |n| {
                    matches!(
                        n,
                        NodeKind::Statement(Statement::Expression(Expression::If(_)))
                    )
                }) {
                    match &if_cond.condition {
                        Expression::MethodCall(method_call) => {
                            if check_method_call(codebase, method_call) {
                                errors.push(DetectorResult {
                                    file_path: codebase
                                        .find_node_file(contract.id)
                                        .expect("Failed to find source file for the given contract ID")
                                        .path
                                        .clone(),
                                    offset_start: function.location.offset_start,
                                    offset_end: function.location.offset_end,
                                    extra: {
                                        let mut map = std::collections::HashMap::new();
                                        map.insert("CONTRACT_NAME".to_string(), contract.name.clone());
                                        map.insert("FUNCTION_NAME".to_string(), function.name.clone());
                                        Some(map)
                                    },
                                });
                            }
                        }
                        Expression::Identifier(id) => {
                            if check_identifier(codebase, id) {
                                errors.push(DetectorResult {
                                    file_path: codebase
                                        .find_node_file(contract.id)
                                        .expect("Failed to find source file for the given contract ID")
                                        .path
                                        .clone(),
                                    offset_start: function.location.offset_start,
                                    offset_end: function.location.offset_end,
                                    extra: {
                                        let mut map = std::collections::HashMap::new();
                                        map.insert("CONTRACT_NAME".to_string(), contract.name.clone());
                                        map.insert("FUNCTION_NAME".to_string(), function.name.clone());
                                        Some(map)
                                    },
                                });
                            }
                        }
                        Expression::FunctionCall(func_call) => {
                            let scope_id = codebase.get_parent_container(func_call.id)
                                .expect("Function call has no scope container")
                                .id();
                            if check_function_call(codebase, func_call, scope_id) {
                                errors.push(DetectorResult {
                                    file_path: codebase
                                        .find_node_file(contract.id)
                                        .expect("Failed to find source file for the given contract ID")
                                        .path
                                        .clone(),
                                    offset_start: function.location.offset_start,
                                    offset_end: function.location.offset_end,
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

fn check_method_call(codebase: &SealedCodebase, method_call: &Rc<MethodCall>) -> bool {
    if method_call.method_name == "has"
        && codebase.get_expression_type(method_call.base.id()).name()
            == "soroban_sdk::storage::Temporary"
    {
        return true;
    }
    false
}

fn check_identifier(codebase: &SealedCodebase, id: &Rc<Identifier>) -> bool {
    let scope_container = codebase
        .get_parent_container(id.id)
        .expect("Identifier has no scope container");

    let symbol_orgin = codebase
        .lookup_symbol_origin(scope_container.id(), id.name.as_str())
        .expect("Identifier has no symbol origin");

    match symbol_orgin {
        NodeKind::Statement(Statement::Let(let_stmt)) => {
            if let Some(initial_value) = &let_stmt.initial_value {
                match initial_value {
                    Expression::MethodCall(method_call) => {
                        if check_method_call(codebase, method_call) {
                            return true;
                        }
                    }
                    Expression::FunctionCall(func_call) => {
                        if check_function_call(codebase, func_call, scope_container.id()) {
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
        NodeKind::Statement(Statement::Expression(Expression::Assign(assign))) => {
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
        _ => {}
    }
    false
}

fn check_function_call(
    codebase: &SealedCodebase,
    function_call: &Rc<FunctionCall>,
    scope_id: u32,
) -> bool {
    if let Some(Definition::Function(func)) =
        codebase.get_function_by_name(scope_id, function_call.function_name.as_str())
    {
        if func.returns.borrow().name() != "bool" {
            return false;
        }
        if let Some(body) = &func.body {
            for stmt in body.statements.iter().rev() {
                if let Statement::Expression(e) = stmt {
                    if !e.is_ret() {
                        continue;
                    }
                    match e {
                        Expression::MethodCall(mc) => {
                            if check_method_call(codebase, mc) {
                                return true;
                            }
                        }
                        Expression::FunctionCall(fc) => {
                            if check_function_call(codebase, fc, scope_id) {
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
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use soroban_security_detectors_sdk::build_codebase;
    use std::collections::HashMap;

    #[test]
    fn test_temporary_storage_value_used_as_condition_1() {
        let detector = TemporaryStorageValueUsedAsCondition;
        let src = r#"#![no_std]
        use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};
        
        #[contract]
        pub struct ContractContract;

        #[contractimpl]
        impl ContractContract {
            pub fn way_aaa(env: Env, to: Symbol) -> Vec<Symbol> {
                let storage = env.storage();
                let key = symbol_short!("key");
                if storage.temporary().has(&key) {
                    let value: Vec<Symbol> = storage.temporary().get(&key).unwrap();
                    return value;
                }
                vec![]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 229);
        assert_eq!(detector_result.offset_end, 600);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "ContractContract".to_string());
                map.insert("FUNCTION_NAME".to_string(), "way_aaa".to_string());
                map
            })
        );
    }

    #[test]
    fn test_temporary_storage_value_used_as_condition_2() {
        let detector = TemporaryStorageValueUsedAsCondition;
        let src = r#"#![no_std]
        use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};
        
        #[contract]
        pub struct ContractContract;

        #[contractimpl]
        impl ContractContract {
            pub fn way_aaa(env: Env, to: Symbol) -> Vec<Symbol> {
                let storage = env.storage();
                let key = symbol_short!("key");
                let condition = storage.temporary().has(&key);
                if condition {
                    let value: Vec<Symbol> = storage.temporary().get(&key).unwrap();
                    return value;
                }
                vec![]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 229);
        assert_eq!(detector_result.offset_end, 643);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "ContractContract".to_string());
                map.insert("FUNCTION_NAME".to_string(), "way_aaa".to_string());
                map
            })
        );
    }

    #[test]
    fn test_temporary_storage_value_used_as_condition_3() {
        let detector = TemporaryStorageValueUsedAsCondition;
        let src = r#"#![no_std]
        use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};
        
        #[contract]
        pub struct ContractContract;

        fn get_condition(env: &Env, key: &Symbol) -> bool {
            let storage = env.storage();
            storage.temporary().has(key)
        }

        #[contractimpl]
        impl ContractContract {
            pub fn way_aaa(env: Env, to: Symbol) -> Vec<Symbol> {
                let storage = env.storage();
                let key = symbol_short!("key");
                let condition = get_condition(&env, &key);
                if condition {
                    let value: Vec<Symbol> = storage.temporary().get(&key).unwrap();
                    return value;
                }
                vec![]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 382);
        assert_eq!(detector_result.offset_end, 792);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "ContractContract".to_string());
                map.insert("FUNCTION_NAME".to_string(), "way_aaa".to_string());
                map
            })
        );
    }

    #[test]
    fn test_temporary_storage_value_used_as_condition_4() {
        let detector = TemporaryStorageValueUsedAsCondition;
        let src = r#"#![no_std]
        use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};
        
        #[contract]
        pub struct ContractContract;

        fn get_condition(env: &Env, key: &Symbol) -> bool {
            let storage = env.storage();
            storage.temporary().has(key)
        }

        #[contractimpl]
        impl ContractContract {
            pub fn way_aaa(env: Env, to: Symbol) -> Vec<Symbol> {
                let storage = env.storage();
                let key = symbol_short!("key");
                if get_condition(&env, &key) {
                    let value: Vec<Symbol> = storage.temporary().get(&key).unwrap();
                    return value;
                }
                vec![]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let detector_result = errors.first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 382);
        assert_eq!(detector_result.offset_end, 749);
        assert_eq!(
            detector_result.extra,
            Some({
                let mut map = HashMap::new();
                map.insert("CONTRACT_NAME".to_string(), "ContractContract".to_string());
                map.insert("FUNCTION_NAME".to_string(), "way_aaa".to_string());
                map
            })
        );
    }
}
