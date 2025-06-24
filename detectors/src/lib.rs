#![warn(clippy::pedantic)]
include!(concat!(env!("OUT_DIR"), "/mod_includes.rs"));
include!(concat!(env!("OUT_DIR"), "/detector_report_templates.rs"));
include!(concat!(env!("OUT_DIR"), "/register.rs"));

use std::rc::Rc;

use soroban_security_detectors_sdk::{
    definition::Definition,
    expression::{Assign, Expression, Identifier, If, MethodCall},
    node_type::NodeKind,
    statement::{Let, Statement},
    DetectorResult, SealedCodebase,
};

#[allow(dead_code, clippy::too_many_lines)]
fn temporary_storage_value_used_as_condition(
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

fn check_method_call(codebase: &SealedCodebase, method_call: &Rc<MethodCall>) -> bool {
    if method_call.method_name == "has" {
        if let Some(base_type) = codebase.get_expression_type(method_call.base.id()) {
            if base_type.name() == "soroban_sdk::storage::Temporary" {
                return true;
            }
        }
    }
    false
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
                                Expression::MethodCall(m) if m.method_name == "has"
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
            // assert!(
            //     assign_exprs.iter().any(|assign| {
            //         if let Expression::Identifier(ident) = &assign.left {
            //             ident.name == id.name
            //                 && matches!(
            //                     &assign.right,
            //                     Expression::MethodCall(m) if m.method_name == "has"
            //                 )
            //         } else {
            //             false
            //         }
            //     }),
            //     "Expected variable '{}' to be re-assigned by 'has'",
            //     id.name
            // );

            // identifier can be defined as a function parameter
            // assert!(
            //     function.parameters().any(|param| param.name == id.name),
            //     "Expected '{}' to be a function parameter",
            //     id.name
            // );
        }
        _ => {}
    }
    false
}

#[cfg(test)]
mod test {
    use soroban_security_detectors_sdk::build_codebase;
    use soroban_security_detectors_sdk::expression::{Expression, If};
    use soroban_security_detectors_sdk::node_type::NodeKind;
    use soroban_security_detectors_sdk::statement::{Let, Statement};
    use soroban_security_detectors_sdk::{definition::Definition, expression::Assign};
    use std::collections::HashMap;
    use std::rc::Rc;

    use crate::temporary_storage_value_used_as_condition;

    #[test]
    fn test_temporary_storage_value_used_as_condition_1() {
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
        let result = temporary_storage_value_used_as_condition(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
    }

    #[test]
    fn test_temporary_storage_value_used_as_condition_2() {
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
        let result = temporary_storage_value_used_as_condition(&codebase);
        assert!(result.is_some());
        let errors = result.unwrap();
        assert_eq!(errors.len(), 1, "{errors:?}");
    }

    #[test]
    fn test_1() {
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
        let contract = codebase.contracts().next().unwrap();
        for function in contract.functions().chain(contract.methods()) {
            let children = codebase.get_children_cmp(function.id, |n| {
                matches!(n, NodeKind::Expression(Expression::Identifier(_)))
                    || matches!(
                        n,
                        NodeKind::Statement(Statement::Expression(Expression::Identifier(_)))
                    )
            });
            let mut identifiers = children
                .iter()
                .map(|n| match n {
                    NodeKind::Statement(Statement::Expression(Expression::Identifier(
                        identifier,
                    )))
                    | NodeKind::Expression(Expression::Identifier(identifier)) => identifier,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            identifiers.sort_by(|a, b| {
                a.location
                    .start_line
                    .cmp(&b.location.start_line)
                    .then_with(|| a.name.cmp(&b.name))
            });
            let expected = [
                ("env", "soroban_sdk::Env"),
                ("storage", "soroban_sdk::storage::Storage"),
                ("storage", "soroban_sdk::storage::Storage"),
                ("value", "Vec<Symbol>"),
            ];
            assert_eq!(identifiers.len(), expected.len());
            for ((_, exp_ty), identifier) in expected.iter().zip(identifiers.iter()) {
                let ty = codebase
                    .get_symbol_type(function.id, &identifier.name)
                    .unwrap_or_else(|| panic!("Symbol {} has no inferred type", identifier.name));
                assert_eq!(
                    ty.name(),
                    *exp_ty,
                    "Identifier '{}' expected type {} but found {:?}",
                    identifier.name,
                    exp_ty,
                    ty
                );
            }
            for if_cond in codebase.get_children_cmp_cast::<_, Rc<If>>(function.id, |n| {
                matches!(n, NodeKind::Expression(Expression::If(_)))
            }) {
                if let Expression::MethodCall(method_call) = &if_cond.condition {
                    if method_call.method_name == "has" {
                        if let Expression::MethodCall(inner) = &method_call.base {
                            if inner.method_name == "temporary" {
                                let base_type = codebase
                                    .get_expression_type(inner.base.id())
                                    .expect("Base symbol has no inferred type");
                                assert_eq!(
                                    base_type.name(),
                                    "Storage",
                                    "Expected base type of 'temporary' to be 'Storage', found {base_type:?}"
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_2() {
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
        let contract = codebase.contracts().next().unwrap();
        for function in contract.functions().chain(contract.methods()) {
            let children = codebase.get_children_cmp(function.id, |n| {
                matches!(n, NodeKind::Expression(Expression::Identifier(_)))
                    || matches!(
                        n,
                        NodeKind::Statement(Statement::Expression(Expression::Identifier(_)))
                    )
            });
            let mut identifiers = children
                .iter()
                .map(|n| match n {
                    NodeKind::Statement(Statement::Expression(Expression::Identifier(
                        identifier,
                    )))
                    | NodeKind::Expression(Expression::Identifier(identifier)) => identifier,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            identifiers.sort_by(|a, b| {
                a.location
                    .start_line
                    .cmp(&b.location.start_line)
                    .then_with(|| a.name.cmp(&b.name))
            });
            let expected = [
                ("env", "soroban_sdk::Env"),
                ("storage", "soroban_sdk::storage::Storage"),
                ("condition", "bool"),
                ("storage", "soroban_sdk::storage::Storage"),
                ("value", "Vec<Symbol>"),
            ];
            assert_eq!(identifiers.len(), expected.len());
            for ((_, exp_ty), identifier) in expected.iter().zip(identifiers.iter()) {
                let ty = codebase
                    .get_symbol_type(function.id, &identifier.name)
                    .expect("Symbol has no inferred type");
                assert_eq!(
                    ty.name(),
                    *exp_ty,
                    "Identifier '{}' expected type {} but found {:?}",
                    identifier.name,
                    exp_ty,
                    ty
                );
            }
            for if_cond in codebase.get_children_cmp_cast::<_, Rc<If>>(function.id, |n| {
                matches!(n, NodeKind::Expression(Expression::If(_)))
            }) {
                match &if_cond.condition {
                    Expression::MethodCall(method_call) => {
                        if method_call.method_name == "has" {
                            if let Expression::MethodCall(inner) = &method_call.base {
                                if inner.method_name == "temporary" {
                                    let base_type = codebase
                                        .get_expression_type(inner.base.id())
                                        .expect("Base symbol has no inferred type");
                                    assert_eq!(
                                        base_type.name(),
                                        "Storage",
                                        "Expected base type of 'temporary' to be 'Storage', found {base_type:?}"
                                    );
                                }
                            }
                        }
                    }
                    Expression::Identifier(id) => {
                        let scope_container = codebase
                            .get_parent_container(id.id)
                            .expect("Identifier has no scope container");

                        match scope_container {
                            NodeKind::Definition(Definition::Function(function))
                            | NodeKind::Statement(Statement::Definition(Definition::Function(
                                function,
                            ))) => {
                                // identifier can be defined as a variable in the scope
                                let let_stmts = codebase
                                    .get_children_cmp_cast::<_, Rc<Let>>(function.id, |n| {
                                        matches!(n, NodeKind::Statement(Statement::Let(_)))
                                    });
                                assert!(
                                    let_stmts.iter().any(|let_stmt| {
                                        let_stmt.name == id.name
                                            && let_stmt.initial_value.as_ref().is_some_and(|e| {
                                                matches!(e, Expression::MethodCall(m) if m.method_name == "has")
                                            })
                                    }),
                                    "Expected variable '{}' to be initialized by 'has'",
                                    id.name
                                );

                                // identifier can be re-assigned in the scope
                                let assign_exprs = codebase
                                    .get_children_cmp_cast::<_, Rc<Assign>>(function.id, |n| {
                                        matches!(n, NodeKind::Expression(Expression::Assign(_)))
                                    });
                                assert!(
                                    assign_exprs.iter().any(|assign| {
                                        if let Expression::Identifier(ident) = &assign.left {
                                            ident.name == id.name
                                                && matches!(
                                                    &assign.right,
                                                    Expression::MethodCall(m) if m.method_name == "has"
                                                )
                                        } else {
                                            false
                                        }
                                    }),
                                    "Expected variable '{}' to be re-assigned by 'has'",
                                    id.name
                                );

                                // identifier can be defined as a function parameter
                                assert!(
                                    function.parameters().any(|param| param.name == id.name),
                                    "Expected '{}' to be a function parameter",
                                    id.name
                                );
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}
