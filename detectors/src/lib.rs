#![warn(clippy::pedantic)]
include!(concat!(env!("OUT_DIR"), "/mod_includes.rs"));
include!(concat!(env!("OUT_DIR"), "/detector_report_templates.rs"));
include!(concat!(env!("OUT_DIR"), "/register.rs"));

#[cfg(test)]
mod test {
    use soroban_security_detectors_sdk::build_codebase;
    use soroban_security_detectors_sdk::expression::{Expression, If};
    use soroban_security_detectors_sdk::node_type::NodeKind;
    use soroban_security_detectors_sdk::statement::{Let, Statement};
    use soroban_security_detectors_sdk::{definition::Definition, expression::Assign};
    use std::collections::HashMap;
    use std::rc::Rc;

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
            identifiers.sort_by_key(|id| id.location.start_line);
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
            pub fn way(env: Env, to: Symbol) -> Vec<Symbol> {
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
        data.insert("test.rs".to_string(), src.to_string());
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
            identifiers.sort_by_key(|id| id.location.start_line);
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
