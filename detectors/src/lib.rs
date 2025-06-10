#![warn(clippy::pedantic)]
include!(concat!(env!("OUT_DIR"), "/mod_includes.rs"));
include!(concat!(env!("OUT_DIR"), "/detector_report_templates.rs"));
include!(concat!(env!("OUT_DIR"), "/register.rs"));

#[cfg(test)]
mod test {
    use soroban_security_detectors_sdk::build_codebase;
    use soroban_security_detectors_sdk::expression::Expression;
    use soroban_security_detectors_sdk::node_type::NodeKind;
    use soroban_security_detectors_sdk::statement::Statement;
    use std::collections::HashMap;

    #[test]
    fn test_1() {
        let src = r#"#![no_std]
        use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};
        
        #[contract]
        pub struct ContractContract;

        #[contractimpl]
        impl ContractContract {
            pub fn way(env: Env, to: Symbol) -> Vec<Symbol> {
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        for function in contract.functions().chain(contract.methods()) {
            let expanded_function = codebase.inline_function(function);
            let children = codebase.get_children_cmp(expanded_function.id, |n| {
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
                ("env", "Env"),
                ("storage", "soroban_sdk::Storage"),
                ("storage", "soroban_sdk::Storage"),
                ("value", "Vec<Symbol>"),
            ];
            assert_eq!(identifiers.len(), expected.len());
            for ((_, exp_ty), identifier) in expected.iter().zip(identifiers.iter()) {
                let ty = codebase
                    .get_symbol_type(expanded_function.id, &identifier.name)
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
        }
    }
}
