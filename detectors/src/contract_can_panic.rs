use soroban_security_detectors_sdk::{
    expression::Expression, function::Function, node::Node, node_type::NodeKind,
    statement::Statement, DetectorResult, SealedCodebase,
};

soroban_security_detectors_sdk::detector! {
     #[type_name = ContractCanPanic]
    fn contract_can_panic<SealedCodebase>(
        codebase: &SealedCodebase,
    ) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for contract in codebase.contracts() {
            for function in contract.methods.borrow().iter().chain(
                contract.functions.borrow().iter(),
            ) {
                if !function.parameters().any(|p| p.type_name == "Env") {
                    continue;
                }
                let func = codebase.inline_function(function);
                if can_panic(&func) {
                    errors.push(DetectorResult {
                        file_path: codebase.find_node_file(contract.id)
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
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }
}

fn can_panic(function: &Function) -> bool {
    let mut stack: Vec<NodeKind> = Vec::new();
    if let Some(body) = &function.body {
        for stmt in &body.statements {
            stack.push(NodeKind::Statement(stmt.clone()));
        }
    }
    while let Some(node) = stack.pop() {
        if let NodeKind::Statement(stmt) = node {
            match stmt {
                Statement::Macro(mac)
                    if ["panic", "assert", "unreachable"].contains(&mac.name.as_str()) =>
                {
                    return true;
                }
                Statement::Expression(expr) => {
                    if let Expression::Macro(mac) = &expr {
                        if ["panic", "assert", "unreachable"].contains(&mac.name.as_str()) {
                            return true;
                        }
                    }
                    if let Expression::MemberAccess(ma) = &expr {
                        if ["unwrap", "expect"].contains(&ma.member_name.as_str()) {
                            return true;
                        }
                    } else if let Expression::MethodCall(mc) = &expr {
                        if ["unwrap", "expect"].contains(&mc.method_name.as_str()) {
                            return true;
                        }
                    }
                    for child in expr.children() {
                        stack.push(child);
                    }
                }
                Statement::Block(block) => {
                    for s in &block.statements {
                        stack.push(NodeKind::Statement(s.clone()));
                    }
                }
                Statement::Let(let_stmt) => {
                    for child in let_stmt.children() {
                        stack.push(child);
                    }
                }
                _ => {}
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
    fn contract_panics_1() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]
        
        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env, to: Symbol) -> Vec<Symbol> {
                panic!("This is a panic");
                let v = to.name().unwrap().to_vec();
                v.expect("This should not panic");
                vec![to]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 130);
        assert_eq!(detector_result.offset_end, 367);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_2() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]
        
        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env, to: Symbol) -> Vec<Symbol> {
                let v = to.name().unwrap().to_vec();
                v.expect("This should not panic");
                vec![to]
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 130);
        assert_eq!(detector_result.offset_end, 324);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_3() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]
        
        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {

            pub fn here() -> Vec<Symbol> {
                let v = vec![Symbol::from("test")];
                v.expect("This should not panic");
                v
            }

            pub fn hello(env: Env, to: Symbol) -> Vec<Symbol> {
                here()
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 310);
        assert_eq!(detector_result.offset_end, 398);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_4_unwrap_only() {
        let detector = ContractCanPanic;
        let src = r"#![no_std]
        
        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env, to: Symbol) -> Vec<Symbol> {
                let name = to.name().unwrap();
                vec![to]
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 130);
        assert_eq!(detector_result.offset_end, 267);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_5_indirect_panic() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]
        
        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn helper() {
                panic!("Oops");
            }

            pub fn hello(env: Env, to: Symbol) {
                helper();
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 207);
        assert_eq!(detector_result.offset_end, 283);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_6_assert_macro() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]

        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env) {
                assert!(false, "assert panic");
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        assert_eq!(codebase.contracts().count(), 1);
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 122);
        assert_eq!(detector_result.offset_end, 208);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_7_unreachable_macro() {
        let detector = ContractCanPanic;
        let src = r#"#![no_std]

        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env) {
                unreachable!("unreachable panic");
            }
        }
        "#;
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 122);
        assert_eq!(detector_result.offset_end, 211);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }

    #[test]
    fn contract_panics_8_external_helper() {
        let detector = ContractCanPanic;
        let helper_src = r#"#![no_std]

        pub fn helper() {
            panic!("external panic");
        }
        "#;
        let main_src = r"#![no_std]

        mod helper;
        use helper::helper;

        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env) {
                helper();
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/helper.rs".to_string(), helper_src.to_string());
        data.insert("test/lib.rs".to_string(), main_src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test/lib.rs");
        assert_eq!(detector_result.offset_start, 171);
        assert_eq!(detector_result.offset_end, 235);
        assert_eq!(detector_result.extra, {
            let mut map = HashMap::new();
            map.insert("CONTRACT_NAME".to_string(), "Contract".to_string());
            map.insert("FUNCTION_NAME".to_string(), "hello".to_string());
            Some(map)
        });
    }
}
