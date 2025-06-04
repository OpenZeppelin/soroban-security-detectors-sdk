use soroban_security_detectors_sdk::{DetectorResult, SealedCodebase};

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
                let func = codebase.inline_function(function.clone());
                if func.can_panic() {
                    errors.push(DetectorResult {
                        file_path: codebase.find_node_file(contract.id).unwrap().path.clone(),
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 2);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // let function = codebase.inline_function(function.clone());
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 2);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // let function = codebase.inline_function(function.clone());
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // assert!(function.can_panic());
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
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // assert!(function.can_panic());
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

        #[contract]
        pub struct Contract;

        #[contractimpl]
        impl Contract {
            pub fn hello(env: Env) {
                helper::helper();
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("helper.rs".to_string(), helper_src.to_string());
        data.insert("test.rs".to_string(), main_src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");

        // assert_eq!(codebase.contracts().count(), 1);
        // let contract = codebase.contracts().next().unwrap();
        // assert_eq!(contract.name, "Contract");
        // assert_eq!(contract.functions_count(), 1);
        // let function = contract.functions().next().unwrap();
        // assert_eq!(function.name, "hello");
        // let function = codebase.inline_function(function.clone());
        // assert!(function.can_panic());
    }
}
