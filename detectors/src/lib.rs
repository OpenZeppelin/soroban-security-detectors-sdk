#![warn(clippy::pedantic)]
include!(concat!(env!("OUT_DIR"), "/mod_includes.rs"));
include!(concat!(env!("OUT_DIR"), "/detector_report_templates.rs"));
include!(concat!(env!("OUT_DIR"), "/register.rs"));

#[cfg(test)]
mod test {
    use soroban_security_detectors_sdk::build_codebase;
    use std::collections::HashMap;

    #[test]
    fn contract_panics_1() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_2() {
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
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_3() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 2);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        let function = codebase.inline_function(function.clone());
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_4_unwrap_only() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_5_indirect_panic() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 2);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        let function = codebase.inline_function(function.clone());
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_6_assert_macro() {
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
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_7_unreachable_macro() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        assert!(function.can_panic());
    }

    #[test]
    fn contract_panics_8_external_helper() {
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
        assert_eq!(codebase.contracts().count(), 1);
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.name, "Contract");
        assert_eq!(contract.functions_count(), 1);
        let function = contract.functions().next().unwrap();
        assert_eq!(function.name, "hello");
        let function = codebase.inline_function(function.clone());
        assert!(function.can_panic());
    }
}
