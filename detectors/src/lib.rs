#![warn(clippy::pedantic)]
include!(concat!(env!("OUT_DIR"), "/mod_includes.rs"));
include!(concat!(env!("OUT_DIR"), "/detector_report_templates.rs"));
include!(concat!(env!("OUT_DIR"), "/register.rs"));

// mod contract_without_functions;
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
        assert!(function.will_panic());
    }

    // #[test]
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
        assert!(function.will_panic());
    }
}
