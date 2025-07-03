/// Detector for extending TTL with the existing `MAX_TTL` value.
///
/// Flags patterns where a contract extends a temporary storage TTL by passing
/// `MAX_TTL` as the new TTL, which may not have the intended effect.
use std::rc::Rc;

use soroban_security_detectors_sdk::{
    definition::Definition,
    expression::{Expression, FunctionCall, Identifier, MethodCall},
    node_type::NodeKind,
    statement::Statement,
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
    // let function = codebase.inline_function(function);
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

fn check_function_call(
    codebase: &SealedCodebase,
    function_call: &Rc<FunctionCall>,
    scope_id: u32,
) -> bool {
    if let Some(Definition::Function(func)) =
        codebase.get_function_by_name(scope_id, function_call.function_name.as_str())
    {
        if func.returns.borrow().name() != "u32" {
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
    fn test_extend_ttl_with_max_ttl_1() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Address, Env, Symbol, Vec};
        #[contract]
        pub struct TTLExamples;

        #[contractimpl]
        impl TTLExamples {
            
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
        assert_eq!(detector_result.offset_start, 485);
        assert_eq!(detector_result.offset_end, 626);
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
                env.storage().persistent().extend_ttl(&key, extension, env.storage().persistent().max_ttl());
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_function_result() {
        let detector = crate::ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct AliasStorage;

        fn get_max_ttl(env: &Env) -> u32 {
            env.storage().persistent().max_ttl()
        }

        #[contractimpl]
        impl AliasStorage {
            pub fn alias_storage(env: Env, key: Symbol, days: u32) {
                let max_ttl = get_max_ttl(&env);
                let extension = days.min(max_ttl);
                let st = env.storage().persistent();
                st.extend_ttl(&key, extension, max_ttl);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_3() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct AliasStorage;

        #[contractimpl]
        impl AliasStorage {
            pub fn alias_storage(env: Env, key: Symbol, days: u32) {
                let st = env.storage().persistent();
                let max_ttl = st.max_ttl();
                let extension = days.min(max_ttl);
                st.extend_ttl(&key, extension, max_ttl);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_4() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct NestedAlias;

        #[contractimpl]
        impl NestedAlias {
            pub fn nested_alias(env: Env, key: Symbol, days: u32) {
                let m1 = env.storage().persistent().max_ttl();
                let m2 = m1;
                let extension = days.min(m2);
                env.storage().persistent().extend_ttl(&key, extension, m2);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_5() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct BlockScope;

        #[contractimpl]
        impl BlockScope {
            pub fn block_scope(env: Env, key: Symbol) {
                let key_clone = key.clone();
                {
                    let mt = env.storage().persistent().max_ttl();
                    env.storage().persistent().extend_ttl(&key_clone, 1, mt);
                }
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_6() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct FqPath;

        #[contractimpl]
        impl FqPath {
            pub fn fq_path(env: Env, key: Symbol) {
                let mt = env.storage().persistent().max_ttl();
                env.storage().persistent().extend_ttl(&key, 2, mt);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_7() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct DifferentExt;

        #[contractimpl]
        impl DifferentExt {
            pub fn different_ext(env: Env, symbol: Symbol) {
                let maxval = env.storage().persistent().max_ttl();
                env.storage().persistent().extend_ttl(&symbol, maxval, maxval);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_8() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct InnerBlock;

        #[contractimpl]
        impl InnerBlock {
            pub fn some(env: Env, k: Symbol) {
                if true {
                    let max = env.storage().persistent().max_ttl();
                    env.storage().persistent().extend_ttl(&k, 0, max);
                }
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_9() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct AliasTwo;

        #[contractimpl]
        impl AliasTwo {
            pub fn alias_two(env: Env, key: Symbol) {
                let st = env.storage().persistent();
                let max_ttl_storage = st.max_ttl();
                st.extend_ttl(&key, 3, max_ttl_storage);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_with_max_ttl_10() {
        let detector = ExtendTtlWithMaxTtl;
        let src = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
        #[contract]
        pub struct DoubleAlias;

        #[contractimpl]
        impl DoubleAlias {
            pub fn double_alias(env: Env, key: Symbol) {
                let a = env.storage().persistent().max_ttl();
                let b = a;
                let c = b;
                env.storage().persistent().extend_ttl(&key, 4, c);
            }
        }
        ";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    /// ---- Multi-file linking tests ----
    #[test]
    fn test_extend_ttl_multi_file_1() {
        let detector = ExtendTtlWithMaxTtl;
        let src_lib = r"use soroban_sdk::{contract, contractimpl, Env, Symbol}; mod foo;";
        let src_foo = r"
use soroban_sdk::{contractimpl, Env, Symbol};

#[contract]
pub struct Foo;

#[contractimpl]
impl Foo {
    pub fn foo(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 10, max);
    }
}
";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src_lib.to_string());
        data.insert("test/foo.rs".to_string(), src_foo.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_multi_file_2() {
        let detector = ExtendTtlWithMaxTtl;
        let src_lib = r"use soroban_sdk::{contract, contractimpl, Env, Symbol}; mod foo; mod bar;";
        let src_foo = r"
use soroban_sdk::{contractimpl, Env, Symbol};

#[contract]
pub struct Foo;

#[contractimpl]
impl Foo {
    pub fn foo(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 20, max);
    }
}
";
        let src_bar = r"
use soroban_sdk::{contractimpl, Env, Symbol};

#[contract]
pub struct Bar;

#[contractimpl]
impl Bar {
    pub fn bar(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 21, max);
    }
}
";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src_lib.to_string());
        data.insert("test/foo.rs".to_string(), src_foo.to_string());
        data.insert("test/bar.rs".to_string(), src_bar.to_string());
        let codebase = build_codebase(&data).unwrap();
        let errors = detector.check(&codebase).unwrap();
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn test_extend_ttl_multi_file_3() {
        let detector = ExtendTtlWithMaxTtl;
        let src_lib = r"use soroban_sdk::{contract, contractimpl, Env, Symbol}; mod a;";
        let src_a = r"
use soroban_sdk::{contractimpl, Env, Symbol};

#[contract]
pub struct A;

#[contractimpl]
impl A {
    pub fn a(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 30, max);
    }
}
";
        let mut data = HashMap::new();
        data.insert("test/lib.rs".to_string(), src_lib.to_string());
        data.insert("test/a.rs".to_string(), src_a.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(&codebase);
        assert!(result.is_some());
    }

    #[test]
    fn test_extend_ttl_multi_file_4() {
        let detector = ExtendTtlWithMaxTtl;
        let src1 = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct First;

#[contractimpl]
impl First {
    pub fn f(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 41, max);
    }
}
";
        let src2 = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct Second;

#[contractimpl]
impl Second {
    pub fn s(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 42, max);
    }
}
";
        let mut data = HashMap::new();
        data.insert("first.rs".to_string(), src1.to_string());
        data.insert("second.rs".to_string(), src2.to_string());
        let codebase = build_codebase(&data).unwrap();
        let errors = detector.check(&codebase).unwrap();
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn test_extend_ttl_multi_file_5() {
        let detector = ExtendTtlWithMaxTtl;
        let src1 = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct One;

#[contractimpl]
impl One {
    pub fn one(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 43, max);
    }
}
";
        let src2 = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct Two;

#[contractimpl]
impl Two {
    pub fn two(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 44, max);
    }
}
";
        let src3 = r"use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct Three;

#[contractimpl]
impl Three {
    pub fn three(env: Env, key: Symbol) {
        let max = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 45, max);
    }
}
";
        let mut data = HashMap::new();
        data.insert("one.rs".to_string(), src1.to_string());
        data.insert("two.rs".to_string(), src2.to_string());
        data.insert("three.rs".to_string(), src3.to_string());
        let codebase = build_codebase(&data).unwrap();
        let errors = detector.check(&codebase).unwrap();
        assert_eq!(errors.len(), 3);
    }
}
