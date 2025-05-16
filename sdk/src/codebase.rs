use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::contract::Contract;
use crate::definition::Definition;
use crate::directive::Directive;
use crate::expression::Expression;
use crate::file::File;
use crate::node_type::{ContractType, FileChildType, TypeNode};
use crate::statement::Statement;
use crate::{ast::node_type::NodeKind, contract::Struct, custom_type::Type};
use crate::{symbol_table, NodesStorage, SymbolTable};
use serde::{Deserialize, Serialize};

#[allow(dead_code)]
trait CodebaseOpen {}
#[allow(dead_code)]
trait CodebaseSealed {}

pub struct OpenState;
impl CodebaseOpen for OpenState {}

pub struct SealedState;
impl CodebaseSealed for SealedState {}

#[derive(Serialize, Deserialize)]
pub struct Codebase<S> {
    pub(crate) storage: NodesStorage,
    pub(crate) files: Vec<Rc<File>>,
    #[serde(skip)]
    pub(crate) syn_files: HashMap<String, syn::File>,
    #[serde(skip)]
    pub(crate) contract_cache: RefCell<HashMap<u32, Rc<Contract>>>,
    #[serde(skip)]
    pub(crate) symbol_table: Option<SymbolTable>,
    _state: PhantomData<S>,
}

impl Default for Codebase<OpenState> {
    fn default() -> Self {
        Self {
            storage: NodesStorage::default(),
            files: Vec::new(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table: None,
            _state: PhantomData,
        }
    }
}

impl Codebase<SealedState> {
    #[must_use]
    pub fn new(storage: NodesStorage, symbol_table: Option<SymbolTable>) -> Self {
        Self {
            storage,
            files: Vec::new(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table,
            _state: PhantomData,
        }
    }

    pub fn files(&self) -> impl Iterator<Item = Rc<File>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::File(file) = item {
                res.push(file.clone());
            }
        }
        res.into_iter()
    }

    pub fn contracts(&self) -> impl Iterator<Item = Rc<Contract>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Statement(Statement::Definition(Definition::Struct(struct_node))) =
                item
            {
                if struct_node.is_contract {
                    res.push(self.construct_contract_from_struct(struct_node));
                }
            }
        }
        res.into_iter()
    }

    #[must_use = "Use this method to get `Node` source code"]
    pub fn get_node_source_code(&self, node_id: u32) -> Option<String> {
        self.storage.get_node_source_code(node_id)
    }

    fn construct_contract_from_struct(&self, struct_node: &Struct) -> Rc<Contract> {
        if let Some(cached) = self.contract_cache.borrow().get(&struct_node.id) {
            return cached.clone();
        }
        let mut methods = Vec::new();
        let mut functions = Vec::new();
        let mut type_aliases = Vec::new();
        let mut constants = Vec::new();
        let mut macros = Vec::new();
        let mut plane_defs = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Statement(Statement::Definition(Definition::Implementation(
                impl_node,
            ))) = item
            {
                if let Some(for_type) = &impl_node.for_type {
                    let name = match for_type {
                        Type::Typedef(t) => t.clone(),
                        Type::Alias(type_alias) => type_alias.name.clone(),
                        Type::Struct(tstruct) => tstruct.name.clone(),
                    };
                    if name != struct_node.name {
                        continue;
                    }
                } else {
                    continue;
                }
                impl_node.constants.iter().for_each(|constant| {
                    constants.push(constant.clone());
                });
                impl_node.type_aliases.iter().for_each(|type_alias| {
                    type_aliases.push(type_alias.clone());
                });
                impl_node.macros.iter().for_each(|macro_| {
                    macros.push(macro_.clone());
                });
                impl_node.plane_defs.iter().for_each(|plane_def| {
                    plane_defs.push(plane_def.clone());
                });
                for func in &impl_node.functions {
                    if func.is_method() {
                        methods.push(func.clone());
                    } else {
                        functions.push(func.clone());
                    }
                }
            }
        }
        let contract = Rc::new(Contract {
            id: struct_node.id,
            name: struct_node.name.clone(),
            location: struct_node.location.clone(),
            fields: struct_node.fields.clone(),
            methods: RefCell::new(methods),
            functions: RefCell::new(functions),
            type_aliases: RefCell::new(type_aliases),
            constants: RefCell::new(constants),
            macros: RefCell::new(macros),
            plane_defs: RefCell::new(plane_defs),
        });
        self.contract_cache
            .borrow_mut()
            .insert(struct_node.id, contract.clone());
        contract
    }

    pub fn get_expression_type(&self, node_id: u32) -> Option<TypeNode> {
        if let Some(node) = self.storage.find_node(node_id) {
            if let Some(symbol_table) = &self.symbol_table {
                match node {
                    NodeKind::Statement(Statement::Expression(expr)) => {
                        symbol_table.infer_expr_type(&expr)
                    }
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_symbol_type(&self, symbol: &str) -> Option<TypeNode> {
        if let Some(symbol_table) = &self.symbol_table {
            symbol_table.lookdown_symbol(symbol)
        } else {
            None
        }
    }
}

impl<T> Codebase<T> {
    #[must_use = "Use this function to get a Node's source file"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
        self.storage.find_node_file(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build_codebase;

    #[test]
    fn test_find_node_file() {
        let src = "#![no_std]
use soroban_sdk::contract;

#[contract]
struct Contract1;";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let file = codebase.find_node_file(contract.id).unwrap();
        assert_eq!(file.path, "test.rs");
    }

    #[test]
    fn test_contract_methods() {
        let src = "#![no_std]
use soroban_sdk::contract;

#[contract]
struct Contract1;

impl Contract1 {
    fn get_field(&self) -> Self {
        self.field
    }
}";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.methods.borrow().len(), 1);
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_expression_types_1() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
     fn get_field(&self) -> u32 {
            self.field
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "u32");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::MemberAccess(stmt)) = stmt else {
            panic!("Expected MemberAccess statement");
        };
        let Expression::Identifier(base) = &stmt.base else {
            panic!("Expected Identifier expression");
        };
        let t = codebase.get_symbol_type(&base.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let t = codebase.get_symbol_type(&stmt.member_name).unwrap();
        assert_eq!(t.name(), "u32");
        // print!("!! {t:?}");
    }

    #[test]
    fn test_expression_types_2() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_value(&self, a: i32) -> u32 {
            42
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "i32");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "u32");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::Literal(lit_expr)) = stmt else {
            panic!("Expected Literal expression");
        };
        let t = codebase.get_expression_type(lit_expr.id).unwrap();
        assert_eq!(t.name(), "i32");
    }

    #[test]
    fn test_expression_types_3() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_string(&self, s: &str) -> String {
            String::from(\"Hello\")
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "str"); //TODO: add is_ref field?
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "String");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::FunctionCall(func_call)) = stmt else {
            panic!("Expected FunctionCall expression, found {stmt:?}");
        };
        let Expression::Literal(param) = func_call.parameters[0].clone() else {
            panic!("Expected Literal expression");
        };
        let t = codebase.get_expression_type(param.id).unwrap();
        assert_eq!(t.name(), "&str"); //TODO: check me
    }

    #[test]
    fn test_expression_types_4() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_array(&self, v: [i32; 9]) -> Vec<u32> {
            vec![1, 2, 3]
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "[i32; 9]");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "Vec<u32>");
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Macro(marco_expr)) = stmt else {
            panic!("Expected Macro expression, found {stmt:?}");
        };
        let t = codebase.get_expression_type(marco_expr.id).unwrap();
        assert_eq!(t.name(), "Vec<i32>");
    }

    #[test]
    fn test_expression_types_5() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_tuple(&self, t: (u32, String)) -> (u32, String) {
            (42, String::from(\"Hello\"))
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(&param.name).unwrap();
        assert_eq!(t.name(), "(u32, String)");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "(u32, String)");
        let stmt = methods[0].body.as_ref().unwrap().statements.last().unwrap();
        let Statement::Expression(Expression::Tuple(tuple_expr)) = stmt else {
            panic!("Expected Tuple expression");
        };
        let t = codebase.get_expression_type(tuple_expr.id).unwrap();
        assert_eq!(t.name(), "(u32, String)");
    }

    #[test]
    fn test_expression_types_6() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_map(&self) -> HashMap<String, u32> {
            let mut map = HashMap::new();
            map.insert(String::from(\"key\"), 42);
            map
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0].body.as_ref().unwrap().statements.last().unwrap();
        let Statement::Expression(Expression::Identifier(ident_expr)) = stmt else {
            panic!("Expected Identifier expression");
        };
        let t = codebase.get_symbol_type(&ident_expr.name).unwrap();
        assert_eq!(t.name(), "HashMap<String, u32>");
    }

    #[test]
    fn test_expression_types_7() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_option(&self) -> Option<u32> {
            Some(42)
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::FunctionCall(call_expr)) = stmt else {
            panic!("Expected FunctionCall expression");
        };
        let t = codebase.get_expression_type(call_expr.id).unwrap();
        assert_eq!(t.name(), "Option<u32>");
    }

    #[test]
    fn test_expression_types_8() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_result(&self) -> Result<u32, String> {
            Ok(42)
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::FunctionCall(call_expr)) = stmt else {
            panic!("Expected FunctionCall expression");
        };
        let t = codebase.get_expression_type(call_expr.id).unwrap();
        assert_eq!(t.name(), "Result<u32, String>");
    }

    #[test]
    fn test_expression_types_9() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_reference(&self) -> &Self {
            self
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Identifier(ident_expr)) = stmt else {
            panic!("Expected Identifier expression");
        };
        let t = codebase.get_expression_type(ident_expr.id).unwrap();
        assert_eq!(t.name(), "Contract1");
    }

    #[test]
    fn test_expression_types_10() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_pointer(&self) -> *const Self {
            self as *const Self
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Cast(cast_expr)) = stmt else {
            panic!("Expected Cast expression");
        };
        let t = codebase.get_expression_type(cast_expr.id).unwrap();
        assert_eq!(t.name(), "*const Contract1");
    }

    #[test]
    fn test_expression_types_11() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_closure(&self) -> impl Fn(u32) -> u32 {
            |x| x + 1
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Closure(closure_expr)) = stmt else {
            panic!("Expected Closure expression");
        };
        let t = codebase.get_expression_type(closure_expr.id).unwrap();
        assert!(t.name().contains("impl Fn"));
    }

    #[test]
    fn test_expression_types_12() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_trait(&self) -> &dyn std::fmt::Debug {
            self
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[8]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Identifier(ident_expr)) = stmt else {
            panic!("Expected Identifier expression");
        };
        let t = codebase.get_expression_type(ident_expr.id).unwrap();
        assert_eq!(t.name(), "Contract1");
    }

    #[test]
    fn test_expression_types_13() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_macro(&self) -> String {
            format!(\"Hello, {}!\", \"world\")
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Macro(macro_expr)) = stmt else {
            panic!("Expected MacroCall expression");
        };
        let t = codebase.get_expression_type(macro_expr.id).unwrap();
        assert_eq!(t.name(), "String");
    }

    #[test]
    fn test_expression_types_14() {
        let src = "#![no_std]
    #[contract]
    struct Contract1 {
        field: u32,
    }        
    impl Contract1 {
        fn get_function(&self) -> fn(u32) -> u32 {
            |x| x + 1
        }
    }";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let stmt = methods[0]
            .body
            .as_ref()
            .unwrap()
            .statements
            .first()
            .unwrap();
        let Statement::Expression(Expression::Closure(closure_expr)) = stmt else {
            panic!("Expected Closure expression");
        };
        let t = codebase.get_expression_type(closure_expr.id).unwrap();
        assert!(t.name().contains("fn(u32) -> u32"));
    }
}
