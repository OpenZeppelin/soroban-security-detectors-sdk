use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::contract::Contract;
use crate::definition::Definition;
use crate::expression::Expression;
use crate::file::File;
use crate::function::Function;
use crate::node_type::NodeType;
use crate::prelude::ExternPrelude;
use crate::statement::{Block, Statement};
use crate::{ast::node_type::NodeKind, contract::Struct, custom_type::Type};
use crate::{NodesStorage, SymbolTable};
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
    #[serde(skip)]
    pub(crate) syn_files: HashMap<String, syn::File>,
    #[serde(skip)]
    pub(crate) contract_cache: RefCell<HashMap<u32, Rc<Contract>>>,
    #[serde(skip)]
    pub(crate) symbol_table: SymbolTable,
    #[serde(skip)]
    pub(crate) extern_prelude: ExternPrelude,
    pub(crate) _state: PhantomData<S>,
}

impl Default for Codebase<OpenState> {
    fn default() -> Self {
        Self {
            storage: NodesStorage::default(),
            syn_files: HashMap::new(),
            contract_cache: RefCell::new(HashMap::new()),
            symbol_table: SymbolTable::new(),
            extern_prelude: ExternPrelude::new(),
            _state: PhantomData,
        }
    }
}

impl Codebase<SealedState> {
    pub fn files(&self) -> impl Iterator<Item = Rc<File>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::File(file) = item {
                if file.is_synthetic_root() {
                    continue;
                }
                res.push(file.clone());
            }
        }
        res.into_iter()
    }

    pub fn contracts(&self) -> impl Iterator<Item = Rc<Contract>> {
        let mut res = Vec::new();
        for item in &self.storage.nodes {
            if let NodeKind::Definition(Definition::Struct(struct_node)) = item {
                if struct_node.is_contract && !self.is_soroban_sdk_node(struct_node.id) {
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

    pub fn functions(&self) -> impl Iterator<Item = Rc<Function>> + '_ {
        self.list_nodes_cmp(|node| {
            if let NodeKind::Definition(Definition::Function(func)) = node {
                Some(func.clone())
            } else {
                None
            }
        })
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
            if let NodeKind::Definition(Definition::Implementation(impl_node)) = item {
                let name = match &impl_node.for_type {
                    Type::Typename(t) => t.name.clone(),
                    Type::Alias(type_alias) => type_alias.name.clone(),
                    Type::Struct(tstruct) => tstruct.name.clone(),
                };
                if name != struct_node.name {
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

    #[must_use]
    pub fn get_parent_container(&self, id: u32) -> Option<NodeKind> {
        let mut current_id = id;
        while let Some(route) = self.storage.find_parent_node(current_id) {
            current_id = route.id;
            if let Some(node) = self.storage.find_node(current_id) {
                // println!("Checking node: {node:?}\n");
                match &node {
                    NodeKind::Definition(_) | NodeKind::File(_) => {
                        return Some(node);
                    }
                    NodeKind::Statement(stmt) => {
                        if let Statement::Definition(_) /*| Statement::Block(_)*/ = stmt {
                            return Some(node);
                        }
                    }
                    NodeKind::Directive(_)
                    | NodeKind::Expression(_)
                    | NodeKind::Pattern(_)
                    | NodeKind::Literal(_)
                    | NodeKind::Type(_)
                    | NodeKind::Misc(_) => {}
                }
            }
        }
        None
    }

    #[must_use = "Use this function to get a Node's source file"]
    pub fn find_node_file(&self, id: u32) -> Option<Rc<File>> {
        self.storage.find_node_file(id)
    }

    fn is_soroban_sdk_node(&self, id: u32) -> bool {
        if let Some(file) = self.find_node_file(id) {
            file.is_soroban_sdk_file()
        } else {
            false
        }
    }

    pub fn get_children_cmp<F>(&self, id: u32, comparator: F) -> Vec<NodeKind>
    where
        F: Fn(&NodeKind) -> bool,
    {
        let mut result = Vec::new();
        let mut stack: Vec<NodeKind> = Vec::new();

        if let Some(root_node) = self.storage.find_node(id) {
            stack.push(root_node.clone());
        }

        while let Some(current_node) = stack.pop() {
            if comparator(&current_node) {
                result.push(current_node.clone());
            }
            stack.extend(current_node.children());
        }

        result
    }

    pub fn get_children_cmp_cast<F, C>(&self, id: u32, comparator: F) -> Vec<C>
    where
        F: Fn(&NodeKind) -> bool,
        C: From<NodeKind>,
    {
        self.get_children_cmp(id, comparator)
            .into_iter()
            .map(C::from)
            .collect::<Vec<C>>()
    }

    fn list_nodes_cmp<'a, T, F>(&'a self, cast: F) -> impl Iterator<Item = T> + 'a
    where
        F: Fn(&NodeKind) -> Option<T> + 'a,
        T: Clone + 'static,
    {
        self.storage.nodes.iter().filter_map(cast)
    }

    #[must_use]
    pub fn node_type(&self, node: &NodeKind) -> NodeType {
        match node {
            NodeKind::Expression(expr) | NodeKind::Statement(Statement::Expression(expr)) => {
                self.expr_type(expr)
            }
            NodeKind::Definition(def) => match def {
                Definition::Function(f) => {
                    return f.returns.borrow().clone();
                }
                Definition::Static(s) => Self::type_node_from_custom_type(&s.ty),
                Definition::Const(c) => Self::type_node_from_custom_type(&c.type_),
                Definition::AssocType(t) => Self::type_node_from_custom_type(&t.ty),
                _ => NodeType::Empty,
            },
            NodeKind::Type(ty) => Self::type_node_from_custom_type(ty),
            _ => NodeType::Empty,
        }
    }

    fn expr_type(&self, expr: &Expression) -> NodeType {
        match expr {
            Expression::FunctionCall(call) => self
                .functions()
                .find(|f| f.name == call.function_name)
                .map(|f| f.returns.borrow().clone())
                .unwrap_or_default(),
            Expression::MethodCall(call) => {
                let base_ty = self.node_type(&NodeKind::Statement(Statement::Expression(
                    call.base.clone(),
                )));
                if let NodeType::Path(p) = base_ty {
                    if let Some(contract) = self.contracts().find(|c| c.name == p) {
                        if let Some(m) = contract
                            .methods
                            .borrow()
                            .iter()
                            .find(|m| m.name == call.method_name)
                        {
                            return m.returns.borrow().clone();
                        }
                    }
                }
                NodeType::Empty
            }
            Expression::Cast(c) => Self::type_node_from_custom_type(&c.target_type),
            Expression::Closure(c) => Self::type_node_from_custom_type(&c.returns),
            Expression::Return(r) => {
                if let Some(inner) = &r.expression {
                    self.node_type(&NodeKind::Statement(Statement::Expression(inner.clone())))
                } else {
                    NodeType::Empty
                }
            }
            _ => NodeType::Empty,
        }
    }

    fn type_node_from_custom_type(ty: &Type) -> NodeType {
        match ty {
            Type::Typename(t) => NodeType::Path(t.name.clone()),
            Type::Alias(a) => Self::type_node_from_custom_type(&a.ty),
            Type::Struct(s) => {
                let inner = &*s.ty;
                NodeType::Path(inner.name.clone())
            }
        }
    }

    #[must_use]
    pub fn compare_types(&self, a: &NodeKind, b: &NodeKind) -> bool {
        self.node_type(a) == self.node_type(b)
    }

    #[must_use]
    pub fn inline_function(&self, func: &Rc<Function>) -> Function {
        fn inline_statements(
            codebase: &Codebase<SealedState>,
            scope_id: u32,
            stmts: &[Statement],
        ) -> Vec<Statement> {
            let mut result = Vec::new();
            for stmt in stmts {
                match stmt {
                    Statement::Expression(expr) => match &expr {
                        Expression::FunctionCall(fc) => {
                            if let Some(Definition::Function(f)) =
                                codebase.get_function_by_name(scope_id, &fc.function_name)
                            {
                                if let Some(body) = &f.body {
                                    let inlined =
                                        inline_statements(codebase, scope_id, &body.statements);
                                    result.extend(inlined);
                                    continue;
                                }
                            }
                            result.push(stmt.clone());
                        }
                        Expression::MethodCall(mc) => {
                            if let Some(Definition::Function(f)) =
                                codebase.get_function_by_name(scope_id, &mc.method_name)
                            {
                                if let Some(body) = &f.body {
                                    let inlined =
                                        inline_statements(codebase, scope_id, &body.statements);
                                    result.extend(inlined);
                                    continue;
                                }
                            }
                            result.push(stmt.clone());
                        }
                        _ => {}
                    },
                    Statement::Block(block) => {
                        let inlined_block = Block {
                            id: block.id,
                            location: block.location.clone(),
                            statements: inline_statements(codebase, scope_id, &block.statements),
                        };
                        result.push(Statement::Block(Rc::new(inlined_block)));
                    }
                    _ => result.push(stmt.clone()),
                }
            }
            result
        }

        let mut new_func = (**func).clone();
        if let Some(body) = &func.body {
            let stmts = inline_statements(self, func.id, &body.statements);
            new_func.body = Some(Rc::new(Block {
                id: body.id,
                location: body.location.clone(),
                statements: stmts,
            }));
        }
        new_func
    }

    pub fn get_expression_type(&self, node_id: u32) -> NodeType {
        if let Some(node) = self.storage.find_node(node_id) {
            if let Some(parent_container) = self.get_parent_container(node.id()) {
                // println!("parent container {parent_container:?}");
                // println!("node {node:?}");
                match node {
                    NodeKind::Expression(expr)
                    | NodeKind::Statement(Statement::Expression(expr)) => self
                        .symbol_table
                        .infer_expr_type(parent_container.id(), &expr),
                    _ => NodeType::Empty,
                }
            } else {
                NodeType::Empty
            }
        } else {
            NodeType::Empty
        }
    }

    pub fn get_symbol_type(&self, scope_id: u32, symbol: &str) -> Option<NodeType> {
        self.symbol_table.lookup_symbol_in_scope(scope_id, symbol)
    }

    pub fn get_function_by_name(&self, scope_id: u32, name: &str) -> Option<Definition> {
        self.symbol_table.get_function_by_name(scope_id, name)
    }

    pub fn lookup_symbol_origin(&self, scope_id: u32, symbol: &str) -> Option<NodeKind> {
        self.symbol_table.lookup_symbol_origin(scope_id, symbol)
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let file = codebase.find_node_file(contract.id).unwrap();
        assert_eq!(file.path, "test/lib.rs");
    }

    #[test]
    fn test_find_node_file_for_function() {
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let function = contract.methods.borrow().iter().next().unwrap().clone();
        let file = codebase.find_node_file(function.id).unwrap();
        assert_eq!(file.path, "test/lib.rs");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        assert_eq!(contract.methods.borrow().len(), 1);
    }

    #[test]
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        if let NodeType::Reference {
            mutable,
            is_explicit_reference,
            inner: _,
        } = t
        {
            assert!(!mutable);
            assert!(is_explicit_reference);
        } else {
            panic!("Expected Reference type");
        }
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "u32");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::MemberAccess(stmt)) = stmt else {
            panic!("Expected MemberAccess statement");
        };
        let Expression::Identifier(base) = &stmt.base else {
            panic!("Expected Identifier expression");
        };
        let t = codebase.get_symbol_type(method.id, &base.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        assert_eq!(codebase.get_expression_type(stmt.id).name(), "u32");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "i32");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "u32");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::Literal(lit_expr)) = stmt else {
            panic!("Expected Literal expression");
        };
        let t = codebase.get_expression_type(lit_expr.id);
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&str");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "String");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::FunctionCall(func_call)) = stmt else {
            panic!("Expected FunctionCall expression, found {stmt:?}");
        };
        let Expression::Literal(param) = func_call.parameters[0].clone() else {
            panic!("Expected Literal expression");
        };
        let t = codebase.get_expression_type(param.id);
        match t {
            NodeType::Reference {
                inner,
                mutable,
                is_explicit_reference,
            } => {
                assert!(!mutable);
                assert!(is_explicit_reference);
                match *inner {
                    NodeType::Path(path) => {
                        assert_eq!(path, "str");
                    }
                    _ => panic!("Expected Path type, found {inner:?}"),
                }
            }
            _ => panic!("Expected Reference type, found {t:?}"),
        }
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
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
        let t = codebase.get_expression_type(marco_expr.id);
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let param = method.parameters[1].clone();
        assert!(!param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "(u32, String)");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "(u32, String)");
        let stmt = methods[0].body.as_ref().unwrap().statements.last().unwrap();
        let Statement::Expression(Expression::Tuple(tuple_expr)) = stmt else {
            panic!("Expected Tuple expression");
        };
        let t = codebase.get_expression_type(tuple_expr.id);
        assert_eq!(t.name(), "(i32, _)");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "HashMap<String, u32>");
        let stmt = method.body.as_ref().unwrap().statements.last().unwrap();
        let Statement::Expression(Expression::Identifier(ident_expr)) = stmt else {
            panic!("Expected Identifier expression");
        };
        let t = codebase
            .get_symbol_type(method.id, &ident_expr.name)
            .unwrap();
        eprintln!("DEBUG: map variable type: {:?}, name text: {}", t, t.name());
        assert_eq!(t.name(), "HashMap");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "Option<u32>");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::FunctionCall(call_expr)) = stmt else {
            panic!("Expected FunctionCall expression");
        };
        let t = codebase.get_expression_type(call_expr.id);
        assert_eq!(t.name(), "Option<i32>");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "Result<u32, String>");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::FunctionCall(call_expr)) = stmt else {
            panic!("Expected FunctionCall expression");
        };
        let t = codebase.get_expression_type(call_expr.id);
        assert_eq!(t.name(), "Result<i32, _>");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(
            ret.borrow().name(),
            "&soroban_security_detectors_sdk::Contract1"
        );
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
        let t = codebase.get_expression_type(ident_expr.id);
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
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
        let t = codebase.get_expression_type(cast_expr.id);
        assert_eq!(t.name(), "*const soroban_security_detectors_sdk::Contract1");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "impl Fn (u32) -> u32");
        let stmt = method.body.as_ref().unwrap().statements.first().unwrap();
        let Statement::Expression(Expression::Closure(closure_expr)) = stmt else {
            panic!("Expected Closure expression");
        };
        let t = codebase.get_expression_type(closure_expr.id);
        assert_eq!(t.name(), "_ || -> _");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
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
        let t = codebase.get_expression_type(ident_expr.id);
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
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
        data.insert("test/lib.rs".to_string(), src.to_string());
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
        let t = codebase.get_expression_type(macro_expr.id);
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
        data.insert("test/lib.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let methods = contract.methods.borrow();
        let method = methods[0].clone();
        let param = method.parameters[0].clone();
        assert!(param.is_self);
        assert!(!param.is_mut);
        let t = codebase.get_symbol_type(method.id, &param.name).unwrap();
        assert_eq!(t.name(), "&soroban_security_detectors_sdk::Contract1");
        let ret = method.returns.clone();
        assert_eq!(ret.borrow().name(), "fn(u32) -> u32");
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
        let t = codebase.get_expression_type(closure_expr.id);
        assert_eq!(t.name(), "_ || -> _");
    }

    #[test]
    fn inline_function_test_1() {
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
        let contract = codebase.contracts().next().unwrap();
        let functions = contract.functions.borrow();
        let f_hello = functions.iter().find(|m| m.name == "hello").unwrap();
        let _ = codebase.inline_function(f_hello);
        // println!("Inlined function: {inlined:?}");
    }

    #[test]
    fn inline_function_test_2() {
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
        data.insert("test/lib.rs".to_string(), main_src.to_string());
        data.insert("test/helper.rs".to_string(), helper_src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let contract = codebase.contracts().next().unwrap();
        let functions = contract.functions.borrow();
        let f_hello = functions.iter().find(|m| m.name == "hello").unwrap();
        let inlined = codebase.inline_function(f_hello);
        println!("Inlined function: {inlined:?}");
    }
}
