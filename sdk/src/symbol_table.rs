use std::{cell::RefCell, collections::HashMap, rc::Rc};
use syn::parse_str;

use crate::custom_type::Type as CustomType;
use crate::definition::{Definition, Module};
use crate::expression::{
    Binary, Expression, FunctionCall, Identifier, Lit, MemberAccess, MethodCall, Unary,
};
use crate::function::Function;
use crate::literal::Literal;
use crate::node_type::FileChildType;
use crate::node_type::{NodeKind, TypeNode};
use crate::statement::Statement;
use crate::{Codebase, OpenState, SealedState};
type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeRef>,
    definitions: HashMap<String, Vec<Definition>>,
    variables: HashMap<String, TypeNode>,
}

impl Scope {
    fn new(parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            parent,
            definitions: HashMap::new(),
            variables: HashMap::new(),
        }))
    }

    fn insert_def(&mut self, name: String, def: Definition) {
        self.definitions.entry(name).or_default().push(def);
    }

    fn lookup_def(&self, name: &str) -> Option<Vec<Definition>> {
        if let Some(defs) = self.definitions.get(name) {
            return Some(defs.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup_def(name);
        }
        None
    }

    fn insert_var(&mut self, name: String, ty: TypeNode) {
        self.variables.insert(name, ty);
    }

    fn lookup_var(&self, name: &str) -> Option<TypeNode> {
        if let Some(ty) = self.variables.get(name) {
            return Some(ty.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup_var(name);
        }
        None
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    root: ScopeRef,
    /// Map file/module name to its lexical scope
    mod_scopes: HashMap<String, ScopeRef>,
    /// Map from type name to its methods (from `impl Type { ... }` blocks).
    methods: HashMap<String, Vec<Rc<Function>>>,
}

impl SymbolTable {
    #[must_use]
    pub fn from_codebase(codebase: &Codebase<OpenState>) -> Self {
        let root = Scope::new(None);
        let mut table = SymbolTable {
            root: root.clone(),
            mod_scopes: HashMap::new(),
            methods: HashMap::new(),
        };

        for node in &codebase.storage.nodes {
            if let NodeKind::Statement(Statement::Definition(Definition::Implementation(
                impl_node,
            ))) = node
            {
                if let Some(CustomType::Typedef(type_name)) = &impl_node.for_type {
                    let entry = table.methods.entry(type_name.clone()).or_default();
                    for f in &impl_node.functions {
                        entry.push(f.clone());
                    }
                }
            }
        }

        for file in &codebase.files {
            let mod_name = file.name.trim_end_matches(".rs").to_string();
            let module_scope = Scope::new(Some(root.clone()));
            table
                .mod_scopes
                .insert(mod_name.clone(), module_scope.clone());
            for child in file.children.borrow().iter() {
                let FileChildType::Definition(def) = child;
                process_definition(def.clone(), &root, &mut table);
                if let Definition::Module(m) = def {
                    process_module(m, &module_scope, &mod_name, &mut table);
                } else {
                    process_definition(def.clone(), &module_scope, &mut table);
                }
            }
        }

        table
    }

    #[must_use]
    pub fn lookup_def(&self, name: &str) -> Option<Vec<Definition>> {
        self.root.borrow().lookup_def(name)
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<Definition>> {
        self.lookup_def(name)
    }

    #[must_use]
    pub fn infer_expr_type(&self, expr: &Expression) -> Option<TypeNode> {
        infer_expr_type(expr, &self.root, self)
    }
    /// Resolve a potentially qualified path (`mod::Type`) to a Definition
    #[must_use]
    pub fn resolve_path(&self, path: &str) -> Option<Definition> {
        let parts: Vec<&str> = path.split("::").collect();
        if parts.is_empty() {
            return None;
        }
        // Starting scope
        let mut scope = self.root.clone();
        // Traverse module(s) for all but last segment
        for seg in &parts[..parts.len().saturating_sub(1)] {
            // Lookup module definition
            let defs = scope.borrow().lookup_def(seg)?;
            // Expect a module
            let mut module_found = None;
            for def in defs {
                if let Definition::Module(m) = &def {
                    module_found = Some(m.clone());
                    break;
                }
            }
            let module = module_found?;
            // Find scope for this module
            let full = module.name.clone();
            // Use mod_scopes map
            let next_scope = self.mod_scopes.get(&full)?;
            scope = next_scope.clone();
        }
        // Last segment is the name
        let name = parts[parts.len() - 1];
        let defs = scope.borrow().lookup_def(name)?;
        defs.into_iter().next()
    }
}

/// Helper to extract a definition's name, if any.
fn get_definition_name(def: &Definition) -> Option<String> {
    match def {
        Definition::Const(c) => Some(c.name.clone()),
        Definition::Static(s) => Some(s.name.clone()),
        Definition::Enum(e) => Some(e.name.clone()),
        Definition::Struct(s) => Some(s.name.clone()),
        Definition::Contract(c) => Some(c.name.clone()),
        Definition::Function(f) => Some(f.name.clone()),
        Definition::Type(t) => Some(t.name.clone()),
        Definition::Trait(tr) => Some(tr.name.clone()),
        Definition::TraitAlias(ta) => Some(ta.name.clone()),
        Definition::Union(u) => Some(u.name.clone()),
        Definition::ExternCrate(ec) => Some(ec.name.clone()),
        Definition::Macro(m) => Some(m.name.clone()),
        Definition::Module(m) => Some(m.name.clone()),
        _ => None,
    }
}

/// Recursively process a module definition, creating nested scopes and recording in `mod_scopes`.
fn process_module(
    module: &Rc<Module>,
    parent_scope: &ScopeRef,
    parent_path: &String,
    table: &mut SymbolTable,
) {
    // Create a new scope for this module
    let module_scope = Scope::new(Some(parent_scope.clone()));
    // Full path of this module
    let path = if parent_path.is_empty() {
        module.name.clone()
    } else {
        format!("{}::{}", parent_path, module.name)
    };
    table.mod_scopes.insert(path.clone(), module_scope.clone());
    // Insert the module definition itself into its scope
    module_scope
        .borrow_mut()
        .insert_def(module.name.clone(), Definition::Module(module.clone()));
    // Process inner definitions
    if let Some(defs) = &module.definitions {
        for def in defs {
            match def {
                Definition::Module(inner_mod) => {
                    process_module(inner_mod, &module_scope, &path, table);
                }
                _ => process_definition(def.clone(), &module_scope, table),
            }
        }
    }
}

fn process_definition(def: Definition, scope: &ScopeRef, table: &mut SymbolTable) {
    // Insert named definition
    if let Some(name) = get_definition_name(&def) {
        scope.borrow_mut().insert_def(name.clone(), def.clone());
    }
    // Handle nested scopes for modules and functions
    match def {
        Definition::Module(m) => {
            let module_scope = Scope::new(Some(scope.clone()));
            if let Some(defs) = &m.definitions {
                for sub in defs {
                    process_definition(sub.clone(), &module_scope.clone(), table);
                }
            }
        }
        Definition::Function(f) => {
            let fun_scope = Scope::new(Some(scope.clone()));
            // Bind function parameters
            for p in &f.parameters {
                let ty_node = match parse_str::<syn::Type>(&p.type_name) {
                    Ok(ty) => TypeNode::from_syn_item(&ty),
                    Err(_) => TypeNode::Path(p.type_name.clone()),
                };
                fun_scope.borrow_mut().insert_var(p.name.clone(), ty_node);
            }
            // Infer let-statement variables in function body
            if let Some(body) = &f.body {
                for stmt in &body.statements {
                    if let Statement::Let(let_stmt) = stmt {
                        if let Some(init) = &let_stmt.initial_value {
                            if let Some(vty) = infer_expr_type(init, &fun_scope, table) {
                                fun_scope
                                    .borrow_mut()
                                    .insert_var(let_stmt.name.clone(), vty);
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

#[allow(clippy::too_many_lines)]
fn infer_expr_type(expr: &Expression, scope: &ScopeRef, table: &SymbolTable) -> Option<TypeNode> {
    match expr {
        Expression::Identifier(id) => {
            // Qualified path resolution: module::Name
            if id.name.contains("::") {
                let (module, rest) = id.name.split_once("::").unwrap();
                if let Some(mod_scope) = table.mod_scopes.get(module) {
                    if let Some(defs) = mod_scope.borrow().lookup_def(rest) {
                        if let Some(def) = defs.first() {
                            // infer type from found definition
                            let ty = match def {
                                Definition::Const(c) => c.type_.to_type_node(),
                                Definition::Static(s) => s.ty.to_type_node(),
                                Definition::Function(f) => f.returns.clone(),
                                Definition::Type(t) => match syn::parse_str::<syn::Type>(&t.ty) {
                                    Ok(ty) => TypeNode::from_syn_item(&ty),
                                    Err(_) => TypeNode::Path(t.name.clone()),
                                },
                                Definition::Struct(s) | Definition::Contract(s) => {
                                    TypeNode::Path(s.name.clone())
                                }
                                Definition::Enum(e) => TypeNode::Path(e.name.clone()),
                                Definition::Union(u) => TypeNode::Path(u.name.clone()),
                                Definition::Module(m) => TypeNode::Path(m.name.clone()),
                                Definition::TraitAlias(ta) => TypeNode::Path(ta.name.clone()),
                                _ => return None,
                            };
                            return Some(ty);
                        }
                    }
                }
            }
            if let Some(v) = scope.borrow().lookup_var(&id.name) {
                return Some(v);
            }
            if let Some(defs) = scope.borrow().lookup_def(&id.name) {
                if let Some(def) = defs.first() {
                    let ty_node = match def {
                        Definition::Const(c) => c.type_.to_type_node(),
                        Definition::Static(s) => s.ty.to_type_node(),
                        Definition::Function(f) => f.returns.clone(),
                        Definition::Type(t) => {
                            // Top-level type alias: parse its RHS
                            match syn::parse_str::<syn::Type>(&t.ty) {
                                Ok(ty) => TypeNode::from_syn_item(&ty),
                                Err(_) => TypeNode::Path(t.name.clone()),
                            }
                        }
                        Definition::Struct(s) | Definition::Contract(s) => {
                            TypeNode::Path(s.name.clone())
                        }
                        Definition::Enum(e) => TypeNode::Path(e.name.clone()),
                        Definition::Union(u) => TypeNode::Path(u.name.clone()),
                        Definition::Module(m) => TypeNode::Path(m.name.clone()),
                        Definition::TraitAlias(ta) => TypeNode::Path(ta.name.clone()),
                        _ => return None,
                    };
                    return Some(ty_node);
                }
            }
            None
        }
        Expression::Lit(lit_expr) => match &lit_expr.value {
            Literal::Bool(_) => Some(TypeNode::Path("bool".to_string())),
            Literal::Byte(_) => Some(TypeNode::Path("u8".to_string())),
            Literal::Char(_) => Some(TypeNode::Path("char".to_string())),
            Literal::Int(_) => Some(TypeNode::Path("i128".to_string())),
            Literal::Float(_) => Some(TypeNode::Path("f64".to_string())),
            Literal::String(_) => Some(TypeNode::Path("&str".to_string())),
            Literal::BString(_) => Some(TypeNode::Path("&[u8]".to_string())),
            Literal::CString(_) => Some(TypeNode::Path("*const c_char".to_string())),
        },
        Expression::Binary(bin) => {
            match bin {
                // Comparison operators return bool
                Binary::Eq(b)
                | Binary::Ne(b)
                | Binary::Lt(b)
                | Binary::Le(b)
                | Binary::Gt(b)
                | Binary::Ge(b) => {
                    let _ = infer_expr_type(&b.left, scope, table)?;
                    let _ = infer_expr_type(&b.right, scope, table)?;
                    Some(TypeNode::Path("bool".to_string()))
                }
                // Arithmetic and bitwise operators: types must match
                Binary::Add(b)
                | Binary::AddAssign(b)
                | Binary::Sub(b)
                | Binary::SubAssign(b)
                | Binary::Mul(b)
                | Binary::MulAssign(b)
                | Binary::Div(b)
                | Binary::DivAssign(b)
                | Binary::Mod(b)
                | Binary::ModAssign(b)
                | Binary::BitXor(b)
                | Binary::BitXorAssign(b)
                | Binary::BitAnd(b)
                | Binary::BitAndAssign(b)
                | Binary::BitOr(b)
                | Binary::BitOrAssign(b)
                | Binary::Shl(b)
                | Binary::ShlAssign(b)
                | Binary::Shr(b)
                | Binary::ShrAssign(b)
                | Binary::And(b)
                | Binary::Or(b) => {
                    let lt = infer_expr_type(&b.left, scope, table)?;
                    let rt = infer_expr_type(&b.right, scope, table)?;
                    if lt == rt {
                        Some(lt)
                    } else {
                        None
                    }
                }
            }
        }
        Expression::Unary(u) => {
            let inner = match u {
                Unary::Deref(inner) | Unary::Not(inner) | Unary::Neg(inner) => &inner.expression,
            };
            infer_expr_type(inner, scope, table)
        }
        Expression::FunctionCall(fc) => {
            for def in scope.borrow().lookup_def(&fc.function_name)? {
                if let Definition::Function(f) = def {
                    return Some(f.returns.clone());
                }
            }
            None
        }
        Expression::MethodCall(mc) => {
            let base_ty = infer_expr_type(&mc.base, scope, table)?;
            if let TypeNode::Path(type_name) = base_ty {
                if let Some(methods) = table.methods.get(&type_name) {
                    for f in methods {
                        if f.name == mc.method_name {
                            return Some(f.returns.clone());
                        }
                    }
                }
            }
            None
        }
        Expression::MemberAccess(ma) => {
            let base_ty = infer_expr_type(&ma.base, scope, table)?;
            if let TypeNode::Path(type_name) = base_ty {
                if let Some(defs) = scope.borrow().lookup_def(&type_name) {
                    for def in defs {
                        if let Definition::Struct(s) = def {
                            for (field, fty) in &s.fields {
                                if &ma.member_name == field {
                                    // field type from AST
                                    return Some(fty.to_type_node());
                                }
                            }
                        }
                    }
                }
            }
            None
        }
        Expression::Return(r) => {
            // Return expression type or unit
            if let Some(e) = &r.expression {
                infer_expr_type(e, scope, table)
            } else {
                Some(TypeNode::Tuple(Vec::new()))
            }
        }
        _ => None,
    }
}
