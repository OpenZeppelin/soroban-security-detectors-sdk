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
    children: Vec<ScopeRef>,
    definitions: HashMap<String, Vec<Definition>>,
    variables: HashMap<String, TypeNode>,
}

impl Scope {
    fn new(parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            parent,
            children: Vec::new(),
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

    fn lookup_symbol(&self, name: &str) -> Option<TypeNode> {
        if let Some(ty) = self.variables.get(name) {
            return Some(ty.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup_symbol(name);
        }
        None
    }

    fn lookdown_symbol(&self, name: &str) -> Option<TypeNode> {
        if let Some(ty) = self.variables.get(name) {
            return Some(ty.clone());
        }
        for child in &self.children {
            if let Some(ty) = child.borrow().lookdown_symbol(name) {
                return Some(ty);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scope: ScopeRef,
    /// Map file/module name to its lexical scope
    mod_scopes: HashMap<String, ScopeRef>,
    /// Map from type name to its methods (from `impl Type { ... }` blocks).
    methods: HashMap<String, Vec<Rc<Function>>>,
}

impl SymbolTable {
    #[must_use]
    pub fn from_codebase(codebase: &Codebase<SealedState>) -> Self {
        let root = Scope::new(None);
        let mut table = SymbolTable {
            scope: root.clone(),
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
            root.borrow_mut().children.push(module_scope.clone());
            table
                .mod_scopes
                .insert(mod_name.clone(), module_scope.clone());
            for child in file.children.borrow().iter() {
                let FileChildType::Definition(def) = child;
                process_definition(def.clone(), &root, &mut table, codebase);
                if let Definition::Module(m) = def {
                    process_module(m, &module_scope, &mod_name, &mut table, codebase);
                } else {
                    process_definition(def.clone(), &module_scope, &mut table, codebase);
                }
            }
        }

        table
    }

    #[must_use]
    pub fn lookup_def(&self, name: &str) -> Option<Vec<Definition>> {
        self.scope.borrow().lookup_def(name)
    }

    #[must_use]
    pub fn lookup_symbol(&self, name: &str) -> Option<TypeNode> {
        self.scope.borrow().lookup_symbol(name)
    }

    #[must_use]
    pub fn lookdown_symbol(&self, name: &str) -> Option<TypeNode> {
        self.scope.borrow().lookdown_symbol(name)
    }

    #[must_use]
    pub fn infer_expr_type(
        &self,
        expr: &Expression,
        codebase: &Codebase<SealedState>,
    ) -> Option<TypeNode> {
        infer_expr_type(expr, &self.scope, self, codebase)
    }

    #[must_use]
    pub fn resolve_path(&self, path: &str) -> Option<Definition> {
        let parts: Vec<&str> = path.split("::").collect();
        if parts.is_empty() {
            return None;
        }

        let mut scope = self.scope.clone();
        for seg in &parts[..parts.len().saturating_sub(1)] {
            let defs = scope.borrow().lookup_def(seg)?;
            let mut module_found = None;
            for def in defs {
                if let Definition::Module(m) = &def {
                    module_found = Some(m.clone());
                    break;
                }
            }
            let module = module_found?;
            let full = module.name.clone();
            let next_scope = self.mod_scopes.get(&full)?;
            scope = next_scope.clone();
        }
        let name = parts[parts.len() - 1];
        let defs = scope.borrow().lookup_def(name)?;
        defs.into_iter().next()
    }

    #[must_use]
    #[allow(clippy::needless_pass_by_value)]
    pub fn find_self_type_for_method(&self, function: Rc<Function>) -> Option<String> {
        self.methods.iter().find_map(|(type_name, methods)| {
            if methods.iter().any(|m| m.name == function.name) {
                Some(type_name.clone())
            } else {
                None
            }
        })
    }
}

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

fn process_module(
    module: &Rc<Module>,
    parent_scope: &ScopeRef,
    parent_path: &String,
    table: &mut SymbolTable,
    codebase: &Codebase<SealedState>,
) {
    let module_scope = Scope::new(Some(parent_scope.clone()));
    parent_scope
        .borrow_mut()
        .children
        .push(module_scope.clone());
    let path = if parent_path.is_empty() {
        module.name.clone()
    } else {
        format!("{}::{}", parent_path, module.name)
    };
    table.mod_scopes.insert(path.clone(), module_scope.clone());
    module_scope
        .borrow_mut()
        .insert_def(module.name.clone(), Definition::Module(module.clone()));
    if let Some(defs) = &module.definitions {
        for def in defs {
            match def {
                Definition::Module(inner_mod) => {
                    process_module(inner_mod, &module_scope, &path, table, codebase);
                }
                _ => process_definition(def.clone(), &module_scope, table, codebase),
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn process_definition(
    def: Definition,
    scope: &ScopeRef,
    table: &mut SymbolTable,
    codebase: &Codebase<SealedState>,
) {
    if let Some(name) = get_definition_name(&def) {
        scope.borrow_mut().insert_def(name.clone(), def.clone());
    }
    match def {
        Definition::Module(m) => {
            let module_scope = Scope::new(Some(scope.clone()));
            scope.borrow_mut().children.push(module_scope.clone());
            if let Some(defs) = &m.definitions {
                for sub in defs {
                    process_definition(sub.clone(), &module_scope.clone(), table, codebase);
                }
            }
        }
        Definition::Struct(s) => {
            let struct_scope = Scope::new(Some(scope.clone()));
            scope.borrow_mut().children.push(struct_scope.clone());
            for (field, fty) in &s.fields {
                struct_scope
                    .borrow_mut()
                    .insert_var(field.clone(), fty.to_type_node());
            }
        }
        Definition::Implementation(impl_node) => {
            if let Some(for_type) = &impl_node.for_type {
                let impl_scope = Scope::new(Some(scope.clone()));
                scope.borrow_mut().children.push(impl_scope.clone());
                table
                    .methods
                    .insert(for_type.to_type_node().name(), impl_node.functions.clone());
                for f in &impl_node.functions {
                    impl_scope
                        .borrow_mut()
                        .insert_def(f.name.clone(), Definition::Function(f.clone()));
                    process_definition(
                        Definition::Function(f.clone()),
                        &impl_scope,
                        table,
                        codebase,
                    );
                }
            }
        }
        Definition::Function(f) => {
            let fun_scope = Scope::new(Some(scope.clone()));
            scope.borrow_mut().children.push(fun_scope.clone());
            for p in &f.parameters {
                let mut ty_node = match parse_str::<syn::Type>(&p.type_name) {
                    Ok(ty) => TypeNode::from_syn_item(&ty),
                    Err(_) => TypeNode::Path(p.type_name.clone()),
                };
                if let Some(self_ty) = table.find_self_type_for_method(f.clone()) {
                    ty_node = match ty_node {
                        TypeNode::Path(ref name) if name == "Self" => {
                            TypeNode::Path(self_ty.clone())
                        }
                        TypeNode::Reference {
                            inner: old_inner,
                            mutable,
                            is_explicit_reference,
                        } if old_inner.name() == "Self" => TypeNode::Reference {
                            inner: Box::new(TypeNode::Path(self_ty.clone())),
                            mutable,
                            is_explicit_reference,
                        },
                        TypeNode::Ptr { inner, mutable } if inner.name() == "Self" => {
                            TypeNode::Ptr {
                                inner: Box::new(TypeNode::Path(self_ty.clone())),
                                mutable,
                            }
                        }
                        other => other,
                    };
                }
                fun_scope.borrow_mut().insert_var(p.name.clone(), ty_node);
            }
            if let Some(self_ty) = table.find_self_type_for_method(f.clone()) {
                let new_ret = match f.returns.borrow().clone() {
                    TypeNode::Path(name) if name == "Self" => TypeNode::Path(self_ty.clone()),
                    TypeNode::Reference {
                        inner: old_inner,
                        mutable,
                        is_explicit_reference,
                    } if old_inner.name() == "Self" => TypeNode::Reference {
                        inner: Box::new(TypeNode::Path(self_ty.clone())),
                        mutable,
                        is_explicit_reference,
                    },
                    TypeNode::Ptr { inner, mutable } if inner.name() == "Self" => TypeNode::Ptr {
                        inner: Box::new(TypeNode::Path(self_ty.clone())),
                        mutable,
                    },
                    other => other,
                };
                *f.returns.borrow_mut() = new_ret;
            }
            if let Some(body) = &f.body {
                for stmt in &body.statements {
                    if let Statement::Let(let_stmt) = stmt {
                        if let Some(init) = &let_stmt.initial_value {
                            if let Some(vty) = infer_expr_type(init, &fun_scope, table, codebase) {
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
fn infer_expr_type(
    expr: &Expression,
    scope: &ScopeRef,
    table: &SymbolTable,
    codebase: &Codebase<SealedState>,
) -> Option<TypeNode> {
    match expr {
        Expression::Identifier(id) => {
            // Qualified path resolution: module::Name
            if id.name.contains("::") {
                let (module, rest) = id.name.split_once("::").unwrap();
                if let Some(mod_scope) = table.mod_scopes.get(module) {
                    if let Some(defs) = mod_scope.borrow().lookup_def(rest) {
                        if let Some(def) = defs.first() {
                            let ty = match def {
                                Definition::Const(c) => c.type_.to_type_node(),
                                Definition::Static(s) => s.ty.to_type_node(),
                                Definition::Function(f) => f.returns.borrow().clone(),
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
            if let Some(v) = table.lookdown_symbol(&id.name) {
                return Some(v);
            }
            if let Some(defs) = scope.borrow().lookup_def(&id.name) {
                if let Some(def) = defs.first() {
                    let ty_node = match def {
                        Definition::Const(c) => c.type_.to_type_node(),
                        Definition::Static(s) => s.ty.to_type_node(),
                        Definition::Function(f) => f.returns.borrow().clone(),
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
        Expression::Literal(lit_expr) => match &lit_expr.value {
            Literal::Bool(_) => Some(TypeNode::Path("bool".to_string())),
            Literal::Byte(_) => Some(TypeNode::Path("u8".to_string())),
            Literal::Char(_) => Some(TypeNode::Path("char".to_string())),
            Literal::Int(_) => Some(TypeNode::Path("i32".to_string())),
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
                    let _ = infer_expr_type(&b.left, scope, table, codebase)?;
                    let _ = infer_expr_type(&b.right, scope, table, codebase)?;
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
                    let lt = infer_expr_type(&b.left, scope, table, codebase)?;
                    let rt = infer_expr_type(&b.right, scope, table, codebase)?;
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
            infer_expr_type(inner, scope, table, codebase)
        }
        Expression::FunctionCall(fc) => {
            if let Some(defs) = scope.borrow().lookup_def(&fc.function_name) {
                for def in defs {
                    if let Definition::Function(f) = def {
                        return Some(f.returns.borrow().clone());
                    }
                }
            }
            if let Expression::Identifier(base) = &fc.expression {
                let name = base.name.clone();
                if name == "Some" {
                    if let Some(arg) = &fc.parameters.first() {
                        if let Some(ty) = infer_expr_type(arg, scope, table, codebase) {
                            return Some(TypeNode::Path(format!("Option<{}>", ty.name())));
                        }
                    }
                    return Some(TypeNode::Path("Option<_>".to_string()));
                }
                if name == "None" {
                    return Some(TypeNode::Path("Option<_>".to_string()));
                }
                if name == "Ok" {
                    if let Some(arg) = &fc.parameters.first() {
                        if let Some(ty) = infer_expr_type(arg, scope, table, codebase) {
                            return Some(TypeNode::Path(format!("Result<{}, _>", ty.name())));
                        }
                    }
                    return Some(TypeNode::Path("Result<_, _>".to_string()));
                }
                if name.contains("::") {
                    let (module, rest) = name.split_once("::").unwrap();
                    let module = module.trim();
                    let rest = rest.trim();
                    if let Some(mod_scope) = table.mod_scopes.get(module) {
                        if let Some(defs) = mod_scope.borrow().lookup_def(rest) {
                            if let Some(Definition::Function(f)) = defs.first() {
                                return Some(f.returns.borrow().clone());
                            }
                        }
                    }
                    if ["Vec", "HashMap"].contains(&module) {
                        return Some(TypeNode::Reference {
                            inner: Box::new(TypeNode::Path(module.to_string())),
                            mutable: false,
                            is_explicit_reference: false,
                        });
                    }
                } else if let Some(v) = scope.borrow().lookup_symbol(&name) {
                    return Some(v);
                }
            }
            None
        }
        Expression::MethodCall(mc) => {
            let base_ty = infer_expr_type(&mc.base, scope, table, codebase)?;
            if let TypeNode::Path(type_name) = base_ty {
                if let Some(methods) = table.methods.get(&type_name) {
                    for f in methods {
                        if f.name == mc.method_name {
                            return Some(f.returns.borrow().clone());
                        }
                    }
                }
            }
            None
        }
        Expression::MemberAccess(ma) => {
            let base_ty = infer_expr_type(&ma.base, scope, table, codebase)?;
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
                infer_expr_type(e, scope, table, codebase)
            } else {
                Some(TypeNode::Tuple(Vec::new()))
            }
        }
        Expression::Cast(c) => {
            let mut ty_node = c.target_type.to_type_node();
            if ty_node.is_self() {
                if let Some(NodeKind::Statement(Statement::Definition(Definition::Function(f)))) =
                    codebase.get_parent_container(c.id)
                {
                    if let Some(self_ty) = table.find_self_type_for_method(f.clone()) {
                        ty_node = match ty_node {
                            TypeNode::Path(ref name) if name == "Self" => {
                                TypeNode::Path(self_ty.clone())
                            }
                            TypeNode::Reference {
                                inner: old_inner,
                                mutable,
                                is_explicit_reference,
                            } if old_inner.name() == "Self" => TypeNode::Reference {
                                inner: Box::new(TypeNode::Path(self_ty.clone())),
                                mutable,
                                is_explicit_reference,
                            },
                            TypeNode::Ptr { inner, mutable } if inner.name() == "Self" => {
                                TypeNode::Ptr {
                                    inner: Box::new(TypeNode::Path(self_ty.clone())),
                                    mutable,
                                }
                            }
                            other => other,
                        };
                    }
                }
            }
            Some(ty_node)
        }
        Expression::Closure(cl) => Some(cl.returns.to_type_node()),
        Expression::Macro(m) => {
            if m.name == "format" {
                // format! macro returns String
                return Some(TypeNode::Path("String".to_string()));
            }
            if m.name == "vec" {
                let re =
                    regex::Regex::new(r"^\s*(?P<elems>.+?)(?:\s*;\s*(?P<count>.+))?\s*$").unwrap();
                if let Some(caps) = re.captures(&m.text) {
                    if caps.name("count").is_some() {
                        let expr_str = caps.name("elems").unwrap().as_str();
                        if let Ok(expr) = syn::parse_str::<syn::Type>(expr_str) {
                            let ty = TypeNode::from_syn_item(&expr);
                            return Some(TypeNode::Reference {
                                inner: Box::new(ty),
                                mutable: false,
                                is_explicit_reference: false,
                            });
                        }
                    } else {
                        let elems_str = caps.name("elems").unwrap().as_str();
                        let elems: Vec<&str> = elems_str
                            .split(',')
                            .map(str::trim)
                            .filter(|s| !s.is_empty())
                            .collect();
                        if let Some(first) = elems.first() {
                            if let Ok(expr) = syn::parse_str::<syn::Type>(first) {
                                let ty = TypeNode::from_syn_item(&expr);
                                return Some(TypeNode::Reference {
                                    inner: Box::new(ty),
                                    mutable: false,
                                    is_explicit_reference: false,
                                });
                            }
                            if let Ok(syn::Lit::Int(_)) = syn::parse_str::<syn::Lit>(first) {
                                return Some(TypeNode::Reference {
                                    inner: Box::new(TypeNode::Path("Vec<i32>".to_string())),
                                    mutable: false,
                                    is_explicit_reference: false,
                                });
                            }
                        }
                    }
                }
                // Fallback if parsing fails
                return Some(TypeNode::Path("Vec<_>".to_string()));
            }
            None
        }
        Expression::Tuple(t) => {
            let mut types = Vec::new();
            for e in &t.elements {
                if let Some(ty) = infer_expr_type(e, scope, table, codebase) {
                    types.push(ty);
                } else {
                    types.push(TypeNode::Empty);
                }
            }
            Some(TypeNode::Tuple(types))
        }
        _ => None,
    }
}
