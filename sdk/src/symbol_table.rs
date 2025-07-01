use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use syn::parse_str;

use crate::{
    custom_type::Type,
    definition::Definition,
    directive::Use,
    expression::{BinOp, Expression},
    function::Function,
    literal::Literal,
    misc::Misc,
    node::Visibility,
    node_type::NodeType,
    prelude::ExternPrelude,
    statement::{Block, Statement},
};

pub(crate) type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Clone)]
pub(crate) enum DefinitionRef {
    Ref(String, Definition),
    QualifiedName(String),
}

impl DefinitionRef {
    pub(crate) fn name(&self) -> String {
        match self {
            DefinitionRef::QualifiedName(name) | DefinitionRef::Ref(name, _) => name.clone(),
        }
    }
    // pub(crate) fn as_def(&self) -> Option<Definition> {
    //     match self {
    //         DefinitionRef::Ref(_, def) => Some(def.clone()),
    //         DefinitionRef::QualifiedName(_) => None,
    //     }
    // }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Scope {
    pub(crate) id: u32,
    pub(crate) name: String,
    pub(crate) parent: Option<ScopeRef>,
    pub(crate) children: Vec<ScopeRef>,
    pub(crate) imports: Vec<Rc<Use>>,
    import_aliases: HashMap<String, String>,
    pub(crate) definitions: HashMap<String, Definition>,
    variables: HashMap<String, (u32, NodeType)>,
    // Structs and their methods
    methods: HashMap<String, Vec<Rc<Function>>>,
    // Enums and their methods
    enums: HashMap<DefinitionName, Vec<Rc<Function>>>,
}

impl Scope {
    pub(crate) fn new(id: u32, name: String, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            id,
            name,
            parent,
            children: Vec::new(),
            imports: Vec::new(),
            import_aliases: HashMap::new(),
            definitions: HashMap::new(),
            variables: HashMap::new(),
            methods: HashMap::new(),
            enums: HashMap::new(),
        }))
    }

    // pub(crate) fn insert_def(&mut self, qualified_name: String, def: Definition) {
    //     self.definitions.insert(qualified_name, def);
    // }

    pub fn visible_child(&self, ident: &str) -> Option<DefOrScopeRef> {
        /* ---------- 1.  sub‑modules ---------- */
        if let Some(sub) = self
            .children
            .iter()
            .find(|s| s.borrow().name.rsplit("::").next() == Some(ident))
        {
            return Some(DefOrScopeRef::Module(sub.clone()));
        }

        /* ---------- 2.  local definitions ---------- */
        if let Some(def) = self.definitions.get(ident) {
            return Some(DefOrScopeRef::Definition(def.clone()));
        }

        /* ---------- 3.  resolved imports ---------- */
        for u in &self.imports {
            // The `target` map holds *all* paths imported by this `use` tree.
            // Key = the path string as it appeared in the code; we need to
            // compare its *bound* name inside the scope:
            //
            //   use foo::Bar;              // bound name = "Bar"
            //   use foo::Baz as Qux;       // bound name = "Qux"  (alias)
            //   use foo::{bar as spam};    // "spam"
            //
            // During parsing you already stored these bound names in
            // `imported_types` (with "%alias" if present).  When the resolver
            // succeeds it records the `Definition` (Some) in `target`.

            for (key, def_opt) in u.target.borrow().iter() {
                // Extract bound name = alias if present, else last path segment
                let bound = if let Some((_, alias)) = key.split_once('%') {
                    alias
                } else {
                    key.rsplit("::").next().unwrap_or(key)
                };
                if bound == ident {
                    if let Some(def) = def_opt {
                        return Some(DefOrScopeRef::Definition(def.clone()));
                    }
                    // Still `None` ⇒ import not resolved, ignore for now
                }
            }
        }

        None
    }

    fn get_crate_name(&self) -> String {
        if let Some(parent) = &self.parent {
            parent.borrow().get_crate_name()
        } else {
            self.name.clone()
        }
    }

    fn relative_to_absolute_path(&self, path: &str) -> String {
        if path.starts_with("crate") {
            let crate_name = self.get_crate_name();
            return path.replace("crate", &crate_name);
        }
        path.to_string()
    }

    fn resolve_self(&self) -> Option<DefinitionRef> {
        if let Some((_, ty)) = self.variables.get("self") {
            return self.lookup_def(&ty.name());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().resolve_self();
        }
        None
    }

    fn lookup_def(&self, name: &str) -> Option<DefinitionRef> {
        let mut name: &str = name;
        if name.contains("::") {
            name = name.split("::").last().unwrap_or(name);
        }
        if self.import_aliases.contains_key(name) {
            name = self.import_aliases.get(name).unwrap();
        }
        if let Some(def) = self.try_get_definition(name) {
            if self
                .definitions
                .keys()
                .find_map(|def_name| {
                    if def_name == name {
                        Some(DefinitionRef::Ref(def_name.clone(), def.clone()))
                    } else {
                        None
                    }
                })
                .is_some()
            {
                let q_name = format!("{}::{}", self.name, def.name());
                return Some(DefinitionRef::Ref(q_name, def));
            }
            return Some(DefinitionRef::Ref(name.to_string(), def.clone()));
        }
        for import in &self.imports {
            for it in &import.imported_types {
                if it == name || it.ends_with(name) {
                    if let Some(Some(def)) = import.target.borrow().get(it) {
                        return Some(DefinitionRef::Ref(it.clone(), def.clone()));
                    }
                    return Some(DefinitionRef::QualifiedName(it.clone()));
                }
            }
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup_def(name);
        }
        None
    }

    fn insert_var(&mut self, name: String, id: u32, ty: NodeType) {
        self.variables.insert(name, (id, ty));
    }

    fn lookup_symbol(&self, name: &str) -> Option<NodeType> {
        if let Some(ty) = self.variables.get(name) {
            return Some(ty.1.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup_symbol(name);
        }
        None
    }

    pub(crate) fn try_get_definition(&self, name: &str) -> Option<Definition> {
        self.definitions.get(name).cloned()
    }

    fn qualify_definition_name(&self, name: &str) -> Option<String> {
        if let Some(res) = self.definitions.keys().find_map(|def_name| {
            if def_name == name {
                Some(def_name.clone())
            } else {
                None
            }
        }) {
            return Some(res);
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().qualify_definition_name(name);
        }
        None
    }

    pub fn crate_root_id(&self) -> u32 {
        if let Some(parent) = &self.parent {
            parent.borrow().crate_root_id()
        } else {
            self.id
        }
    }

    pub fn crate_root(&self) -> Option<ScopeRef> {
        if let Some(parent) = &self.parent {
            if parent.borrow().parent.is_none() {
                Some(parent.clone())
            } else {
                parent.borrow().crate_root()
            }
        } else {
            None
        }
    }

    // fn try_get_definition_by_name(&self, name: &str) -> Option<Definition> {
    //     self.definitions.keys().find_map(|def_name| {
    //         if def_name.name == name {
    //             self.definitions.get(def_name).cloned()
    //         } else {
    //             None
    //         }
    //     })
    // }

    // fn try_get_definition_by_qualified_name(&self, name: &str) -> Option<Definition> {
    //     self.definitions.keys().find_map(|def_name| {
    //         if def_name.qualified_name == name {
    //             self.definitions.get(def_name).cloned()
    //         } else {
    //             None
    //         }
    //     })
    // }

    // fn lookdown_symbol(&self, name: &str) -> Option<NodeType> {
    //     if let Some(ty) = self.variables.get(name) {
    //         return Some(ty.1.clone());
    //     }
    //     for child in &self.children {
    //         if let Some(ty) = child.borrow().lookdown_symbol(name) {
    //             return Some(ty);
    //         }
    //     }
    //     None
    // }

    // fn get_struct_methods_by_struct_name(&self, name: &str) -> Option<Vec<Rc<Function>>> {
    //     // if let Some(def_ref) = self.lookup_def(name) {
    //     //     DefinitionRef::Ref(def) => {}
    //     //     DefinitionRef::QualifiedName(q_name) => { //it means that the type is imported but not yet resolved

    //     //     }
    //     // }
    //     if let Some(methods) = self
    //         .methods
    //         .iter()
    //         .find(|(type_name, _)| type_name.name == name)
    //         .map(|(_, methods)| methods.clone())
    //     {
    //         return Some(methods);
    //     }
    //     if let Some(parent) = &self.parent {
    //         if let Some(res) = parent.borrow().get_struct_methods_by_struct_name(name) {
    //             return Some(res);
    //         }
    //     }
    //     None
    // }
    //F-IXME write qualify_type_name

    // fn get_struct_methods_by_qualified_struct_name(
    //     &self,
    //     name: &str,
    // ) -> Option<&Vec<Rc<Function>>> {
    //     self.methods
    //         .iter()
    //         .find(|(type_name, _)| type_name.qualified_name == name)
    //         .map(|(_, methods)| methods)
    // }

    fn functions(&self) -> impl Iterator<Item = &Rc<Function>> {
        self.definitions.values().filter_map(|def| {
            if let Definition::Function(f) = def {
                Some(f)
            } else {
                None
            }
        })
    }

    fn methods(&self) -> impl Iterator<Item = &Rc<Function>> {
        self.methods.values().flat_map(|methods| methods.iter())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
struct DefinitionName {
    name: String,
    qualified_name: String,
}

pub(crate) enum DefOrScopeRef {
    Module(ScopeRef),
    Definition(Definition),
}
impl DefOrScopeRef {
    pub(crate) fn as_module(&self) -> Option<Rc<RefCell<Scope>>> {
        match self {
            DefOrScopeRef::Module(scope) => Some(scope.clone()),
            DefOrScopeRef::Definition(_) => None,
        }
    }

    // pub(crate) fn as_definition(&self) -> Option<Definition> {
    //     match self {
    //         DefOrScopeRef::Module(_) => None,
    //         DefOrScopeRef::Definition(def) => Some(def.clone()),
    //     }
    // }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolTable {
    pub(crate) scopes: HashMap<u32, ScopeRef>,
    mod_scopes: HashMap<String, ScopeRef>,
    defs: HashMap<(u32, String), Definition>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new()
    }
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        SymbolTable {
            scopes: HashMap::new(),
            mod_scopes: HashMap::new(),
            defs: HashMap::new(),
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn insert_scope(&mut self, scope: ScopeRef) {
        self.scopes.insert(scope.borrow().id, scope.clone());
        self.mod_scopes
            .insert(scope.borrow().name.clone(), scope.clone());
    }

    pub(crate) fn insert_def(
        &mut self,
        scope_id: u32,
        qualified_name: String,
        def: Definition,
    ) -> Option<Definition> {
        self.defs.insert((scope_id, qualified_name), def)
    }

    fn get_scope_by_def_id(&self, id: u32) -> Option<ScopeRef> {
        for ((s_id, _), v) in &self.defs {
            if v.id() == id {
                return self.scopes.get(s_id).cloned();
            }
        }
        None
    }

    pub(crate) fn build_symbol_tables(&mut self) {
        let scopes = self.scopes.values().cloned().collect::<Vec<_>>();
        for scope in scopes {
            let functions = scope
                .borrow()
                .definitions
                .iter()
                .filter_map(|(_, def)| {
                    if let Definition::Function(f) = def {
                        Some(f.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            for mut function in functions {
                self.build_function_symbol_table(&scope, &mut function);
            }
            let scope_borrow = scope.borrow();
            let methods = scope_borrow.methods.values().cloned().collect::<Vec<_>>();
            drop(scope_borrow);
            for methods_group in methods {
                for method in methods_group {
                    self.build_function_symbol_table(&scope, &mut method.clone());
                }
            }
            let scope_borrow = scope.borrow();
            let functions = scope_borrow.functions().collect::<Vec<_>>();
            for function in functions {
                self.build_function_symbol_table(&scope, &mut function.clone());
            }
        }
    }

    fn build_function_symbol_table(&mut self, scope: &ScopeRef, f: &mut Rc<Function>) {
        let self_type = self.find_self_type_for_method(f.clone());
        let fn_scope = self
            .scopes
            .get(&f.id)
            .unwrap_or_else(|| {
                panic!(
                    "Function scope with id {} not found for function {}",
                    f.id, f.name
                )
            })
            .clone();
        for p in &f.parameters {
            // let mut ty_node = match parse_str::<syn::Type>(&p.type_name) {
            //     Ok(ty) => NodeType::from_syn_item(&ty),
            //     Err(_) => NodeType::Path(p.type_name.clone()),
            // };
            // if ty_node.is_self() && self_type.is_some() {
            //     let self_ty_ref = self_type.as_ref().unwrap();
            //     ty_node = match ty_node {
            //         NodeType::Path(_) => NodeType::Path(self_ty_ref.clone()),
            //         NodeType::Reference {
            //             inner: _,
            //             mutable,
            //             is_explicit_reference,
            //         } => NodeType::Reference {
            //             inner: Box::new(NodeType::Path(self_ty_ref.clone())),
            //             mutable,
            //             is_explicit_reference,
            //         },
            //         NodeType::Ptr { inner: _, mutable } => NodeType::Ptr {
            //             inner: Box::new(NodeType::Path(self_ty_ref.clone())),
            //             mutable,
            //         },
            //         other => other,
            //     };
            // }
            // if let Some(def_ref) = scope.borrow().lookup_def(&ty_node.name()) {
            //     //T-ODO: should this be a part or re-qualification?
            //     match def_ref {
            //         DefinitionRef::Ref(qualified_name, _) => {
            //             ty_node = NodeType::Path(qualified_name);
            //         }
            //         DefinitionRef::QualifiedName(qualified_name) => {
            //             ty_node = NodeType::Path(qualified_name);
            //         }
            //     }
            // }
            let mut param_ty = NodeType::from_string(&p.type_name);
            if param_ty.is_self() {
                if let Some(self_ty_name) = &self_type {
                    param_ty.replace_path(self_ty_name.clone());
                }
            }
            let param_pure_type_name = param_ty.pure_name();

            if let Some(DefinitionRef::Ref(qname, _)) =
                scope.borrow().lookup_def(&param_pure_type_name)
            {
                param_ty.replace_path(qname.clone());
            }

            fn_scope
                .borrow_mut()
                .insert_var(p.name.clone(), p.id, param_ty);
        }
        let ty_node_name = &f.returns.borrow().pure_name();
        if let Some(def_ref) = scope.borrow().lookup_def(ty_node_name) {
            f.returns.borrow_mut().replace_path(def_ref.name());
        }
        if let Some(block) = &f.body {
            for stmt in &block.statements {
                process_statement(stmt, &fn_scope, self);
            }
        }
    }

    // fn lookup_methods(&self, scope: ScopeRef, def_name: String) -> Option<Vec<Rc<Function>>> {
    //     let mut name = def_name;
    //     if scope.borrow().import_aliases.contains_key(&name) {
    //         name = scope.borrow().import_aliases.get(&name).unwrap().clone();
    //     }
    //     if scope.borrow().methods.contains_key(&name) {
    //         return scope.borrow().methods.get(&name).cloned();
    //     }
    //     for import in &scope.borrow().imports {
    //         if import.imported_types.contains(&name) {
    //             if let Some(Some(def)) = import.target.borrow().get(&name) {
    //                 return self
    //                     .scopes
    //                     .get(&def.id())
    //                     .and_then(|s| s.borrow().methods.get(&name).cloned());
    //             }
    //         }
    //     }
    //     None
    // }

    // #[must_use]
    // #[allow(clippy::missing_panics_doc, clippy::too_many_lines)]
    // pub fn from_codebase(codebase: &Codebase<SealedState>) -> Self {
    //     let root = Scope::new(0, String::from("user_codebase"), None);
    //     let mut table = SymbolTable {
    //         scopes: HashMap::new(),
    //         defs: HashMap::new(),
    //         mod_scopes: HashMap::new(),
    //     };
    //     let (mut soroban_sdk_files, mut user_files): (Vec<_>, Vec<_>) =
    //         codebase.files.iter().partition(|f| {
    //             f.file_module_name().starts_with("soroban_sdk::")
    //                 || f.file_module_name().starts_with("soroban_sdk_macro")
    //         });
    //     soroban_sdk_files.sort_by_key(|f| f.path.clone());
    //     user_files.sort_by_key(|f| f.path.clone());
    //     SymbolTable::process_files(root.clone(), &mut table, codebase, soroban_sdk_files);
    //     // let root_mod = codebase
    //     //     .files
    //     //     .iter()
    //     //     .map(|f| f.path.split('/').collect::<Vec<_>>())
    //     //     .fold(None, |acc: Option<Vec<&str>>, path_parts| match acc {
    //     //         None => Some(path_parts),
    //     //         Some(common) => Some(
    //     //             common
    //     //                 .into_iter()
    //     //                 .zip(path_parts)
    //     //                 .take_while(|(a, b)| a == b)
    //     //                 .map(|(a, _)| a)
    //     //                 .collect(),
    //     //         ),
    //     //     })
    //     //     .unwrap_or_default()
    //     //     .join("/");
    //     SymbolTable::process_files(root.clone(), &mut table, codebase, user_files);
    //     table
    // }
    // #[allow(clippy::needless_pass_by_value)]
    // fn process_files(
    //     root: ScopeRef,
    //     table: &mut SymbolTable,
    //     codebase: &Codebase<SealedState>,
    //     files: Vec<&Rc<File>>,
    // ) {
    //     // Pass 1: Per-File Symbol Discovery
    //     let mut created_mod_scopes = vec![];
    //     for file in files {
    //         let file_mod_name = format!("test::{}", file.file_module_name());
    //         let file_mod_scope = Scope::new(file.id, file_mod_name.clone(), Some(root.clone()));
    //         created_mod_scopes.push(file_mod_scope.clone());
    //         root.borrow_mut().children.push(file_mod_scope.clone());
    //         table
    //             .mod_scopes
    //             .insert(file_mod_name.clone(), file_mod_scope.clone());
    //         let mut children = file.children.borrow().clone();
    //         children.sort_by(|a, b| match (a, b) {
    //             (NodeKind::Directive(_), NodeKind::Directive(_)) => std::cmp::Ordering::Equal,
    //             (NodeKind::Directive(_), _) => std::cmp::Ordering::Less,
    //             (_, NodeKind::Directive(_)) => std::cmp::Ordering::Greater,
    //             _ => std::cmp::Ordering::Equal,
    //         });
    //         for child in children {
    //             match &child {
    //                 NodeKind::Directive(Directive::Use(rc_use)) => {
    //                     file_mod_scope.borrow_mut().imports.push(rc_use.clone());
    //                 }
    //                 NodeKind::Definition(def) => {
    //                     process_definition(&file_mod_scope, def.clone(), table);
    //                 }
    //                 _ => {}
    //             }
    //         }
    //     }
    //     // Pass 2: Import Resolution and Linking
    //     for scope in &created_mod_scopes {
    //         let imports = scope.borrow().imports.clone();
    //         for import in imports {
    //             for imported in &import.imported_types {
    //                 let (mut path, _) = if let Some((orig, alias)) = imported.split_once('%') {
    //                     scope
    //                         .borrow_mut()
    //                         .import_aliases
    //                         .insert(alias.to_string(), orig.to_string());
    //                     (orig.to_string(), Some(alias.to_string()))
    //                 } else {
    //                     (imported.to_string(), None)
    //                 };
    //                 if let Some(def_ref) = table.resolve_path(scope.borrow().id, &path) {
    //                     match def_ref {
    //                         DefinitionRef::Ref(_, def) => {
    //                             import
    //                                 .target
    //                                 .borrow_mut()
    //                                 .insert(imported.clone(), Some(def.clone()));
    //                         }
    //                         DefinitionRef::QualifiedName(_) => {}
    //                     }
    //                 } else {
    //                     import.target.borrow_mut().insert(imported.clone(), None);
    //                 }
    //             }
    //         }
    //     }
    //     // let mut defs = table.scope.borrow_mut().definitions.clone();
    //     // let mut imps = table.scope.borrow_mut().imports.clone();
    //     for scope in &created_mod_scopes {
    //         SymbolTable::re_qualify_variables(scope, &mut HashMap::new(), &mut Vec::new());
    //     }
    //     // Re-qualify variables whose types match imported aliases
    //     // for (key, scope) in &table.mod_scopes {
    //     //     let mut to_update = Vec::new();
    //     //     for (var, (id, ty)) in &scope.borrow().variables {
    //     //         if let NodeType::Path(name) = ty {
    //     //             if let Some(full) = scope.borrow().import_aliases.get(name) {
    //     //                 if ty.name() != *full {
    //     //                     to_update.push((var.clone(), *id, full.clone()));
    //     //                 }
    //     //             }
    //     //         }
    //     //     }
    //     //     for (var, id, full) in to_update {
    //     //         scope.borrow_mut().insert_var(var, id, NodeType::Path(full));
    //     //     }
    //     // }
    //     // Pass 3: Fill types for missed symbols
    //     let mut scopes = created_mod_scopes;
    //     while let Some(scope) = scopes.pop() {
    //         let variables = scope.borrow().variables.clone();
    //         for (_, id_nt) in variables {
    //             match &id_nt.1 {
    //                 NodeType::Empty => {
    //                     if let Some(NodeKind::Statement(s)) = codebase.storage.find_node(id_nt.0) {
    //                         process_statement(&s, &scope, table, codebase);
    //                     }
    //                 }
    //                 NodeType::Path(_path) => {}
    //                 _ => {}
    //             }
    //         }
    //         for child in &scope.borrow().children {
    //             scopes.push(child.clone());
    //         }
    //     }
    // }
    // fn re_qualify_variables(
    //     scope: &ScopeRef,
    //     definitions: &mut HashMap<String, Definition>,
    //     imports: &mut Vec<Rc<Use>>,
    // ) {
    //     for import in &scope.borrow().imports {
    //         if !imports.iter().any(|i| i.id == import.id) {
    //             imports.push(import.clone());
    //         }
    //     }
    //     for (name, def) in &scope.borrow().definitions {
    //         if !definitions.contains_key(name) {
    //             definitions.insert(name.clone(), def.clone());
    //         }
    //     }
    //     for var in &mut scope.borrow_mut().variables {
    //         if let NodeType::Path(name) = &var.1 .1.clone() {
    //             for i in imports.iter() {
    //                 //what if we have this definition in the scope?
    //                 if let Some(imported) = i
    //                     .imported_types
    //                     .iter()
    //                     .find(|it| *it == name || it.split("::").last() == Some(name))
    //                 {
    //                     var.1 .1 = NodeType::Path(imported.clone());
    //                 }
    //             }
    //         }
    //     }
    //     for child in &scope.borrow().children {
    //         SymbolTable::re_qualify_variables(child, definitions, imports);
    //     }
    // }
    // #[must_use]
    // pub fn lookup_def(&self, name: &str) -> Option<DefinitionRef> {
    //     self.scope.borrow().lookup_def(name)
    // }
    // #[must_use]
    // pub fn lookup_symbol(&self, name: &str) -> Option<NodeType> {
    //     self.scopes.borrow().lookup_symbol(name)
    // }

    #[must_use]
    pub fn lookup_symbol_in_scope(&self, scope_id: u32, name: &str) -> Option<NodeType> {
        if self.scopes.contains_key(&scope_id) {
            return self
                .scopes
                .get(&scope_id)
                .unwrap()
                .borrow()
                .lookup_symbol(name);
        }
        let mut stack: Vec<ScopeRef> = self.scopes.values().cloned().collect();
        while let Some(scope) = stack.pop() {
            if scope.borrow().id == scope_id {
                return scope.borrow().lookup_symbol(name);
            }
            for child in &scope.borrow().children {
                stack.push(child.clone());
            }
        }
        None
    }

    // #[must_use]
    // pub fn lookdown_symbol(&self, id: u32, name: &str) -> Option<NodeType> {
    //     let mut stack = vec![self.scope.clone()];
    //     while let Some(scope) = stack.pop() {
    //         if scope.borrow().id == id {
    //             if let Some(ty) = scope.borrow().lookdown_symbol(name) {
    //                 return Some(ty);
    //             }
    //         }
    //         for child in &scope.borrow().children {
    //             stack.push(child.clone());
    //         }
    //     }
    //     None
    // }

    #[must_use]
    pub fn infer_expr_type(&self, scope_id: u32, expr: &Expression) -> NodeType {
        if let Some(scope) = self.scopes.get(&scope_id) {
            return infer_expr_type(expr, scope, self);
        }
        let mut stack: Vec<ScopeRef> = self.scopes.values().cloned().collect();
        while let Some(scope) = stack.pop() {
            if scope.borrow().id == scope_id {
                return infer_expr_type(expr, &scope, self);
            }
            for child in &scope.borrow().children {
                stack.push(child.clone());
            }
        }
        NodeType::Empty
    }

    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn find_self_type_for_method(&self, function: Rc<Function>) -> Option<String> {
        fn find_in_scope(scope: &ScopeRef, function_name: &str) -> Option<String> {
            let scope_b = scope.borrow();
            for (type_name, methods) in &scope_b.methods {
                if methods.iter().any(|m| m.name == function_name) {
                    return Some(type_name.clone());
                }
            }
            for child in &scope_b.children {
                if let Some(name) = find_in_scope(child, function_name) {
                    return Some(name);
                }
            }
            None
        }
        for scope in self.mod_scopes.values() {
            if let Some(name) = find_in_scope(scope, &function.name) {
                return Some(name);
            }
        }
        None
    }

    // pub(crate) fn find_definition_scope(&self, name: &str) -> Option<ScopeRef> {
    //     for (_, scope) in &self.mod_scopes {
    //         if scope.borrow().qualify_definition_name(name).is_some() {
    //             return Some(scope.clone());
    //         }
    //     }
    //     None
    // }

    pub(crate) fn get_function_by_name(&self, scope_id: u32, name: &str) -> Option<Definition> {
        if let Some(scope) = &self.scopes.get(&scope_id) {
            if let Some(def_ref) = scope.borrow().lookup_def(name) {
                match def_ref {
                    DefinitionRef::Ref(_, def) => return Some(def),
                    DefinitionRef::QualifiedName(q_name) => {
                        for d in &self.defs {
                            if d.0 .1 == q_name {
                                return Some(d.1.clone());
                            }
                        }
                    }
                }
            }
        }
        None
    }

    #[allow(unused_variables, clippy::too_many_lines)]
    pub(crate) fn lookup_symbol_origin(
        &self,
        scope_id: u32,
        symbol: &str,
    ) -> Option<crate::node_type::NodeKind> {
        fn find_stmt(block: &Block, id: u32) -> Option<Statement> {
            for stmt in &block.statements {
                if stmt.id() == id {
                    return Some(stmt.clone());
                }
                match stmt {
                    Statement::Block(inner) => {
                        if let Some(s) = find_stmt(inner, id) {
                            return Some(s);
                        }
                    }
                    Statement::Expression(expr) => match expr {
                        Expression::If(if_expr) => {
                            if let Some(s) = find_stmt(&if_expr.then_branch, id) {
                                return Some(s);
                            }
                            if let Some(Expression::EBlock(inner)) = &if_expr.else_branch {
                                if let Some(s) = find_stmt(&inner.block, id) {
                                    return Some(s);
                                }
                            }
                        }
                        Expression::ForLoop(for_loop) => {
                            if let Some(s) = find_stmt(&for_loop.block, id) {
                                return Some(s);
                            }
                        }
                        Expression::Loop(loop_stmt) => {
                            if let Some(s) = find_stmt(&loop_stmt.block, id) {
                                return Some(s);
                            }
                        }
                        Expression::TryBlock(try_block) => {
                            if let Some(s) = find_stmt(&try_block.block, id) {
                                return Some(s);
                            }
                        }
                        Expression::EBlock(inner) => {
                            if let Some(s) = find_stmt(&inner.block, id) {
                                return Some(s);
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            None
        }
        fn find_call_sites(
            block: &Block,
            name: &str,
            param_idx: usize,
            caller_id: u32,
            calls: &mut Vec<(u32, Expression)>,
        ) {
            for stmt in &block.statements {
                if let Statement::Expression(Expression::FunctionCall(fc)) = stmt {
                    if fc.function_name == name {
                        if let Some(arg) = fc.parameters.get(param_idx) {
                            calls.push((caller_id, arg.clone()));
                        }
                    }
                }
                if let Statement::Block(inner) = stmt {
                    find_call_sites(inner, name, param_idx, caller_id, calls);
                }
                if let Statement::Expression(Expression::If(if_expr)) = stmt {
                    find_call_sites(&if_expr.then_branch, name, param_idx, caller_id, calls);
                    if let Some(Expression::EBlock(inner)) = &if_expr.else_branch {
                        find_call_sites(&inner.block, name, param_idx, caller_id, calls);
                    }
                }
                if let Statement::Expression(Expression::ForLoop(for_loop)) = stmt {
                    find_call_sites(&for_loop.block, name, param_idx, caller_id, calls);
                }
                if let Statement::Expression(Expression::Loop(loop_stmt)) = stmt {
                    find_call_sites(&loop_stmt.block, name, param_idx, caller_id, calls);
                }
            }
        }
        if let Some(scope_rc) = self.scopes.get(&scope_id) {
            let scope = scope_rc.borrow();
            if let Some((var_id, _)) = scope.variables.get(symbol) {
                if let Some(parent_rc) = &scope.parent {
                    let parent = parent_rc.borrow();
                    if let Some(func_name) = scope.name.rsplit("::").next() {
                        if let Some(DefinitionRef::Ref(_, Definition::Function(func_def))) =
                            parent.lookup_def(func_name)
                        {
                            if let Some((idx, param)) = func_def
                                .parameters
                                .iter()
                                .enumerate()
                                .find(|(_, p)| p.id == *var_id)
                            {
                                let mut calls = Vec::new();
                                for def in self.defs.values() {
                                    if let Definition::Function(caller_def) = def {
                                        if caller_def.id != func_def.id {
                                            if let Some(body) = &caller_def.body {
                                                find_call_sites(
                                                    body,
                                                    func_name,
                                                    idx,
                                                    caller_def.id,
                                                    &mut calls,
                                                );
                                            }
                                        }
                                    }
                                }
                                if calls.len() == 1 {
                                    let (caller_id, arg_expr) = &calls[0];
                                    if let Expression::Identifier(id) = arg_expr {
                                        return self.lookup_symbol_origin(*caller_id, &id.name);
                                    }
                                }
                                return Some(crate::node_type::NodeKind::Misc(Misc::FnParameter(
                                    param.clone(),
                                )));
                            }
                            if let Some(body) = &func_def.body {
                                if let Some(stmt) = find_stmt(body, *var_id) {
                                    return Some(crate::node_type::NodeKind::Statement(stmt));
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut current = self.scopes.get(&scope_id).cloned();
        while let Some(scope_rc) = current {
            let scope = scope_rc.borrow();
            if let Some(def_ref) = scope.lookup_def(symbol) {
                match def_ref {
                    DefinitionRef::Ref(_, def) => {
                        return Some(crate::node_type::NodeKind::Definition(def.clone()));
                    }
                    DefinitionRef::QualifiedName(qn) => {
                        for ((_, name), def) in &self.defs {
                            if *name == qn {
                                return Some(crate::node_type::NodeKind::Definition(def.clone()));
                            }
                        }
                    }
                }
            }
            current = scope.parent.as_ref().map(Rc::clone);
        }
        None
    }
}

pub(crate) fn process_definition(
    parent_scope: &ScopeRef,
    def: Definition,
    table: &mut SymbolTable,
) {
    let parent_path = parent_scope.borrow().name.clone();
    let name = def.name();
    let qualified = format!("{parent_path}::{name}");
    parent_scope
        .borrow_mut()
        .definitions
        .insert(name.clone(), def.clone());
    // let crate_id = parent_scope.borrow().crate_root_id();
    table.insert_def(parent_scope.borrow().id, qualified.clone(), def.clone());
    match def {
        Definition::Struct(s) => {
            parent_scope
                .borrow_mut()
                .methods
                .insert(s.name.clone(), Vec::new());
        }
        Definition::Function(f) => {
            let fn_scope = Scope::new(f.id, qualified, Some(parent_scope.clone()));
            // parent_scope.borrow_mut().children.push(fn_scope.clone());
            table.insert_scope(fn_scope.clone());
            // fn_scope
            //     .borrow_mut()
            //     .functions
            //     .insert(f.name.clone(), f.clone());
            // fn_scope //T-ODO: revisit this
            //     .borrow_mut()
            //     .insert_def(name.clone(), Definition::Function(f.clone()));
        }
        Definition::Implementation(i) => {
            // let type_node = i.for_type.to_type_node();
            for constant in &i.constants {
                let constant_name = constant.name.clone();
                let constant_def = Definition::Const(constant.clone());
                parent_scope
                    .borrow_mut()
                    .definitions
                    .insert(constant_name, constant_def);
            }
            for ta in &i.type_aliases {
                let alias_name = ta.name.clone();
                let alias_def = Definition::TypeAlias(ta.clone());
                parent_scope
                    .borrow_mut()
                    .definitions
                    .insert(alias_name, alias_def);
            }
            if let Type::Typename(target) = &i.for_type {
                let def_name_op = parent_scope.borrow().qualify_definition_name(&target.name);
                if let Some(def_name) = def_name_op {
                    // let target_def = parent_scope
                    //     .borrow()
                    //     .lookup_def(&target.name)
                    //     .map(|dr| {
                    //         if let DefinitionRef::Ref(_, def) = dr {
                    //             def
                    //         } else {
                    //             panic!(
                    //                 "Expected a reference to a definition, found qualified name"
                    //             );
                    //         }
                    //     })
                    //     .unwrap();
                    if let Some(method_list) = parent_scope.borrow_mut().methods.get_mut(&def_name)
                    {
                        for func in &i.functions {
                            method_list.push(func.clone());
                            // let qualified = format!("{def_name}::{}", func.name.clone()); //F-IXME this is actually is a copy from above, need to refactor
                            // let fn_scope =
                            //     Scope::new(func.id, qualified, Some(parent_scope.clone()));
                            // table.insert_scope(fn_scope.clone());
                            // fn_scope
                            //     .borrow_mut()
                            //     .functions
                            //     .insert(func.name.clone(), func.clone());
                            // fn_scope //T-ODO: revisit this
                            //     .borrow_mut()
                            //     .insert_def(name.clone(), Definition::Function(func.clone()));
                        }
                    }
                    for func in &i.functions {
                        process_definition(
                            &parent_scope.clone(),
                            Definition::Function(func.clone()),
                            table,
                        );
                    }
                }
            }
        }
        _ => {}
    }
}

pub fn fixpoint_resolver(table: &mut SymbolTable, extern_prelude: &mut ExternPrelude) {
    loop {
        let mut progress = false;
        for scope in table.scopes.values() {
            for rc_use in &scope.borrow().imports {
                if rc_use.is_resolved() {
                    continue;
                }
                let imported_types = rc_use.imported_types.clone();
                for import_path in imported_types {
                    let (orig_path, _) = import_path
                        .split_once('%')
                        .map(|(orig, alias)| (orig.to_string(), Some(alias.to_string())))
                        .unwrap_or((import_path.clone(), None));
                    let (head, tail) = match orig_path.split_once("::") {
                        Some((h, t)) => (h.to_string(), t.to_string()),
                        None => (orig_path.clone(), String::new()),
                    };
                    let start_scope = match head.as_str() {
                        "crate" => scope.borrow().crate_root(),
                        "self" => Some(scope.clone()),
                        "super" => scope.borrow().parent.clone(),
                        other => {
                            if let Some(ext) = extern_prelude.get(other) {
                                Some(ext.root_scope.clone())
                            } else {
                                scope
                                    .borrow()
                                    .visible_child(other)
                                    .and_then(|d| d.as_module())
                                    .or_else(|| {
                                        scope.borrow().crate_root().and_then(|root| {
                                            root.borrow()
                                                .visible_child(other)
                                                .and_then(|d| d.as_module())
                                        })
                                    })
                            }
                        }
                    };
                    if let Some(start) = start_scope {
                        if let Some(def) = walk_segments(start, &tail, scope) {
                            // println!(
                            //     "Resolved import: {} in scope {}",
                            //     import_path,
                            //     scope.borrow().name
                            // );
                            if rc_use.target.borrow().contains_key(&import_path)
                                && rc_use.target.borrow().get(&import_path).unwrap().is_some()
                            {
                                continue;
                            }
                            rc_use.insert_target(import_path, Some(def));
                            progress = true;
                        }
                    }
                }
            }
        }
        if !progress {
            break;
        } // loop until no more imports can be resolved
    }
    table.build_symbol_tables();
}

#[allow(clippy::assigning_clones)]
fn scope_path(scope: &ScopeRef) -> Vec<String> {
    let mut segments = Vec::new();
    let mut cur = Some(scope.clone());
    while let Some(sc) = cur {
        segments.push(sc.borrow().name.clone());
        cur = sc.borrow().parent.clone();
    }
    segments.reverse();
    segments
}

impl Visibility {
    pub(crate) fn is_visible_from(&self, from_scope: &ScopeRef, owner_scope: &ScopeRef) -> bool {
        if *self == Visibility::Public {
            return true;
        }

        let same_crate =
            from_scope.borrow().crate_root_id() == owner_scope.borrow().crate_root_id();

        match self {
            Visibility::Private | Visibility::Inherited => {
                let owner_path = scope_path(owner_scope);
                let from_path = scope_path(from_scope);
                from_path.starts_with(&owner_path)
            }
            Visibility::PubCrate => same_crate,
            Visibility::PubSuper => {
                if let Some(parent) = owner_scope.borrow().parent.clone() {
                    let super_path = scope_path(&parent);
                    let from_path = scope_path(from_scope);
                    from_path.starts_with(&super_path)
                } else {
                    // owner is crate root → `pub(super)` degenerates to private
                    false
                }
            }
            Visibility::PubIn(path) => {
                let target_mod = path
                    .split("::")
                    .filter(|s| !s.is_empty())
                    .map(str::to_owned)
                    .collect::<Vec<_>>();
                let from_path = scope_path(from_scope);
                from_path.starts_with(&target_mod)
            }

            Visibility::Public => unreachable!("handled above {:?}", self),
        }
    }
}

fn walk_segments(mut scope: ScopeRef, path: &str, from_crate: &ScopeRef) -> Option<Definition> {
    if path.is_empty() {
        return None;
    }
    let mut segments = path.split("::").filter(|s| !s.is_empty()).peekable();
    while let Some(seg) = segments.next() {
        let is_last = segments.peek().is_none();
        let child = scope.borrow().visible_child(seg)?;
        match child {
            DefOrScopeRef::Module(sub_scope) => {
                if is_last {
                    return None;
                }
                scope = sub_scope;
            }
            DefOrScopeRef::Definition(def) => {
                // Found a definition
                if !def.visibility().is_visible_from(from_crate, &scope) {
                    return None;
                }
                if is_last {
                    return Some(def);
                }
                return None;
            }
        }
    }
    None
}

#[allow(clippy::too_many_lines)]
fn process_statement(stmt: &Statement, scope: &ScopeRef, table: &mut SymbolTable) {
    match stmt {
        Statement::Let(let_stmt) => {
            let mut vty = if let Some(init) = &let_stmt.initial_value {
                let ty = infer_expr_type(init, scope, table);
                if ty != NodeType::Empty {
                    ty
                } else if let Some((_, ty_str)) = let_stmt.pattern.kind.split_once(':') {
                    parse_str::<syn::Type>(ty_str.trim())
                        .map(|ty| NodeType::from_syn_item(&ty))
                        .unwrap_or(NodeType::Empty)
                } else {
                    NodeType::Empty
                }
            } else {
                NodeType::Empty
            };
            if let Some(def) = scope.borrow().lookup_def(&vty.name()) {
                match def {
                    DefinitionRef::Ref(_, _) => {}
                    DefinitionRef::QualifiedName(qualified_name) => {
                        vty = NodeType::Path(qualified_name);
                    }
                }
            }
            scope
                .borrow_mut()
                .insert_var(let_stmt.name.clone(), let_stmt.id, vty);
        }
        Statement::Expression(Expression::If(if_expr)) => {
            for stmt in &if_expr.then_branch.statements {
                process_statement(stmt, scope, table);
            }
        }
        Statement::Block(block) => {
            for stmt in &block.statements {
                process_statement(stmt, scope, table);
            }
        }
        Statement::Expression(Expression::EBlock(e_block)) => {
            for stmt in &e_block.block.statements {
                process_statement(stmt, scope, table);
            }
            // if let Expression::Identifier(id) = expr {
            //     if let Some(v) = table.lookdown_symbol(scope.borrow().id, &id.name) {
            //         scope.borrow_mut().insert_var(id.name.clone(), v);
            //     }
            // }
        }
        // Statement::Definition(def) => process_definition(def.clone(), scope, table, codebase),
        _ => {}
    }
}

#[allow(clippy::too_many_lines)]
fn infer_expr_type(expr: &Expression, scope: &ScopeRef, table: &SymbolTable) -> NodeType {
    match expr {
        Expression::Identifier(id) => {
            // Qualified path resolution: module::Name
            if id.name.contains("::") {
                let (module, rest) = id.name.split_once("::").unwrap();
                if let Some(mod_scope) = table.mod_scopes.get(module) {
                    if let Some(def) = mod_scope.borrow().lookup_def(rest) {
                        let ty = match def {
                            DefinitionRef::Ref(_, Definition::Const(c)) => c.type_.to_type_node(),
                            DefinitionRef::Ref(_, Definition::Static(s)) => s.ty.to_type_node(),
                            DefinitionRef::Ref(_, Definition::Function(f)) => {
                                f.returns.borrow().clone()
                            }
                            DefinitionRef::Ref(_, Definition::AssocType(t)) => t.ty.to_type_node(),
                            DefinitionRef::Ref(
                                _,
                                Definition::Struct(s) | Definition::Contract(s),
                            ) => NodeType::Path(s.name.clone()),
                            DefinitionRef::Ref(_, Definition::Enum(e)) => {
                                NodeType::Path(e.name.clone())
                            }
                            DefinitionRef::Ref(_, Definition::Union(u)) => {
                                NodeType::Path(u.name.clone())
                            }
                            DefinitionRef::Ref(_, Definition::Module(m)) => {
                                NodeType::Path(m.name.clone())
                            }
                            DefinitionRef::Ref(_, Definition::TraitAlias(ta)) => {
                                NodeType::Path(ta.name.clone())
                            }
                            _ => return NodeType::Empty,
                        };
                        return ty;
                    }
                }
            }
            if let Some(v) = scope.borrow().lookup_symbol(&id.name) {
                return v;
            }
            if let Some(def) = scope.borrow().lookup_def(&id.name) {
                let ty_node = match def {
                    DefinitionRef::Ref(_, Definition::Const(c)) => c.type_.to_type_node(),
                    DefinitionRef::Ref(_, Definition::Static(s)) => s.ty.to_type_node(),
                    DefinitionRef::Ref(_, Definition::Function(f)) => f.returns.borrow().clone(),
                    DefinitionRef::Ref(_, Definition::AssocType(t)) => t.ty.to_type_node(),
                    DefinitionRef::Ref(_, Definition::Struct(s) | Definition::Contract(s)) => {
                        NodeType::Path(s.name.clone())
                    }
                    DefinitionRef::Ref(_, Definition::Enum(e)) => NodeType::Path(e.name.clone()),
                    DefinitionRef::Ref(_, Definition::Union(u)) => NodeType::Path(u.name.clone()),
                    DefinitionRef::Ref(_, Definition::Module(m)) => NodeType::Path(m.name.clone()),
                    DefinitionRef::Ref(_, Definition::TraitAlias(ta)) => {
                        NodeType::Path(ta.name.clone())
                    }
                    _ => return NodeType::Empty,
                };
                return ty_node;
            }
            NodeType::Empty
        }
        Expression::Literal(lit_expr) => match &lit_expr.value {
            Literal::Bool(_) => NodeType::Path("bool".to_string()),
            Literal::Byte(_) => NodeType::Path("u8".to_string()),
            Literal::Char(_) => NodeType::Path("char".to_string()),
            Literal::Int(_) => NodeType::Path("i32".to_string()),
            Literal::Float(_) => NodeType::Path("f64".to_string()),
            Literal::String(_) => NodeType::Reference {
                inner: Box::new(NodeType::Path("str".to_string())),
                mutable: false,
                is_explicit_reference: true,
            },
            Literal::BString(_) => NodeType::Reference {
                inner: Box::new(NodeType::Path("[u8]".to_string())),
                mutable: false,
                is_explicit_reference: true,
            },
            Literal::CString(cs) => NodeType::Ptr {
                inner: Box::new(NodeType::Path("c_char".to_string())),
                mutable: cs.value.starts_with("*const"),
            },
        },
        Expression::Binary(bin) => {
            match bin.operator {
                BinOp::Add
                | BinOp::Sub
                | BinOp::Mul
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BitXor
                | BinOp::BitAnd
                | BinOp::BitOr
                | BinOp::Shl
                | BinOp::Shr => {
                    let left_ty = infer_expr_type(&bin.left, scope, table);
                    let right_ty = infer_expr_type(&bin.right, scope, table);
                    if left_ty == right_ty {
                        return left_ty;
                    }
                    right_ty //Won't fix, fallback
                }
                BinOp::And
                | BinOp::Or
                | BinOp::Eq
                | BinOp::Ne
                | BinOp::Lt
                | BinOp::Le
                | BinOp::Ge
                | BinOp::Gt => NodeType::Path("bool".to_string()),
                BinOp::AddAssign
                | BinOp::SubAssign
                | BinOp::MulAssign
                | BinOp::DivAssign
                | BinOp::ModAssign
                | BinOp::BitXorAssign
                | BinOp::BitAndAssign
                | BinOp::BitOrAssign
                | BinOp::ShlAssign
                | BinOp::ShrAssign => infer_expr_type(&bin.left, scope, table),
            }
        }
        Expression::Unary(u) => infer_expr_type(&u.expression, scope, table),
        Expression::FunctionCall(fc) => {
            for def in scope.borrow().functions().chain(scope.borrow().methods()) {
                if def.name == fc.function_name {
                    return def.returns.borrow().clone();
                }
            }
            if let Expression::Identifier(base) = &fc.expression {
                let name = base.name.clone();
                if name == "Some" {
                    if let Some(arg) = &fc.parameters.first() {
                        let ty = infer_expr_type(arg, scope, table);
                        return NodeType::Path(format!("Option<{}>", ty.name()));
                    }
                    return NodeType::Path("Option<_>".to_string());
                }
                if name == "None" {
                    return NodeType::Path("Option<_>".to_string());
                }
                if name == "Ok" {
                    if let Some(arg) = &fc.parameters.first() {
                        let ty = infer_expr_type(arg, scope, table);
                        return NodeType::Path(format!("Result<{}, _>", ty.name()));
                    }
                    return NodeType::Path("Result<_, _>".to_string());
                }
                if name.contains("::") {
                    let (module, rest) = name.split_once("::").unwrap();
                    let module = module.trim();
                    let rest = rest.trim();
                    if let Some(mod_scope) = table.mod_scopes.get(module) {
                        for def in mod_scope
                            .borrow()
                            .functions()
                            .chain(mod_scope.borrow().methods())
                        {
                            if def.name == rest {
                                return def.returns.borrow().clone();
                            }
                        }
                    }
                    if ["Vec", "HashMap"].contains(&module) {
                        return NodeType::Reference {
                            inner: Box::new(NodeType::Path(module.to_string())),
                            mutable: false,
                            is_explicit_reference: false,
                        };
                    }
                } else if let Some(v) = scope.borrow().lookup_symbol(&name) {
                    return v;
                }
            }
            NodeType::Empty
        }
        Expression::MethodCall(mc) => {
            let base_ty = infer_expr_type(&mc.base, scope, table);
            if base_ty == NodeType::Empty {
                return NodeType::Empty;
            }
            let type_name_owned = base_ty.pure_name().clone();
            let type_name = type_name_owned.as_str();
            // if let NodeType::Path(type_name) = &base_ty {
            let base_def = if let Some(base_def_ref) = scope.borrow().lookup_def(type_name) {
                match base_def_ref {
                    DefinitionRef::Ref(_, def) => Some(def),
                    DefinitionRef::QualifiedName(_) => None,
                }
            } else if let Some(d) = table.defs.iter().find(|((_, n), _)| n == type_name) {
                Some(d.1.clone())
            } else {
                return NodeType::Empty;
            };
            if let Some(def) = base_def {
                if let Some(def_scope) = table.get_scope_by_def_id(def.id()) {
                    if let Some(method) =
                        def_scope
                            .borrow()
                            .methods
                            .get(&def.name())
                            .and_then(|methods| {
                                methods.iter().find(|f| f.name == mc.method_name).cloned()
                            })
                    {
                        let ty_node_name = method.returns.borrow().pure_name();
                        if let Some(def) = def_scope.borrow().lookup_def(&ty_node_name) {
                            match def {
                                DefinitionRef::Ref(qualified_name, _) => {
                                    *method.returns.borrow_mut() = NodeType::Path(
                                        def_scope
                                            .borrow()
                                            .relative_to_absolute_path(&qualified_name),
                                    );
                                }
                                DefinitionRef::QualifiedName(qualified_name) => {
                                    *method.returns.borrow_mut() = NodeType::Path(qualified_name);
                                }
                            }
                        }
                        return method.returns.borrow().clone();
                    }
                }
            }
            // if let Some(base_def_ref) = scope.borrow().lookup_def(type_name) {
            //     match base_def_ref {
            //         DefinitionRef::Ref(def_name, def) => {
            //             for (k, v) in &table.defs {
            //                 if def.id() == v.id() {
            //                     if let Some(def_scope) = table.scopes.get(&k.0).cloned() {
            //                         let methods =
            //                             table.lookup_methods(def_scope.clone(), def.name());
            //                         if let Some(method) =
            //                             def_scope.borrow().methods.get(&def.name()).and_then(
            //                                 |methods| {
            //                                     methods
            //                                         .iter()
            //                                         .find(|f| f.name == mc.method_name)
            //                                         .cloned()
            //                                 },
            //                             )
            //                         {
            //                             let mut ty_node = method.returns.to_type_node();
            //                             if let Some(def) =
            //                                 def_scope.borrow().lookup_def(&ty_node.name())
            //                             {
            //                                 match def {
            //                                     DefinitionRef::Ref(qualified_name, _) => {
            //                                         ty_node = NodeType::Path(
            //                                             def_scope
            //                                                 .borrow()
            //                                                 .relative_to_absolute_path(
            //                                                     qualified_name,
            //                                                 ),
            //                                         );
            //                                     }
            //                                     DefinitionRef::QualifiedName(
            //                                         qualified_name,
            //                                     ) => {
            //                                         ty_node = NodeType::Path(qualified_name);
            //                                     }
            //                                 }
            //                             }
            //                             return Some(ty_node);
            //                         }
            //                     }
            //                 }
            //             }
            //         }
            //         DefinitionRef::QualifiedName(_) => {}
            //     }
            // }
            // else {
            //     if let Some(d) = table.defs.iter().find(|((_, n), _)| n == type_name) {
            //         println!("resolved in global defs: {type_name}");
            //     }
            // }
            // if let Some((method_scope, method)) = table
            //     .resolve_type_methods(scope.borrow().id, type_name)
            //     .and_then(|(scope, methods)| {
            //         methods
            //             .iter()
            //             .find(|f| *f.name == mc.method_name)
            //             .cloned()
            //             .map(|methods| (scope, methods))
            //     })
            // {
            //     let mut ty_node = method.returns.to_type_node();
            //     //T-ODO: can be self
            //     if let Some(def) = method_scope.borrow().lookup_def(&ty_node.name()) {
            //         match def {
            //             DefinitionRef::Ref(qualified_name, _) => {
            //                 ty_node = NodeType::Path(qualified_name);
            //             }
            //             DefinitionRef::QualifiedName(qualified_name) => {
            //                 ty_node = NodeType::Path(qualified_name);
            //             }
            //         }
            //     }
            //     return Some(ty_node);
            // }
            // }
            NodeType::Empty
        }
        Expression::MemberAccess(ma) => {
            let base_ty = infer_expr_type(&ma.base, scope, table);
            if let Some(def_ref) = scope.borrow().lookup_def(&base_ty.pure_name()) {
                match def_ref {
                    DefinitionRef::Ref(_, Definition::Struct(s) | Definition::Contract(s)) => {
                        for (field, fty) in &s.fields {
                            if &ma.member_name == field {
                                return fty.to_type_node();
                            }
                        }
                    }
                    DefinitionRef::Ref(_, Definition::Enum(e)) => {
                        for variant in &e.variants {
                            if &ma.member_name == variant {
                                return NodeType::Path(base_ty.name().clone());
                            }
                        }
                    }
                    DefinitionRef::QualifiedName(qn) => {
                        // If the type is a qualified name, we can try to resolve it
                        if let Some(def) = scope.borrow().lookup_def(&qn) {
                            if let DefinitionRef::Ref(
                                _,
                                Definition::Struct(s) | Definition::Contract(s),
                            ) = def
                            {
                                for (field, fty) in &s.fields {
                                    if &ma.member_name == field {
                                        return fty.to_type_node();
                                    }
                                }
                            }
                        } else {
                            return NodeType::Path(qn.clone());
                        }
                    }
                    DefinitionRef::Ref(_, _) => {}
                }
            }
            // if let Some(Definition::Struct(s)) = scope.borrow().lookup_def(&type_name) {
            //     for (field, fty) in &s.fields {
            //         if &ma.member_name == field {
            //             // field type from AST
            //             return Some(fty.to_type_node());
            //         }
            //     }
            // }

            NodeType::Empty
        }
        Expression::Return(r) => {
            if let Some(e) = &r.expression {
                infer_expr_type(e, scope, table)
            } else {
                NodeType::Tuple(Vec::new())
            }
        }
        Expression::Cast(c) => {
            let mut ty_node = c.target_type.to_type_node();
            if ty_node.is_self() {
                if let Some(self_ty) = scope.borrow().resolve_self() {
                    ty_node = match ty_node {
                        NodeType::Path(ref name) if name == "Self" => {
                            NodeType::Path(self_ty.name())
                        }
                        NodeType::Reference {
                            inner: old_inner,
                            mutable,
                            is_explicit_reference,
                        } if old_inner.name() == "Self" => NodeType::Reference {
                            inner: Box::new(NodeType::Path(self_ty.name())),
                            mutable,
                            is_explicit_reference,
                        },
                        NodeType::Ptr { inner, mutable } if inner.name() == "Self" => {
                            NodeType::Ptr {
                                inner: Box::new(NodeType::Path(self_ty.name())),
                                mutable,
                            }
                        }
                        other => other,
                    };
                }
            }
            ty_node
        }
        Expression::Closure(cl) => {
            let ty_node = cl.returns.to_type_node();
            if ty_node.name().is_empty() {
                let ty_node = infer_expr_type(&cl.body, scope, table);
                if ty_node != NodeType::Empty {
                    return ty_node;
                }
            }
            NodeType::Closure {
                inputs: cl
                    .captures
                    .iter()
                    .map(|c| {
                        let ty = infer_expr_type(&Expression::Identifier(c.clone()), scope, table);
                        if ty == NodeType::Empty {
                            NodeType::Empty
                        } else {
                            if ty.is_self() {
                                if let Some(self_ty) = scope.borrow().resolve_self() {
                                    return NodeType::Path(self_ty.name());

                                    // if let Some(self_ty) =
                                    //     table.find_self_type_for_method(f.clone())
                                    // {
                                    // }
                                }
                            }
                            ty
                        }
                    })
                    .collect(),
                output: Box::new(ty_node),
            }
        }
        Expression::Macro(m) => {
            if m.name == "symbol_short" {
                return NodeType::Path("Symbol".to_string());
            }
            if m.name == "format" {
                // format! macro returns String
                return NodeType::Path("String".to_string());
            }
            if m.name == "vec" {
                let re =
                    regex::Regex::new(r"^\s*(?P<elems>.+?)(?:\s*;\s*(?P<count>.+))?\s*$").unwrap();
                if let Some(caps) = re.captures(&m.text) {
                    if caps.name("count").is_some() {
                        let expr_str = caps.name("elems").unwrap().as_str();
                        if let Ok(expr) = syn::parse_str::<syn::Type>(expr_str) {
                            let ty = NodeType::from_syn_item(&expr);
                            return NodeType::Reference {
                                inner: Box::new(ty),
                                mutable: false,
                                is_explicit_reference: false,
                            };
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
                                let ty = NodeType::from_syn_item(&expr);
                                return NodeType::Reference {
                                    inner: Box::new(ty),
                                    mutable: false,
                                    is_explicit_reference: false,
                                };
                            }
                            if let Ok(syn::Lit::Int(_)) = syn::parse_str::<syn::Lit>(first) {
                                return NodeType::Reference {
                                    inner: Box::new(NodeType::Path("Vec<i32>".to_string())),
                                    mutable: false,
                                    is_explicit_reference: false,
                                };
                            }
                        }
                    }
                }
                // Fallback if parsing fails
                return NodeType::Path("Vec<_>".to_string());
            }
            NodeType::Empty
        }
        Expression::Tuple(t) => {
            let mut types = Vec::new();
            for e in &t.elements {
                types.push(infer_expr_type(e, scope, table));
            }
            NodeType::Tuple(types)
        }
        _ => NodeType::Empty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{directive::Directive, node_type::NodeKind, Codebase, OpenState};

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub(crate) fn resolve_path(
        table: &SymbolTable,
        scope_id: u32,
        path: &str,
    ) -> Option<DefinitionRef> {
        let mut parts = path
            .split("::")
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            return None;
        }
        let name = parts.pop().unwrap();
        if parts.is_empty() {
            if let Some(scope) = table
                .mod_scopes
                .iter()
                .find(|(_, v)| v.borrow().id == scope_id)
                .map(|(_, v)| v)
            {
                return scope.borrow().lookup_def(name);
            }
        }
        let module_path = parts.join("::");
        for (mod_scope_name, mod_scope) in &table.mod_scopes {
            if *mod_scope_name == module_path {
                for (def_name, def) in &mod_scope.borrow().definitions {
                    if def_name == name {
                        return Some(DefinitionRef::Ref(def_name.clone(), def.clone()));
                    }
                }
            }
        }
        None
    }

    fn build_table_and_uses(src: &str) -> (SymbolTable, Vec<Rc<Use>>) {
        let mut cb = Codebase::<OpenState>::default();
        let content = src.to_string();
        let mut data = HashMap::new();
        data.insert("test/test.rs".to_string(), content);
        let sealed = cb.build_api(&data).unwrap();
        let table = sealed.symbol_table.clone();
        let file = sealed.files().next().unwrap().clone();
        let uses = file
            .children
            .borrow()
            .iter()
            .filter_map(|node| {
                if let NodeKind::Directive(Directive::Use(u)) = node {
                    Some(u.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        (table, uses)
    }

    #[test]
    fn test_resolve_path_basic() {
        let src = r"
            mod a {
                pub struct S;
                pub mod b {
                    pub enum E { A }
                }
            }
        ";
        let (table, _) = build_table_and_uses(src);
        let scope_id = table.mod_scopes.iter().next().unwrap().1.borrow().id;
        assert!(resolve_path(
            &table,
            scope_id,
            "soroban_security_detectors_sdk::test::a::S"
        )
        .is_some());
        assert!(resolve_path(
            &table,
            scope_id,
            "soroban_security_detectors_sdk::test::a::b::E"
        )
        .is_some());
        assert!(resolve_path(
            &table,
            scope_id,
            "soroban_security_detectors_sdk::test::a::X"
        )
        .is_none());
        assert!(resolve_path(&table, scope_id, "").is_none());
    }

    #[test]
    fn test_import_target_resolution() {
        let mut cb = Codebase::<OpenState>::default();
        let file1 = r"
        pub type MyType = u8;
        pub mod sub {
            pub struct SubType;
        }
        "
        .to_string();

        let file2 = r"
        use file1::MyType;
        use file1::sub::SubType as Renamed;
        "
        .to_string();
        let mut data = HashMap::new();
        data.insert("test/file1.rs".to_string(), file1);
        data.insert("test/file2.rs".to_string(), file2);
        let sealed = cb.build_api(&data).unwrap();
        let table = sealed.symbol_table.clone();

        let file2 = sealed.files().find(|f| f.name == "file2.rs").unwrap();
        let uses = file2
            .children
            .borrow()
            .iter()
            .filter_map(|node| {
                if let NodeKind::Directive(Directive::Use(u)) = node {
                    Some(u.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(uses.len(), 2);

        let full1 = "file1::MyType".to_string();
        let u1 = uses
            .iter()
            .find(|u| u.imported_types == vec![full1.clone()])
            .unwrap();
        let DefinitionRef::Ref(_, my_def) = resolve_path(
            &table,
            table
                .mod_scopes
                .iter()
                .find(|s| !s.1.borrow().definitions.is_empty())
                .unwrap()
                .1
                .borrow()
                .id,
            "soroban_security_detectors_sdk::file1::MyType",
        )
        .unwrap() else {
            panic!("Expected a reference to a definition");
        };
        let binding = u1.target.borrow();
        println!("Use target: {binding:?}, checking key {full1:?}");
        let found = binding.get(&full1);
        assert_eq!(found.unwrap().as_ref().unwrap().id(), my_def.id());

        let full2 = "file1::sub::SubType%Renamed".to_string();
        let u2 = uses
            .iter()
            .find(|u| u.imported_types == vec![full2.clone()])
            .unwrap();
        let DefinitionRef::Ref(_, sub) = resolve_path(
            &table,
            table.mod_scopes.iter().next().unwrap().1.borrow().id,
            "soroban_security_detectors_sdk::file1::sub::SubType",
        )
        .unwrap() else {
            panic!("Expected a reference to a definition");
        };
        assert_eq!(u2.target.borrow().get(&full2), Some(&Some(sub)));
    }

    #[test]
    fn test_lookup_symbol_origin_basic() {
        let src = r"
        pub const C: u32 = 5;

        fn f() {
            let x = C;
            let y = x;
        }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let origin_c_root = table.lookup_symbol_origin(root_scope, "C").unwrap();
        if let crate::node_type::NodeKind::Definition(ref def) = origin_c_root {
            assert_eq!(def.name(), "C");
        } else {
            panic!("Expected Definition for C");
        }
        let f_def = table.get_function_by_name(root_scope, "f").unwrap();
        let f_scope = f_def.id();
        let origin_c = table.lookup_symbol_origin(f_scope, "C").unwrap();
        assert_eq!(origin_c.id(), origin_c_root.id());
        let origin_x = table.lookup_symbol_origin(f_scope, "x").unwrap();
        if let crate::node_type::NodeKind::Statement(stmt) = origin_x.clone() {
            if let Statement::Let(let_stmt) = stmt {
                assert_eq!(let_stmt.name, "x");
            } else {
                panic!("Expected Let statement for x");
            }
        } else {
            panic!("Expected Statement variant for x origin");
        }
        let origin_y = table.lookup_symbol_origin(f_scope, "y").unwrap();
        if let crate::node_type::NodeKind::Statement(stmt) = origin_y.clone() {
            if let Statement::Let(let_stmt) = stmt {
                assert_eq!(let_stmt.name, "y");
            } else {
                panic!("Expected Let statement for y");
            }
        } else {
            panic!("Expected Statement variant for y origin");
        }
        let origin_x_again = table.lookup_symbol_origin(f_scope, "x").unwrap();
        assert_eq!(origin_x_again.id(), origin_x.id());
        assert!(table.lookup_symbol_origin(f_scope, "z").is_none());
    }

    #[test]
    fn test_lookup_symbol_origin_parameter() {
        let src = r"
        fn g(p: u32) {
            let q = p;
        }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let g_def = table.get_function_by_name(root_scope, "g").unwrap();
        let g_scope = g_def.id();
        let origin_p = table.lookup_symbol_origin(g_scope, "p").unwrap();
        if let crate::node_type::NodeKind::Misc(Misc::FnParameter(param)) = origin_p {
            assert_eq!(param.name, "p");
        } else {
            panic!("Expected FnParameter origin for p");
        }
    }

    #[test]
    fn test_lookup_symbol_origin_parameter_single_call_constant() {
        let src = r"
        const C: u32 = 5;

        fn g(p: u32) {
            let q = p;
        }

        fn f() {
            g(C);
        }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let g_scope = table.get_function_by_name(root_scope, "g").unwrap().id();
        let origin_c = table.lookup_symbol_origin(root_scope, "C").unwrap();
        let origin_p = table.lookup_symbol_origin(g_scope, "p").unwrap();
        assert_eq!(origin_p.id(), origin_c.id());
    }

    #[test]
    fn test_lookup_symbol_origin_parameter_single_call_variable() {
        let src = r"
        fn g(p: u32) {
            let q = p;
        }

        fn f() {
            let x = 42;
            g(x);
        }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let g_scope = table.get_function_by_name(root_scope, "g").unwrap().id();
        let f_scope = table.get_function_by_name(root_scope, "f").unwrap().id();
        let origin_x = table.lookup_symbol_origin(f_scope, "x").unwrap();
        let origin_p = table.lookup_symbol_origin(g_scope, "p").unwrap();
        assert_eq!(origin_p.id(), origin_x.id());
    }

    #[test]
    fn test_lookup_symbol_origin_parameter_multiple_calls() {
        let src = r"
        fn g(p: u32) {
            let q = p;
        }

        fn a() { g(1); }
        fn b() { g(2); }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let g_scope = table.get_function_by_name(root_scope, "g").unwrap().id();
        let origin_p = table.lookup_symbol_origin(g_scope, "p").unwrap();
        if let crate::node_type::NodeKind::Misc(Misc::FnParameter(param)) = origin_p {
            assert_eq!(param.name, "p");
        } else {
            panic!("Expected FnParameter for multiple calls");
        }
    }

    #[test]
    fn test_lookup_symbol_origin_parameter_single_call_nested_block() {
        let src = r"
        const C: u32 = 5;

        fn h(p: u32) {
            let q = p;
        }

        fn f() {
            if true {
                let x = C;
                h(x);
            }
        }
        ";
        let (table, _) = build_table_and_uses(src);
        // Use the module scope for the test file (test.rs)
        let root_scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::test")
            .unwrap()
            .borrow()
            .id;
        let h_scope = table.get_function_by_name(root_scope, "h").unwrap().id();
        let f_scope = table.get_function_by_name(root_scope, "f").unwrap().id();
        let origin_x = table.lookup_symbol_origin(f_scope, "x").unwrap();
        let origin_p = table.lookup_symbol_origin(h_scope, "p").unwrap();
        assert_eq!(origin_p.id(), origin_x.id());
    }

    #[test]
    fn test_import_across_file_scopes() {
        let mut cb = Codebase::<OpenState>::default();
        let file1 = r"
            pub struct AStruct;
        "
        .to_string();

        let file2 = r"
            use file1::AStruct;
        "
        .to_string();
        let mut data = HashMap::new();
        data.insert("test/file1.rs".to_string(), file1);
        data.insert("test/file2.rs".to_string(), file2);

        let sealed = cb.build_api(&data).unwrap();
        let table = sealed.symbol_table.clone();

        let file2 = sealed.files().find(|f| f.name == "file2.rs").unwrap();
        let uses = file2
            .children
            .borrow()
            .iter()
            .filter_map(|node| {
                if let NodeKind::Directive(Directive::Use(u)) = node {
                    Some(u.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(uses.len(), 1);

        let u = &uses[0];
        let scope = table
            .mod_scopes
            .get("soroban_security_detectors_sdk::file2")
            .expect("Module scope for file2 not found");
        let DefinitionRef::Ref(_, expected) = resolve_path(
            &table,
            scope.borrow().id,
            "soroban_security_detectors_sdk::file1::AStruct",
        )
        .unwrap() else {
            panic!("Expected a reference to a definition");
        };
        let key = &u.imported_types[0];
        // println!("Use target: {:?}", u.target.borrow());
        assert_eq!(u.target.borrow().get(key), Some(&Some(expected)));
    }
}
