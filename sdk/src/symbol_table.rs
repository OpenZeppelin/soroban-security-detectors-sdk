use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use syn::parse_str;

use crate::custom_type::Type;
use crate::definition::{self, Definition};
use crate::directive::{Directive, Use};
use crate::expression::Expression;
use crate::file::File;
use crate::function::Function;
use crate::literal::Literal;
use crate::node::Visibility;
use crate::node_type::{NodeKind, NodeType};
use crate::prelude::ExternPrelude;
use crate::statement::Statement;
use crate::{Codebase, SealedState};

pub(crate) type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Clone)]
enum DefinitionRef {
    Ref(String, Definition),
    QualifiedName(String),
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
    functions: HashMap<String, Rc<Function>>,
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
            functions: HashMap::new(),
            enums: HashMap::new(),
        }))
    }

    pub(crate) fn insert_def(&mut self, qualified_name: String, def: Definition) {
        self.definitions.insert(qualified_name, def);
    }

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

    fn lookup_def(&self, name: &str) -> Option<DefinitionRef> {
        let mut name: &str = name;
        if self.import_aliases.contains_key(name) {
            name = self.import_aliases.get(name).unwrap();
        }
        if let Some(def) = self.try_get_definition(name) {
            if let Some(res) = self.definitions.keys().find_map(|def_name| {
                if def_name == name {
                    Some(DefinitionRef::Ref(def_name.clone(), def.clone()))
                } else {
                    None
                }
            }) {
                return Some(res);
            }
            return Some(DefinitionRef::Ref(name.to_string(), def.clone())); //FIXME: here return qualified name
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
    //FIXME write qualify_type_name

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
        self.functions.values()
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

    pub(crate) fn as_definition(&self) -> Option<Definition> {
        match self {
            DefOrScopeRef::Module(_) => None,
            DefOrScopeRef::Definition(def) => Some(def.clone()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SymbolTable {
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

    pub(crate) fn build_symbol_tables(&mut self) {
        let scopes = self.scopes.values().cloned().collect::<Vec<_>>();
        for scope in scopes {
            let definitions = scope.borrow().definitions.clone();
            for definition in definitions.values() {
                match definition {
                    // Definition::Const(_) => todo!(),
                    // Definition::ExternCrate(extern_crate) => todo!(),
                    // Definition::Enum(_) => todo!(),
                    // Definition::Contract(_) => todo!(),
                    // Definition::Struct(_) => todo!(),
                    Definition::Function(f) => {
                        let self_type = self.find_self_type_for_method(f.clone());
                        for param in &f.parameters {
                            let mut param_ty = NodeType::from_string(&param.type_name);
                            if param_ty.is_self() {
                                if let Some(self_ty_name) = &self_type {
                                    param_ty = NodeType::from_string(self_ty_name);
                                }
                            }
                            let parent_rc = scope.borrow().parent.as_ref().cloned();
                            if let Some(parent_scope) = parent_rc {
                                if let Some(DefinitionRef::Ref(qname, _)) =
                                    parent_scope.borrow().lookup_def(&param_ty.name())
                                {
                                    param_ty = NodeType::Path(qname.clone());
                                }
                            }
                            scope
                                .borrow_mut()
                                .insert_var(param.name.clone(), param.id, param_ty);
                            if let Some(block) = &f.body {
                                for stmt in &block.statements {
                                    process_statement(stmt, &scope, self);
                                }
                            }
                        }
                    }
                    // Definition::TypeAlias(type_alias) => todo!(),
                    // Definition::AssocType(type_alias) => todo!(),
                    // Definition::Macro(_) => todo!(),
                    // Definition::Module(module) => todo!(),
                    // Definition::Static(_) => todo!(),
                    // Definition::Implementation(implementation) => todo!(),
                    // Definition::Trait(_) => todo!(),
                    // Definition::TraitAlias(trait_alias) => todo!(),
                    // Definition::Plane(plane) => todo!(),
                    // Definition::Union(union) => todo!(),
                    _ => {}
                }
            }
        }
    }

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
    pub fn infer_expr_type(&self, scope_id: u32, expr: &Expression) -> Option<NodeType> {
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
        None
    }

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub(crate) fn resolve_path(&self, scope_id: u32, path: &str) -> Option<DefinitionRef> {
        eprintln!("RESOLVE‑REQ  scope={}  path={}", scope_id, path);
        let mut parts = path
            .split("::")
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            return None;
        }
        let name = parts.pop().unwrap();
        if parts.is_empty() {
            if let Some(scope) = self
                .mod_scopes
                .iter()
                .find(|(_, v)| v.borrow().id == scope_id)
                .map(|(_, v)| v)
            {
                return scope.borrow().lookup_def(name);
            }
        }
        let module_path = parts.join("::");
        for (mod_scope_name, mod_scope) in &self.mod_scopes {
            if *mod_scope_name == module_path
                || mod_scope_name.split("::").next() == Some(module_path.as_str())
            {
                for (def_name, def) in &mod_scope.borrow().definitions {
                    if def_name == name {
                        return Some(DefinitionRef::Ref(def_name.clone(), def.clone()));
                    }
                }
            }
        }
        None
    }

    pub(crate) fn resolve_type_methods(
        &self,
        scope_id: u32,
        path: &str,
    ) -> Option<(&ScopeRef, Vec<Rc<Function>>)> {
        //TODO: return the scope ref here will be enough actually
        let mut parts = path
            .split("::")
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            return None;
        }
        let name = parts.pop().unwrap();
        if parts.is_empty() {
            if let Some(scope) = self
                .mod_scopes
                .iter()
                .find(|(_, v)| v.borrow().id == scope_id)
                .map(|(_, v)| v)
            {
                for methods_group in &scope.borrow().methods {
                    if methods_group.0 == name {
                        return Some((scope, methods_group.1.clone()));
                    }
                }
            }
        } //FIXME: this must iterate only over imported scopes
        let module_path = parts.join("::");
        for (mod_scope_name, mod_scope) in &self.mod_scopes {
            if *mod_scope_name == module_path
                || mod_scope_name.split("::").next() == Some(module_path.as_str())
            {
                for (def_name, _) in &mod_scope.borrow().definitions {
                    if def_name == path && mod_scope.borrow().methods.contains_key(def_name) {
                        return Some((
                            mod_scope,
                            mod_scope
                                .borrow()
                                .methods
                                .get(def_name)
                                .cloned()
                                .unwrap_or_default(),
                        ));
                    }
                }
            }
        }
        None
    }

    // fn get_struct_methods_by_struct_name(&self, name: &str) -> Option<Vec<Rc<Function>>> {
    //     fn recurse(scope: &ScopeRef, name: &str) -> Option<Vec<Rc<Function>>> {
    //         if let Some(methods) = scope.borrow().get_struct_methods_by_struct_name(name) {
    //             return Some(methods.clone());
    //         }
    //         for child in &scope.borrow().children {
    //             if let Some(found) = recurse(child, name) {
    //                 return Some(found);
    //             }
    //         }
    //         None
    //     }
    //     recurse(&self.scope, name)
    // }

    // fn get_struct_methods_by_qualified_struct_name(&self, name: &str) -> Option<Vec<Rc<Function>>> {
    //     fn recurse(scope: &ScopeRef, name: &str) -> Option<Vec<Rc<Function>>> {
    //         if let Some(methods) = scope
    //             .borrow()
    //             .get_struct_methods_by_qualified_struct_name(name)
    //         {
    //             return Some(methods.clone());
    //         }
    //         for child in &scope.borrow().children {
    //             if let Some(found) = recurse(child, name) {
    //                 return Some(found);
    //             }
    //         }
    //         None
    //     }
    //     recurse(&self.scope, name)
    // }

    // Return the `DefinitionName` for a struct (or trait) by its unqualified name.
    // fn get_struct_def_name_by_name(&self, name: &str) -> Option<DefinitionName> {
    //     fn recurse(scope: &ScopeRef, name: &str) -> Option<DefinitionName> {
    //         for def_name in scope.borrow().methods.keys() {
    //             if def_name.name == name {
    //                 return Some(def_name.clone());
    //             }
    //         }
    //         for child in &scope.borrow().children {
    //             if let Some(found) = recurse(child, name) {
    //                 return Some(found);
    //             }
    //         }
    //         None
    //     }
    //     recurse(&self.scope, name)
    // }

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
        for (_, scope) in &self.mod_scopes {
            if let Some(name) = find_in_scope(scope, &function.name) {
                return Some(name);
            }
        }
        None
    }

    pub(crate) fn find_definition_scope(&self, name: &str) -> Option<ScopeRef> {
        for (_, scope) in &self.mod_scopes {
            if scope.borrow().qualify_definition_name(name).is_some() {
                return Some(scope.clone());
            }
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
    let crate_id = parent_scope.borrow().crate_root_id();
    table.insert_def(crate_id, qualified.clone(), def.clone());
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
            fn_scope
                .borrow_mut()
                .functions
                .insert(f.name.clone(), f.clone());
            fn_scope
                .borrow_mut()
                .insert_def(name.clone(), Definition::Function(f.clone()));
        }
        Definition::Implementation(i) => {
            // let type_node = i.for_type.to_type_node();
            if let Type::Typename(target) = &i.for_type {
                let def_name_op = parent_scope.borrow().qualify_definition_name(&target.name);
                if let Some(def_name) = def_name_op {
                    if let Some(method_list) = parent_scope.borrow_mut().methods.get_mut(&def_name)
                    {
                        for func in &i.functions {
                            method_list.push(func.clone());
                        }
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
            let dbg_scope_name = scope.borrow().name.clone();
            for rc_use in &scope.borrow().imports {
                if rc_use.is_resolved() {
                    continue;
                }
                let imported_types = rc_use.imported_types.clone();
                for import_path in imported_types {
                    // Handle alias like "orig_path%alias"
                    let (orig_path, alias_opt) = import_path
                        .split_once('%')
                        .map(|(orig, alias)| (orig.to_string(), Some(alias.to_string())))
                        .unwrap_or((import_path.clone(), None));
                    // Determine the starting scope for resolution
                    let (head, tail) = match orig_path.split_once("::") {
                        Some((h, t)) => (h.to_string(), t.to_string()),
                        None => (orig_path.clone(), String::new()),
                    };
                    let start_scope = match head.as_str() {
                        "crate" => scope.borrow().crate_root(), // current crate root
                        "self" => Some(scope.clone()),          // current scope
                        "super" => scope.borrow().parent.clone(), // parent scope
                        other => {
                            // maybe an extern crate or a top-level module in current crate
                            if let Some(ext) = extern_prelude.get(other) {
                                Some(ext.root_scope.clone())
                            } else {
                                // visible_child checks submodules or definitions by name
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
                        if let Some(def) = walk_segments(start, &tail, scope.clone()) {
                            println!(
                                "Resolved import: {} in scope {}",
                                import_path,
                                scope.borrow().name
                            );
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
}

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
    pub fn is_visible_from(&self, from_scope: &ScopeRef, owner_scope: &ScopeRef) -> bool {
        if *self == Visibility::Public {
            return true;
        }

        let same_crate =
            from_scope.borrow().crate_root_id() == owner_scope.borrow().crate_root_id();

        match self {
            Visibility::Private => {
                let owner_path = scope_path(owner_scope);
                let from_path = scope_path(from_scope);
                from_path.starts_with(&owner_path)
            }
            Visibility::PubCrate => same_crate,
            Visibility::PubSuper => {
                // Visible in the parent of owner and everything below that
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
                // Normalize both paths to Vec<String>
                let target_mod = path
                    .split("::")
                    .filter(|s| !s.is_empty())
                    .map(str::to_owned)
                    .collect::<Vec<_>>();
                let from_path = scope_path(from_scope);
                from_path.starts_with(&target_mod)
            }

            Visibility::Public | _ => unreachable!("handled above"),
        }
    }
}

fn walk_segments(mut scope: ScopeRef, path: &str, from_crate: ScopeRef) -> Option<Definition> {
    if path.is_empty() {
        return None;
    }
    let mut segments = path.split("::").filter(|s| !s.is_empty()).peekable();
    while let Some(seg) = segments.next() {
        let is_last = segments.peek().is_none();
        let child = scope.borrow().visible_child(&seg)?;
        match child {
            DefOrScopeRef::Module(sub_scope) => {
                if is_last {
                    return None;
                }
                scope = sub_scope;
            }
            DefOrScopeRef::Definition(def) => {
                // Found a definition
                if !def.visibility().is_visible_from(&from_crate, &scope) {
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

// #[allow(clippy::too_many_lines)]
// fn process_definition(
//     def: &Definition,
//     parent_path: &str,
//     scope: &ScopeRef,
//     table: &mut SymbolTable,
//     codebase: &Codebase<SealedState>,
// ) {
//     let unqualified = def.name();
//     let qualified_name = format!("{parent_path}::{unqualified}");
//     let def_name = DefinitionName {
//         name: unqualified.clone(),
//         qualified_name: qualified_name.clone(),
//     };
//     scope.borrow_mut().insert_def(def_name, def.clone());
//     match def {
//         Definition::Module(m) => {
//             let module_scope = Scope::new(m.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(module_scope.clone());
//             let path = if parent_path.is_empty() {
//                 m.name.clone()
//             } else {
//                 format!("{}::{}", parent_path, m.name)
//             };
//             table.mod_scopes.insert(path.clone(), module_scope.clone());
//             if let Some(defs) = &m.definitions {
//                 for sub in defs {
//                     process_definition(sub, &path, &module_scope, table, codebase);
//                 }
//             }
//         }
//         Definition::Enum(e) => {
//             let enum_scope = Scope::new(e.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(enum_scope.clone());
//             let def_name = DefinitionName {
//                 name: e.name.clone(),
//                 qualified_name: format!("{parent_path}::{}", e.name),
//             };
//             scope.borrow_mut().enums.entry(def_name).or_default();
//         }
//         Definition::Struct(s) | Definition::Contract(s) => {
//             let struct_scope = Scope::new(s.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(struct_scope.clone());
//             let def_name = DefinitionName {
//                 name: s.name.clone(),
//                 qualified_name: format!("{parent_path}::{}", s.name),
//             };
//             scope.borrow_mut().methods.entry(def_name).or_default(); //FIXME insert to the parent scope?
//             for (field, fty) in &s.fields {
//                 struct_scope
//                     .borrow_mut()
//                     .insert_var(field.clone(), s.id, fty.to_type_node());
//             }
//         }
//         Definition::Implementation(impl_node) => {
//             let impl_scope = Scope::new(impl_node.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(impl_scope.clone());
//             let type_node = impl_node.for_type.to_type_node();
//             let debug = type_node.name();
//             let target_def_name = scope
//                 .borrow()
//                 .qualify_definition_name(&type_node.name())
//                 .unwrap_or_else(|| DefinitionName {
//                     name: type_node.name().rsplit("::").next().unwrap().to_string(), //FIXME suspicious,
//                     qualified_name: format!("{parent_path}::{}", type_node.name()),
//                 });
//             if let Some(def_scope) = table.find_definition_scope(&target_def_name.qualified_name) {
//                 // If the definition scope is found, we can insert the methods into it
//                 def_scope
//                     .borrow_mut()
//                     .methods
//                     .entry(target_def_name)
//                     .and_modify(|methods| methods.extend(impl_node.functions.clone()))
//                     .or_insert_with(|| impl_node.functions.clone());
//             } else {
//                 // If not found, we can insert it into the current scope
//                 scope
//                     .borrow_mut()
//                     .methods
//                     .entry(target_def_name)
//                     .and_modify(|methods| methods.extend(impl_node.functions.clone()))
//                     .or_insert_with(|| impl_node.functions.clone());
//             }
//             // let type_name_str = type_node.name();
//             // let unqualified = type_name_str.rsplit("::").next().unwrap().to_string(); //FIXME suspicious
//             // let def_name = DefinitionName {
//             //     name: unqualified,
//             //     qualified_name: format!("{parent_path}::{type_name_str}"),
//             // };
//             // scope
//             //     .borrow_mut()
//             //     .methods
//             //     .entry(def_name)
//             //     .and_modify(|methods| methods.extend(impl_node.functions.clone()))
//             //     .or_insert_with(|| impl_node.functions.clone()); //FIXME this updates methods for the current scope, not the parent scope, is it correct?
//             for f in &impl_node.functions {
//                 process_definition(
//                     &Definition::Function(f.clone()),
//                     parent_path,
//                     &impl_scope,
//                     table,
//                     codebase,
//                 );
//             }
//         }
//         Definition::Trait(t) => {
//             let def_name = DefinitionName {
//                 name: t.name.clone(),
//                 qualified_name: format!("{parent_path}::{}", t.name),
//             };
//             {
//                 let mut scope_mut = scope.borrow_mut();
//                 let methods_entry = scope_mut.methods.entry(def_name.clone()).or_default();
//                 for item in &t.items {
//                     if let Definition::Function(f) = item {
//                         methods_entry.push(f.clone());
//                     }
//                 }
//             }
//         }
//         Definition::Function(f) => {
//             let fun_scope = Scope::new(f.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(fun_scope.clone());
//             scope.borrow_mut().functions.insert(
//                 //FIXME this should be done for all definitions, then different definitions can be accessed by methods()
//                 DefinitionName {
//                     name: f.name.clone(),
//                     qualified_name: format!("{parent_path}::{}", f.name),
//                 },
//                 f.clone(),
//             );
//             let fname = f.name.clone();
//             let self_ty = table.find_self_type_for_method(f.clone());
//             for p in &f.parameters {
//                 let mut ty_node = match parse_str::<syn::Type>(&p.type_name) {
//                     Ok(ty) => NodeType::from_syn_item(&ty),
//                     Err(_) => NodeType::Path(p.type_name.clone()),
//                 };
//                 if ty_node.is_self() && self_ty.is_some() {
//                     let self_ty_ref = self_ty.as_ref().unwrap();
//                     ty_node = match ty_node {
//                         NodeType::Path(_) => NodeType::Path(self_ty_ref.clone()),
//                         NodeType::Reference {
//                             inner: _,
//                             mutable,
//                             is_explicit_reference,
//                         } => NodeType::Reference {
//                             inner: Box::new(NodeType::Path(self_ty_ref.clone())),
//                             mutable,
//                             is_explicit_reference,
//                         },
//                         NodeType::Ptr { inner: _, mutable } => NodeType::Ptr {
//                             inner: Box::new(NodeType::Path(self_ty_ref.clone())),
//                             mutable,
//                         },
//                         other => other,
//                     };
//                 }
//                 if let Some(def_ref) = scope.borrow().lookup_def(&ty_node.name()) {
//                     //TODO: should this be a part or re-qualification?
//                     match def_ref {
//                         DefinitionRef::Ref(qualified_name, _) => {
//                             ty_node = NodeType::Path(qualified_name);
//                         }
//                         DefinitionRef::QualifiedName(qualified_name) => {
//                             ty_node = NodeType::Path(qualified_name);
//                         }
//                     }
//                 }
//                 fun_scope
//                     .borrow_mut()
//                     .insert_var(p.name.clone(), p.id, ty_node);
//             }
//             // let ty_node = f.returns.to_type_node();
//             // let ret_id = f.returns.id();
//             // let ret_loc = f.returns.location();
//             // if let Some(def_ref) = scope.borrow().lookup_def(&ty_node.name()) {
//             //     match def_ref {
//             //         DefinitionRef::Ref(d) => {}
//             //         DefinitionRef::QualifiedName(qualified_name) => {
//             //             if let Some(f_mut) = Rc::get_mut(&mut f) {
//             //                 f_mut.returns = crate::ast::custom_type::Type::Typename(Rc::new(
//             //                     crate::ast::custom_type::Typename {
//             //                         id: f.returns.id(),
//             //                         location: f.returns.location(),
//             //                         name: qualified_name.clone(),
//             //                     },
//             //                 ));
//             //             }
//             //         }
//             //     }
//             // }

//             if let Some(body) = &f.body {
//                 for stmt in &body.statements {
//                     process_statement(stmt, &fun_scope, table, codebase);
//                 }
//             }
//         }
//         Definition::Const(c) => {
//             let const_scope = Scope::new(c.id, qualified_name, Some(scope.clone()));
//             scope.borrow_mut().children.push(const_scope.clone());
//             let ty_node = c.type_.to_type_node();
//             const_scope
//                 .borrow_mut()
//                 .insert_var(c.name.clone(), c.id, ty_node);
//         }
//         _ => {}
//     }
// }

#[allow(clippy::too_many_lines)]
fn process_statement(stmt: &Statement, scope: &ScopeRef, table: &mut SymbolTable) {
    match stmt {
        Statement::Let(let_stmt) => {
            let mut vty = if let Some(init) = &let_stmt.initial_value {
                if let Some(ty) = infer_expr_type(init, scope, table) {
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
                    DefinitionRef::Ref(_, d) => {}
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
        // Statement::Expression(expr) => {
        //     if let Expression::Identifier(id) = expr {
        //         if let Some(v) = table.lookdown_symbol(scope.borrow().id, &id.name) {
        //             scope.borrow_mut().insert_var(id.name.clone(), v);
        //         }
        //     }
        // }
        // Statement::Definition(def) => process_definition(def.clone(), scope, table, codebase),
        _ => {}
    }
}

#[allow(clippy::too_many_lines)]
fn infer_expr_type(expr: &Expression, scope: &ScopeRef, table: &SymbolTable) -> Option<NodeType> {
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
                                f.returns.to_type_node()
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
                            _ => return None,
                        };
                        return Some(ty);
                    }
                }
            }
            if let Some(v) = scope.borrow().lookup_symbol(&id.name) {
                return Some(v);
            }
            if let Some(def) = scope.borrow().lookup_def(&id.name) {
                let ty_node = match def {
                    DefinitionRef::Ref(_, Definition::Const(c)) => c.type_.to_type_node(),
                    DefinitionRef::Ref(_, Definition::Static(s)) => s.ty.to_type_node(),
                    DefinitionRef::Ref(_, Definition::Function(f)) => f.returns.to_type_node(),
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
                    _ => return None,
                };
                return Some(ty_node);
            }
            None
        }
        Expression::Literal(lit_expr) => match &lit_expr.value {
            Literal::Bool(_) => Some(NodeType::Path("bool".to_string())),
            Literal::Byte(_) => Some(NodeType::Path("u8".to_string())),
            Literal::Char(_) => Some(NodeType::Path("char".to_string())),
            Literal::Int(_) => Some(NodeType::Path("i32".to_string())),
            Literal::Float(_) => Some(NodeType::Path("f64".to_string())),
            Literal::String(_) => Some(NodeType::Path("&str".to_string())),
            Literal::BString(_) => Some(NodeType::Path("&[u8]".to_string())),
            Literal::CString(_) => Some(NodeType::Path("*const c_char".to_string())),
        },
        Expression::Binary(bin) => {
            let _ = infer_expr_type(&bin.left, scope, table)?;
            let _ = infer_expr_type(&bin.right, scope, table)?;
            Some(NodeType::Path("bool".to_string())) //FIXME: return type depends on operator
        }
        Expression::Unary(u) => infer_expr_type(&u.expression, scope, table),
        Expression::FunctionCall(fc) => {
            for def in scope.borrow().functions().chain(scope.borrow().methods()) {
                if def.name == fc.function_name {
                    return Some(def.returns.to_type_node());
                }
            }
            if let Expression::Identifier(base) = &fc.expression {
                let name = base.name.clone();
                if name == "Some" {
                    if let Some(arg) = &fc.parameters.first() {
                        if let Some(ty) = infer_expr_type(arg, scope, table) {
                            return Some(NodeType::Path(format!("Option<{}>", ty.name())));
                        }
                    }
                    return Some(NodeType::Path("Option<_>".to_string()));
                }
                if name == "None" {
                    return Some(NodeType::Path("Option<_>".to_string()));
                }
                if name == "Ok" {
                    if let Some(arg) = &fc.parameters.first() {
                        if let Some(ty) = infer_expr_type(arg, scope, table) {
                            return Some(NodeType::Path(format!("Result<{}, _>", ty.name())));
                        }
                    }
                    return Some(NodeType::Path("Result<_, _>".to_string()));
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
                                return Some(def.returns.to_type_node());
                            }
                        }
                    }
                    if ["Vec", "HashMap"].contains(&module) {
                        return Some(NodeType::Reference {
                            inner: Box::new(NodeType::Path(module.to_string())),
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
            let base_ty = infer_expr_type(&mc.base, scope, table)?;
            if let NodeType::Path(type_name) = &base_ty {
                if let Some((method_scope, method)) = table
                    .resolve_type_methods(scope.borrow().id, type_name) //FIXME i think it is wrong, we need to lookup a qualified name or so, not look down
                    .and_then(|(scope, methods)| {
                        methods
                            .iter()
                            .find(|f| *f.name == mc.method_name)
                            .cloned()
                            .map(|methods| (scope, methods))
                    })
                {
                    let mut ty_node = method.returns.to_type_node();
                    //TODO: can be self
                    if let Some(def) = method_scope.borrow().lookup_def(&ty_node.name()) {
                        match def {
                            DefinitionRef::Ref(qualified_name, _) => {
                                ty_node = NodeType::Path(qualified_name);
                            }
                            DefinitionRef::QualifiedName(qualified_name) => {
                                ty_node = NodeType::Path(qualified_name);
                            }
                        }
                    }
                    return Some(ty_node);
                }
            }
            None
        }
        Expression::MemberAccess(ma) => {
            let base_ty = infer_expr_type(&ma.base, scope, table)?;
            if let NodeType::Path(type_name) = base_ty {
                if let Some(def_ref) = scope.borrow().lookup_def(&type_name) {
                    match def_ref {
                        DefinitionRef::Ref(_, Definition::Struct(s) | Definition::Contract(s)) => {
                            for (field, fty) in &s.fields {
                                if &ma.member_name == field {
                                    return Some(fty.to_type_node());
                                }
                            }
                        }
                        DefinitionRef::Ref(_, Definition::Enum(e)) => {
                            for variant in &e.variants {
                                if &ma.member_name == variant {
                                    return Some(NodeType::Path(type_name.clone()));
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
                                            return Some(fty.to_type_node());
                                        }
                                    }
                                }
                            } else {
                                return Some(NodeType::Path(qn.clone()));
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
            }
            None
        }
        Expression::Return(r) => {
            if let Some(e) = &r.expression {
                infer_expr_type(e, scope, table)
            } else {
                Some(NodeType::Tuple(Vec::new()))
            }
        }
        // Expression::Cast(c) => {
        // let mut ty_node = c.target_type.to_type_node();
        // if ty_node.is_self() {
        //     if let Some(NodeKind::Definition(Definition::Function(f))) =
        //         codebase.get_parent_container(c.id)
        //     {
        //         if let Some(self_ty) = table.find_self_type_for_method(f.clone()) {
        //             ty_node = match ty_node {
        //                 NodeType::Path(ref name) if name == "Self" => {
        //                     NodeType::Path(self_ty.clone())
        //                 }
        //                 NodeType::Reference {
        //                     inner: old_inner,
        //                     mutable,
        //                     is_explicit_reference,
        //                 } if old_inner.name() == "Self" => NodeType::Reference {
        //                     inner: Box::new(NodeType::Path(self_ty.clone())),
        //                     mutable,
        //                     is_explicit_reference,
        //                 },
        //                 NodeType::Ptr { inner, mutable } if inner.name() == "Self" => {
        //                     NodeType::Ptr {
        //                         inner: Box::new(NodeType::Path(self_ty.clone())),
        //                         mutable,
        //                     }
        //                 }
        //                 other => other,
        //             };
        //         }
        //     }
        // }
        // Some(ty_node)
        // }
        // Expression::Closure(cl) => {
        //     let ty_node = cl.returns.to_type_node();
        //     if ty_node.name().is_empty() {
        //         if let Some(ty_node) = infer_expr_type(&cl.body, scope, table) {
        //             return Some(ty_node);
        //         }
        //     }
        //     Some(NodeType::Closure {
        //         inputs: cl
        //             .captures
        //             .iter()
        //             .map(|c| {
        //                 if let Some(ty) =
        //                     infer_expr_type(&Expression::Identifier(c.clone()), scope, table)
        //                 {
        //                     if ty.is_self() {
        //                         if let Some(NodeKind::Statement(Statement::Definition(
        //                             Definition::Function(f),
        //                         ))) = codebase.get_parent_container(c.id)
        //                         {
        //                             if let Some(self_ty) =
        //                                 table.find_self_type_for_method(f.clone())
        //                             {
        //                                 return NodeType::Path(self_ty);
        //                             }
        //                         }
        //                     }
        //                     ty
        //                 } else {
        //                     NodeType::Empty
        //                 }
        //             })
        //             .collect(),
        //         output: Box::new(ty_node),
        //     })
        // }
        Expression::Macro(m) => {
            if m.name == "symbol_short" {
                return Some(NodeType::Path("Symbol".to_string()));
            }
            if m.name == "format" {
                // format! macro returns String
                return Some(NodeType::Path("String".to_string()));
            }
            if m.name == "vec" {
                let re =
                    regex::Regex::new(r"^\s*(?P<elems>.+?)(?:\s*;\s*(?P<count>.+))?\s*$").unwrap();
                if let Some(caps) = re.captures(&m.text) {
                    if caps.name("count").is_some() {
                        let expr_str = caps.name("elems").unwrap().as_str();
                        if let Ok(expr) = syn::parse_str::<syn::Type>(expr_str) {
                            let ty = NodeType::from_syn_item(&expr);
                            return Some(NodeType::Reference {
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
                                let ty = NodeType::from_syn_item(&expr);
                                return Some(NodeType::Reference {
                                    inner: Box::new(ty),
                                    mutable: false,
                                    is_explicit_reference: false,
                                });
                            }
                            if let Ok(syn::Lit::Int(_)) = syn::parse_str::<syn::Lit>(first) {
                                return Some(NodeType::Reference {
                                    inner: Box::new(NodeType::Path("Vec<i32>".to_string())),
                                    mutable: false,
                                    is_explicit_reference: false,
                                });
                            }
                        }
                    }
                }
                // Fallback if parsing fails
                return Some(NodeType::Path("Vec<_>".to_string()));
            }
            None
        }
        Expression::Tuple(t) => {
            let mut types = Vec::new();
            for e in &t.elements {
                if let Some(ty) = infer_expr_type(e, scope, table) {
                    types.push(ty);
                } else {
                    types.push(NodeType::Empty);
                }
            }
            Some(NodeType::Tuple(types))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Codebase, OpenState};

    fn build_table_and_uses(src: &str) -> (SymbolTable, Vec<Rc<Use>>) {
        let mut cb = Codebase::<OpenState>::default();
        let mut content = src.to_string();
        cb.parse_and_add_file("test/test.rs", &mut content).unwrap();
        let sealed = cb.build_api();
        let table = sealed.symbol_table.clone();
        let file = sealed.files[0].clone();
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
        assert!(table
            .resolve_path(
                table.mod_scopes.iter().next().unwrap().1.borrow().id,
                "test::test::a::S"
            )
            .is_some());
        assert!(table
            .resolve_path(
                table.mod_scopes.iter().next().unwrap().1.borrow().id,
                "test::test::a::b::E"
            )
            .is_some());
        assert!(table
            .resolve_path(
                table.mod_scopes.iter().next().unwrap().1.borrow().id,
                "test::test::a::X"
            )
            .is_none());
        assert!(table
            .resolve_path(table.mod_scopes.iter().next().unwrap().1.borrow().id, "")
            .is_none());
    }

    #[test]
    fn test_import_target_resolution() {
        let mut cb = Codebase::<OpenState>::default();
        let mut file1 = r"
        pub type MyType = u8;
        pub mod sub {
            pub struct SubType;
        }
        "
        .to_string();
        cb.parse_and_add_file("test/file1.rs", &mut file1).unwrap();

        let mut file2 = r"
        use file1::MyType;
        use file1::sub::SubType as Renamed;
        "
        .to_string();
        cb.parse_and_add_file("test/file2.rs", &mut file2).unwrap();

        let sealed = cb.build_api();
        let table = sealed.symbol_table.clone();

        let file2 = sealed.files.iter().find(|f| f.name == "file2.rs").unwrap();
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
        let DefinitionRef::Ref(_, my_def) = table
            .resolve_path(
                table
                    .mod_scopes
                    .iter()
                    .find(|s| !s.1.borrow().definitions.is_empty())
                    .unwrap()
                    .1
                    .borrow()
                    .id,
                "test::file1::MyType",
            )
            .unwrap()
        else {
            panic!("Expected a reference to a definition");
        };
        let binding = u1.target.borrow();
        println!("Use target: {:?}, checking key {:?}", binding, full1);
        let found = binding.get(&full1);
        assert_eq!(found.unwrap().as_ref().unwrap().id(), my_def.id());

        let full2 = "file1::sub::SubType%Renamed".to_string();
        let u2 = uses
            .iter()
            .find(|u| u.imported_types == vec![full2.clone()])
            .unwrap();
        let DefinitionRef::Ref(_, sub) = table
            .resolve_path(
                table.mod_scopes.iter().next().unwrap().1.borrow().id,
                "file1::sub::SubType",
            )
            .unwrap()
        else {
            panic!("Expected a reference to a definition");
        };
        assert_eq!(u2.target.borrow().get(&full2), Some(&Some(sub)));
    }

    #[test]
    fn test_import_across_file_scopes() {
        let mut cb = Codebase::<OpenState>::default();
        let mut file1 = r"
            pub struct AStruct;
        "
        .to_string();
        cb.parse_and_add_file("test/file1.rs", &mut file1).unwrap();

        let mut file2 = r"
            use file1::AStruct;
        "
        .to_string();
        cb.parse_and_add_file("test/file2.rs", &mut file2).unwrap();

        let sealed = cb.build_api();
        let table = sealed.symbol_table.clone();

        let file2 = sealed.files.iter().find(|f| f.name == "file2.rs").unwrap();
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
            .get("test::file2")
            .expect("Module scope for file2 not found");
        let DefinitionRef::Ref(_, expected) = table
            .resolve_path(scope.borrow().id, "test::file1::AStruct")
            .unwrap()
        else {
            panic!("Expected a reference to a definition");
        };
        let key = &u.imported_types[0];
        println!("Use target: {:?}", u.target.borrow());
        assert_eq!(u.target.borrow().get(key), Some(&Some(expected)));
    }
}
