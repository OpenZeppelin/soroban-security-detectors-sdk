use std::{cell::RefCell, collections::HashMap, rc::Rc};
use syn::parse_str;

use crate::definition::Definition;
use crate::directive::{Directive, Use};
use crate::expression::Expression;
use crate::file::File;
use crate::function::Function;
use crate::literal::Literal;
use crate::node_type::{NodeKind, NodeType};
use crate::statement::Statement;
use crate::{Codebase, SealedState};

pub(crate) type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Clone)]
enum DefinitionRef {
    Ref(String, Definition),
    QualifiedName(String),
}

#[derive(Debug)]
pub(crate) struct Scope {
    id: u32,
    parent: Option<ScopeRef>,
    children: Vec<ScopeRef>,
    imports: Vec<Rc<Use>>,
    import_aliases: HashMap<String, String>,
    definitions: HashMap<DefinitionName, Definition>,
    variables: HashMap<String, (u32, NodeType)>,
    functions: HashMap<DefinitionName, Rc<Function>>,
    // Structs and their methods
    methods: HashMap<DefinitionName, Vec<Rc<Function>>>,
    // Enums and their methods
    enums: HashMap<DefinitionName, Vec<Rc<Function>>>,
}

impl Scope {
    pub(crate) fn new(id: u32, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            id,
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

    fn insert_def(&mut self, def_name: DefinitionName, def: Definition) {
        self.definitions.insert(def_name, def);
    }

    fn lookup_def(&self, name: &str) -> Option<DefinitionRef> {
        let mut name: &str = name;
        if self.import_aliases.contains_key(name) {
            name = self.import_aliases.get(name).unwrap();
        }
        if let Some(def) = self.try_get_definition(name) {
            if let Some(res) = self.definitions.keys().find_map(|def_name| {
                if def_name.name == name || def_name.qualified_name == name {
                    Some(DefinitionRef::Ref(
                        def_name.qualified_name.clone(),
                        def.clone(),
                    ))
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

    fn try_get_definition(&self, name: &str) -> Option<Definition> {
        self.definitions.keys().find_map(|def_name| {
            if def_name.name == name || def_name.qualified_name == name {
                self.definitions.get(def_name).cloned()
            } else {
                None
            }
        })
    }

    fn qualify_definition_name(&self, name: &str) -> Option<DefinitionName> {
        if let Some(res) = self.definitions.keys().find_map(|def_name| {
            if def_name.name == name || def_name.qualified_name == name {
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

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scope: ScopeRef,
    mod_scopes: HashMap<String, ScopeRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DefinitionName {
    name: String,
    qualified_name: String,
}

impl SymbolTable {
    #[must_use]
    #[allow(clippy::missing_panics_doc, clippy::too_many_lines)]
    pub fn from_codebase(codebase: &Codebase<SealedState>) -> Self {
        let root = Scope::new(0, None);
        let mut table = SymbolTable {
            scope: root.clone(),
            mod_scopes: HashMap::new(),
        };

        let (mut soroban_sdk_files, mut user_files): (Vec<_>, Vec<_>) =
            codebase.files.iter().partition(|f| {
                f.file_module_name().starts_with("soroban_sdk::")
                    || f.file_module_name().starts_with("soroban_sdk_macro")
            });

        soroban_sdk_files.sort_by_key(|f| f.path.clone());
        user_files.sort_by_key(|f| f.path.clone());

        SymbolTable::process_files(root.clone(), &mut table, codebase, soroban_sdk_files);
        // let root_mod = codebase
        //     .files
        //     .iter()
        //     .map(|f| f.path.split('/').collect::<Vec<_>>())
        //     .fold(None, |acc: Option<Vec<&str>>, path_parts| match acc {
        //         None => Some(path_parts),
        //         Some(common) => Some(
        //             common
        //                 .into_iter()
        //                 .zip(path_parts)
        //                 .take_while(|(a, b)| a == b)
        //                 .map(|(a, _)| a)
        //                 .collect(),
        //         ),
        //     })
        //     .unwrap_or_default()
        //     .join("/");
        SymbolTable::process_files(root.clone(), &mut table, codebase, user_files);

        table
    }

    #[allow(clippy::needless_pass_by_value)]
    fn process_files(
        root: ScopeRef,
        table: &mut SymbolTable,
        codebase: &Codebase<SealedState>,
        files: Vec<&Rc<File>>,
    ) {
        // Pass 1: Per-File Symbol Discovery
        let mut created_mod_scopes = vec![];
        for file in files {
            let file_mod_name = format!("test::{}", file.file_module_name());
            let file_mod_scope = Scope::new(file.id, Some(root.clone()));
            created_mod_scopes.push(file_mod_scope.clone());
            root.borrow_mut().children.push(file_mod_scope.clone());
            table
                .mod_scopes
                .insert(file_mod_name.clone(), file_mod_scope.clone());

            let mut children = file.children.borrow().clone();
            children.sort_by(|a, b| match (a, b) {
                (NodeKind::Directive(_), NodeKind::Directive(_)) => std::cmp::Ordering::Equal,
                (NodeKind::Directive(_), _) => std::cmp::Ordering::Less,
                (_, NodeKind::Directive(_)) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            });
            for child in children {
                match &child {
                    NodeKind::Directive(Directive::Use(rc_use)) => {
                        file_mod_scope.borrow_mut().imports.push(rc_use.clone());
                    }
                    NodeKind::Definition(def) => {
                        process_definition(def, &file_mod_name, &file_mod_scope, table, codebase);
                    }
                    _ => {}
                }
            }
        }

        // Pass 2: Import Resolution and Linking
        for scope in &created_mod_scopes {
            let imports = scope.borrow().imports.clone();
            for import in imports {
                for imported in &import.imported_types {
                    let (mut path, _) = if let Some((orig, alias)) = imported.split_once('%') {
                        scope
                            .borrow_mut()
                            .import_aliases
                            .insert(alias.to_string(), orig.to_string());
                        (orig.to_string(), Some(alias.to_string()))
                    } else {
                        (imported.to_string(), None)
                    };
                    if let Some(def_ref) = table.resolve_path(scope.borrow().id, &path) {
                        match def_ref {
                            DefinitionRef::Ref(_, def) => {
                                import
                                    .target
                                    .borrow_mut()
                                    .insert(imported.clone(), Some(def.clone()));
                            }
                            DefinitionRef::QualifiedName(_) => {}
                        }
                    } else {
                        import.target.borrow_mut().insert(imported.clone(), None);
                    }
                }
            }
        }

        // let mut defs = table.scope.borrow_mut().definitions.clone();
        // let mut imps = table.scope.borrow_mut().imports.clone();
        for scope in &created_mod_scopes {
            SymbolTable::re_qualify_variables(scope, &mut HashMap::new(), &mut Vec::new());
        }

        // Re-qualify variables whose types match imported aliases
        // for (key, scope) in &table.mod_scopes {
        //     let mut to_update = Vec::new();
        //     for (var, (id, ty)) in &scope.borrow().variables {
        //         if let NodeType::Path(name) = ty {
        //             if let Some(full) = scope.borrow().import_aliases.get(name) {
        //                 if ty.name() != *full {
        //                     to_update.push((var.clone(), *id, full.clone()));
        //                 }
        //             }
        //         }
        //     }
        //     for (var, id, full) in to_update {
        //         scope.borrow_mut().insert_var(var, id, NodeType::Path(full));
        //     }
        // }
        // Pass 3: Fill types for missed symbols
        let mut scopes = created_mod_scopes;
        while let Some(scope) = scopes.pop() {
            let variables = scope.borrow().variables.clone();
            for (_, id_nt) in variables {
                match &id_nt.1 {
                    NodeType::Empty => {
                        if let Some(NodeKind::Statement(s)) = codebase.storage.find_node(id_nt.0) {
                            process_statement(&s, &scope, table, codebase);
                        }
                    }
                    NodeType::Path(_path) => {}
                    _ => {}
                }
            }
            for child in &scope.borrow().children {
                scopes.push(child.clone());
            }
        }
    }

    fn re_qualify_variables(
        scope: &ScopeRef,
        definitions: &mut HashMap<DefinitionName, Definition>,
        imports: &mut Vec<Rc<Use>>,
    ) {
        for import in &scope.borrow().imports {
            if !imports.iter().any(|i| i.id == import.id) {
                imports.push(import.clone());
            }
        }
        for (name, def) in &scope.borrow().definitions {
            if !definitions.contains_key(name) {
                definitions.insert(name.clone(), def.clone());
            }
        }
        for var in &mut scope.borrow_mut().variables {
            if let NodeType::Path(name) = &var.1 .1.clone() {
                for i in imports.iter() {
                    //what if we have this definition in the scope?
                    if let Some(imported) = i
                        .imported_types
                        .iter()
                        .find(|it| *it == name || it.split("::").last() == Some(name))
                    {
                        var.1 .1 = NodeType::Path(imported.clone());
                    }
                }
            }
        }
        for child in &scope.borrow().children {
            SymbolTable::re_qualify_variables(child, definitions, imports);
        }
    }

    // #[must_use]
    // pub fn lookup_def(&self, name: &str) -> Option<DefinitionRef> {
    //     self.scope.borrow().lookup_def(name)
    // }

    #[must_use]
    pub fn lookup_symbol(&self, name: &str) -> Option<NodeType> {
        self.scope.borrow().lookup_symbol(name)
    }

    #[must_use]
    pub fn lookup_symbol_in_scope(&self, scope_id: u32, name: &str) -> Option<NodeType> {
        if self.scope.borrow().id == scope_id {
            return self.scope.borrow().lookup_symbol(name);
        }
        let mut stack = vec![self.scope.clone()];
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
    pub fn infer_expr_type(
        &self,
        scope_id: u32,
        expr: &Expression,
        codebase: &Codebase<SealedState>,
    ) -> Option<NodeType> {
        let mut stack = vec![self.scope.clone()];
        while let Some(scope) = stack.pop() {
            if scope.borrow().id == scope_id {
                return infer_expr_type(expr, &scope, self, codebase);
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
                    if def_name.name == name || def_name.qualified_name == path {
                        return Some(DefinitionRef::Ref(
                            def_name.qualified_name.clone(),
                            def.clone(),
                        ));
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
                    if methods_group.0.name == name || methods_group.0.qualified_name == path {
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
                for (def_name, def) in &mod_scope.borrow().definitions {
                    if (def_name.name == name || def_name.qualified_name == path)
                        && mod_scope.borrow().methods.contains_key(def_name)
                    {
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
                    return Some(type_name.name.clone());
                }
            }
            for child in &scope_b.children {
                if let Some(name) = find_in_scope(child, function_name) {
                    return Some(name);
                }
            }
            None
        }
        find_in_scope(&self.scope, &function.name)
    }

    pub(crate) fn find_definition_scope(&self, name: &str) -> Option<ScopeRef> {
        let mut stack = vec![self.scope.clone()];
        while let Some(scope) = stack.pop() {
            if scope.borrow().try_get_definition(name).is_some() {
                return Some(scope);
            }
            for child in &scope.borrow().children {
                stack.push(child.clone());
            }
        }
        None
    }
}

#[allow(clippy::too_many_lines)]
fn process_definition(
    def: &Definition,
    parent_path: &str,
    scope: &ScopeRef,
    table: &mut SymbolTable,
    codebase: &Codebase<SealedState>,
) {
    let unqualified = def.name();
    let def_name = DefinitionName {
        name: unqualified.clone(),
        qualified_name: format!("{parent_path}::{unqualified}"),
    };
    scope.borrow_mut().insert_def(def_name, def.clone());
    match def {
        Definition::Module(m) => {
            let module_scope = Scope::new(m.id, Some(scope.clone()));
            scope.borrow_mut().children.push(module_scope.clone());
            let path = if parent_path.is_empty() {
                m.name.clone()
            } else {
                format!("{}::{}", parent_path, m.name)
            };
            table.mod_scopes.insert(path.clone(), module_scope.clone());
            if let Some(defs) = &m.definitions {
                for sub in defs {
                    process_definition(sub, &path, &module_scope, table, codebase);
                }
            }
        }
        Definition::Enum(e) => {
            let enum_scope = Scope::new(e.id, Some(scope.clone()));
            scope.borrow_mut().children.push(enum_scope.clone());
            let def_name = DefinitionName {
                name: e.name.clone(),
                qualified_name: format!("{parent_path}::{}", e.name),
            };
            scope.borrow_mut().enums.entry(def_name).or_default();
        }
        Definition::Struct(s) | Definition::Contract(s) => {
            let struct_scope = Scope::new(s.id, Some(scope.clone()));
            scope.borrow_mut().children.push(struct_scope.clone());
            let def_name = DefinitionName {
                name: s.name.clone(),
                qualified_name: format!("{parent_path}::{}", s.name),
            };
            scope.borrow_mut().methods.entry(def_name).or_default(); //FIXME insert to the parent scope?
            for (field, fty) in &s.fields {
                struct_scope
                    .borrow_mut()
                    .insert_var(field.clone(), s.id, fty.to_type_node());
            }
        }
        Definition::Implementation(impl_node) => {
            let impl_scope = Scope::new(impl_node.id, Some(scope.clone()));
            scope.borrow_mut().children.push(impl_scope.clone());
            let type_node = impl_node.for_type.to_type_node();
            let debug = type_node.name();
            let target_def_name = scope
                .borrow()
                .qualify_definition_name(&type_node.name())
                .unwrap_or_else(|| DefinitionName {
                    name: type_node.name().rsplit("::").next().unwrap().to_string(), //FIXME suspicious,
                    qualified_name: format!("{parent_path}::{}", type_node.name()),
                });
            if let Some(def_scope) = table.find_definition_scope(&target_def_name.qualified_name) {
                // If the definition scope is found, we can insert the methods into it
                def_scope
                    .borrow_mut()
                    .methods
                    .entry(target_def_name)
                    .and_modify(|methods| methods.extend(impl_node.functions.clone()))
                    .or_insert_with(|| impl_node.functions.clone());
            } else {
                // If not found, we can insert it into the current scope
                scope
                    .borrow_mut()
                    .methods
                    .entry(target_def_name)
                    .and_modify(|methods| methods.extend(impl_node.functions.clone()))
                    .or_insert_with(|| impl_node.functions.clone());
            }
            // let type_name_str = type_node.name();
            // let unqualified = type_name_str.rsplit("::").next().unwrap().to_string(); //FIXME suspicious
            // let def_name = DefinitionName {
            //     name: unqualified,
            //     qualified_name: format!("{parent_path}::{type_name_str}"),
            // };
            // scope
            //     .borrow_mut()
            //     .methods
            //     .entry(def_name)
            //     .and_modify(|methods| methods.extend(impl_node.functions.clone()))
            //     .or_insert_with(|| impl_node.functions.clone()); //FIXME this updates methods for the current scope, not the parent scope, is it correct?
            for f in &impl_node.functions {
                process_definition(
                    &Definition::Function(f.clone()),
                    parent_path,
                    &impl_scope,
                    table,
                    codebase,
                );
            }
        }
        Definition::Trait(t) => {
            let def_name = DefinitionName {
                name: t.name.clone(),
                qualified_name: format!("{parent_path}::{}", t.name),
            };
            {
                let mut scope_mut = scope.borrow_mut();
                let methods_entry = scope_mut.methods.entry(def_name.clone()).or_default();
                for item in &t.items {
                    if let Definition::Function(f) = item {
                        methods_entry.push(f.clone());
                    }
                }
            }
        }
        Definition::Function(f) => {
            let fun_scope = Scope::new(f.id, Some(scope.clone()));
            scope.borrow_mut().children.push(fun_scope.clone());
            scope.borrow_mut().functions.insert(
                //FIXME this should be done for all definitions, then different definitions can be accessed by methods()
                DefinitionName {
                    name: f.name.clone(),
                    qualified_name: format!("{parent_path}::{}", f.name),
                },
                f.clone(),
            );
            let fname = f.name.clone();
            let self_ty = table.find_self_type_for_method(f.clone());
            for p in &f.parameters {
                let mut ty_node = match parse_str::<syn::Type>(&p.type_name) {
                    Ok(ty) => NodeType::from_syn_item(&ty),
                    Err(_) => NodeType::Path(p.type_name.clone()),
                };
                if ty_node.is_self() && self_ty.is_some() {
                    let self_ty_ref = self_ty.as_ref().unwrap();
                    ty_node = match ty_node {
                        NodeType::Path(_) => NodeType::Path(self_ty_ref.clone()),
                        NodeType::Reference {
                            inner: _,
                            mutable,
                            is_explicit_reference,
                        } => NodeType::Reference {
                            inner: Box::new(NodeType::Path(self_ty_ref.clone())),
                            mutable,
                            is_explicit_reference,
                        },
                        NodeType::Ptr { inner: _, mutable } => NodeType::Ptr {
                            inner: Box::new(NodeType::Path(self_ty_ref.clone())),
                            mutable,
                        },
                        other => other,
                    };
                }
                if let Some(def_ref) = scope.borrow().lookup_def(&ty_node.name()) {
                    //TODO: should this be a part or re-qualification?
                    match def_ref {
                        DefinitionRef::Ref(qualified_name, _) => {
                            ty_node = NodeType::Path(qualified_name);
                        }
                        DefinitionRef::QualifiedName(qualified_name) => {
                            ty_node = NodeType::Path(qualified_name);
                        }
                    }
                }
                fun_scope
                    .borrow_mut()
                    .insert_var(p.name.clone(), p.id, ty_node);
            }
            // let ty_node = f.returns.to_type_node();
            // let ret_id = f.returns.id();
            // let ret_loc = f.returns.location();
            // if let Some(def_ref) = scope.borrow().lookup_def(&ty_node.name()) {
            //     match def_ref {
            //         DefinitionRef::Ref(d) => {}
            //         DefinitionRef::QualifiedName(qualified_name) => {
            //             if let Some(f_mut) = Rc::get_mut(&mut f) {
            //                 f_mut.returns = crate::ast::custom_type::Type::Typename(Rc::new(
            //                     crate::ast::custom_type::Typename {
            //                         id: f.returns.id(),
            //                         location: f.returns.location(),
            //                         name: qualified_name.clone(),
            //                     },
            //                 ));
            //             }
            //         }
            //     }
            // }

            if let Some(body) = &f.body {
                for stmt in &body.statements {
                    process_statement(stmt, &fun_scope, table, codebase);
                }
            }
        }
        Definition::Const(c) => {
            let const_scope = Scope::new(c.id, Some(scope.clone()));
            scope.borrow_mut().children.push(const_scope.clone());
            let ty_node = c.type_.to_type_node();
            const_scope
                .borrow_mut()
                .insert_var(c.name.clone(), c.id, ty_node);
        }
        _ => {}
    }
}

#[allow(clippy::too_many_lines)]
fn process_statement(
    stmt: &Statement,
    scope: &ScopeRef,
    table: &mut SymbolTable,
    codebase: &Codebase<SealedState>,
) {
    match stmt {
        Statement::Let(let_stmt) => {
            let mut vty = if let Some(init) = &let_stmt.initial_value {
                if let Some(ty) = infer_expr_type(init, scope, table, codebase) {
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
                process_statement(stmt, scope, table, codebase);
            }
        }
        Statement::Block(block) => {
            for stmt in &block.statements {
                process_statement(stmt, scope, table, codebase);
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
fn infer_expr_type(
    expr: &Expression,
    scope: &ScopeRef,
    table: &SymbolTable,
    codebase: &Codebase<SealedState>,
) -> Option<NodeType> {
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
            let _ = infer_expr_type(&bin.left, scope, table, codebase)?;
            let _ = infer_expr_type(&bin.right, scope, table, codebase)?;
            Some(NodeType::Path("bool".to_string())) //FIXME: return type depends on operator
        }
        Expression::Unary(u) => infer_expr_type(&u.expression, scope, table, codebase),
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
                        if let Some(ty) = infer_expr_type(arg, scope, table, codebase) {
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
                        if let Some(ty) = infer_expr_type(arg, scope, table, codebase) {
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
            let base_ty = infer_expr_type(&mc.base, scope, table, codebase)?;
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
            let base_ty = infer_expr_type(&ma.base, scope, table, codebase)?;
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
                infer_expr_type(e, scope, table, codebase)
            } else {
                Some(NodeType::Tuple(Vec::new()))
            }
        }
        Expression::Cast(c) => {
            let mut ty_node = c.target_type.to_type_node();
            if ty_node.is_self() {
                if let Some(NodeKind::Definition(Definition::Function(f))) =
                    codebase.get_parent_container(c.id)
                {
                    if let Some(self_ty) = table.find_self_type_for_method(f.clone()) {
                        ty_node = match ty_node {
                            NodeType::Path(ref name) if name == "Self" => {
                                NodeType::Path(self_ty.clone())
                            }
                            NodeType::Reference {
                                inner: old_inner,
                                mutable,
                                is_explicit_reference,
                            } if old_inner.name() == "Self" => NodeType::Reference {
                                inner: Box::new(NodeType::Path(self_ty.clone())),
                                mutable,
                                is_explicit_reference,
                            },
                            NodeType::Ptr { inner, mutable } if inner.name() == "Self" => {
                                NodeType::Ptr {
                                    inner: Box::new(NodeType::Path(self_ty.clone())),
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
        Expression::Closure(cl) => {
            let ty_node = cl.returns.to_type_node();
            if ty_node.name().is_empty() {
                if let Some(ty_node) = infer_expr_type(&cl.body, scope, table, codebase) {
                    return Some(ty_node);
                }
            }
            Some(NodeType::Closure {
                inputs: cl
                    .captures
                    .iter()
                    .map(|c| {
                        if let Some(ty) = infer_expr_type(
                            &Expression::Identifier(c.clone()),
                            scope,
                            table,
                            codebase,
                        ) {
                            if ty.is_self() {
                                if let Some(NodeKind::Statement(Statement::Definition(
                                    Definition::Function(f),
                                ))) = codebase.get_parent_container(c.id)
                                {
                                    if let Some(self_ty) =
                                        table.find_self_type_for_method(f.clone())
                                    {
                                        return NodeType::Path(self_ty);
                                    }
                                }
                            }
                            ty
                        } else {
                            NodeType::Empty
                        }
                    })
                    .collect(),
                output: Box::new(ty_node),
            })
        }
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
                if let Some(ty) = infer_expr_type(e, scope, table, codebase) {
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
    use crate::{Codebase, OpenState, SealedState};

    fn build_table_and_uses(src: &str) -> (SymbolTable, Vec<Rc<Use>>) {
        let mut cb = Codebase::<OpenState>::default();
        let mut content = src.to_string();
        cb.parse_and_add_file("test/test.rs", &mut content).unwrap();
        let sealed = cb.build_api();
        let table = sealed.symbol_table.as_ref().unwrap().clone();
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
        let table = sealed.symbol_table.as_ref().unwrap().clone();

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
        let table = sealed.symbol_table.as_ref().unwrap().clone();

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
