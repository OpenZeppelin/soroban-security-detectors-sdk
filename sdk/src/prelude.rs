use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast_types_builder::ParserCtx,
    file::File,
    node_type::NodeKind,
    symbol_table::{Scope, ScopeRef},
    utils::project::{find_crate_root, FileProvider},
    NodesStorage, SymbolTable,
};

#[derive(Clone)]
pub(crate) struct ExternalCrate {
    id: u32,
    name: String,
    root_file: PathBuf,
    pub(crate) root_scope: ScopeRef,
    // file: Rc<File>,
    storage: NodesStorage,
}

impl ExternalCrate {
    #[must_use]
    pub fn get_root_file(&self) -> Option<Rc<File>> {
        for node in &self.storage.nodes {
            if let NodeKind::File(file) = node {
                if file.path == self.root_file.to_string_lossy() {
                    return Some(file.clone());
                }
            }
        }
        None
    }
}

pub(crate) type ExternPrelude = HashMap<String, ExternalCrate>; // key = crate name

pub(crate) fn insert_into_extern_prelude(
    dir: &Path,
    name: &str,
    prelude: &mut ExternPrelude,
    next_id: &mut u32,
    table: &mut SymbolTable,
    storage: &mut NodesStorage,
) {
    let name = name.replace('-', "_");
    if prelude.contains_key(&name) {
        return;
    }
    if let Some(root) = find_crate_root(dir) {
        let scope = Scope::new(*next_id, name.clone(), None);
        let mut parser = ParserCtx::new(
            *next_id,
            FileProvider::default(),
            scope.clone(),
            storage,
            table,
            root.clone(),
        );
        // let (storage, ast_file) = parse_file(&root);
        parser.parse();
        drop(parser);
        //     for item in ast_file.children.borrow().iter() {
        //         if let NodeKind::Definition(def) = item {
        //             process_definition(&scope, def.clone(), table);
        //         } else if let NodeKind::Directive(Directive::Use(u)) = item {
        //             scope.borrow_mut().imports.push(u.clone());
        //         }
        //     }
        let ec = ExternalCrate {
            id: *next_id,
            name: name.clone(),
            root_file: root,
            root_scope: scope.clone(),
            // file: ast_file,
            storage: storage.clone(),
        };
        prelude.insert(name, ec);
        table.insert_scope(scope.clone());
        *next_id += 1;
        //     prelude.insert(name, ec);
    }
}

// pub(crate) fn build_the_world(
//     prelude: &mut ExternPrelude,
// ) -> anyhow::Result<(SymbolTable, ExternPrelude)> {
//     let mut table = SymbolTable::new();
//     for ec in prelude.values_mut() {
//         let scope = Scope::new(*next_id, name.clone(), None);
//         ec.root_scope = scope.clone();
//         table.insert_scope(scope.clone());
//     }
//     fix_point_resolver(prelude, &mut table);
//     Ok((table, std::mem::take(prelude)))
// }

// fn parse_file(path: &Path) -> (NodesStorage, Rc<File>) {
//     let content = std::fs::read_to_string(path)
//         .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));

//     let ast_file = syn::parse_file(content.as_str())
//         .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));
//     let mut storage = NodesStorage::default();
//     let ast: Rc<File> = build_file(&mut storage, path.to_string_lossy().to_string(), ast_file);
//     (storage, ast)
// }

// pub fn resolve_all(symbols: &mut SymbolTable, externs: &ExternPrelude) {
//     loop {
//         let mut progress = false;

//         for scope in symbols.scopes.values() {
//             let mut stack = vec![scope.clone()];
//             while let Some(sc) = stack.pop() {
//                 for u in &sc.borrow().imports {
//                     if u.is_resolved() {
//                         continue;
//                     }

//                     for path in &u.imported_types {
//                         let (head, tail) = path.split_once("::").unwrap_or((path.as_str(), ""));
//                         let tgt_mod = match head {
//                             "crate" => sc.borrow().crate_root(),
//                             "self" => Some(sc.clone()),
//                             "super" => sc.borrow().parent.clone(),
//                             _ => externs.get(head).map(|e| e.root_scope.clone()).or_else(|| {
//                                 sc.borrow().visible_child(head).and_then(|d| d.as_module())
//                             }),
//                         };
//                         if let Some(m) = tgt_mod {
//                             if let Some(def) = walk_segments(m, tail, sc.borrow().crate_id) {
//                                 u.bind(def);
//                                 progress = true;
//                             }
//                         }
//                     }
//                 }
//                 stack.extend(sc.borrow().children.iter().cloned());
//             }
//         }
//         if !progress {
//             break;
//         }
//     }
// }

// fn walk_segments(mut scope: ScopeRef, tail: &str, from_crate: u32) -> Option<Definition> {
//     if tail.is_empty() {
//         return None;
//     }
//     let mut segs = tail.split("::").peekable();

//     while let Some(seg) = segs.next() {
//         let last = segs.peek().is_none();
//         match scope.borrow().visible_child(seg)? {
//             DefOrScopeRef::Definition(d) => {
//                 if !d
//                     .visibility()
//                     .is_visible_from(from_crate, scope.borrow().crate_id)
//                 {
//                     return None;
//                 }
//             }
//             DefOrScopeRef::Module(s) => {
//                 if last {
//                     None;
//                 }
//                 scope = s;
//             }
//         }
//     }
//     None
// }

// pub fn fix_point_resolver(prelude: &mut ExternPrelude, table: &mut SymbolTable) {
//     loop {
//         let mut progress = false;
//         for ext in prelude.values() {
//             let mut stack = vec![ext.root_scope.clone()];
//             while let Some(scope) = stack.pop() {
//                 for use_dir in &ext.file.imports() {
//                     if use_dir.target.borrow().contains_key(&use_dir.path) {
//                         continue;
//                     }
//                     for imported in &use_dir.imported_types {
//                         let (head, tail) =
//                             imported.split_once("::").unwrap_or((imported.as_str(), ""));
//                         let target_scope = match head {
//                             "crate" | "self" => Some(scope.clone()),
//                             "super" => scope.borrow().parent.clone(),
//                             _ => prelude.get(head).map(|c| c.root_scope.clone()).or_else(|| {
//                                 scope.borrow().children.iter().find_map(|c| {
//                                     if c.borrow().name == head {
//                                         Some(c.clone())
//                                     } else {
//                                         None
//                                     }
//                                 })
//                             }),
//                         };
//                         if let Some(ts) = target_scope {
//                             if let Some(def) = walk_remaining_segments(ts, tail) {
//                                 use_dir
//                                     .target
//                                     .borrow_mut()
//                                     .insert(use_dir.path.clone(), Some(def));
//                                 progress = true;
//                             }
//                         }
//                     }
//                 }
//                 for c in &scope.borrow().children {
//                     stack.push(c.clone());
//                 }
//             }
//         }

//         if !progress {
//             break;
//         }
//     }
// }

// fn walk_remaining_segments(start: ScopeRef, tail: &str) -> Option<Definition> {
//     if tail.is_empty() {
//         return None;
//     }
//     let mut scope = start;
//     let mut segs = tail.split("::").peekable();

//     while let Some(seg) = segs.next() {
//         let last = segs.peek().is_none();
//         if last {
//             return scope.borrow().try_get_definition(seg);
//         }
//         let next_scope = {
//             let borrowed = scope.borrow();
//             borrowed
//                 .children
//                 .iter()
//                 .find(|c| c.borrow().name == seg)?
//                 .clone()
//         };
//         scope = next_scope;
//     }
//     None
// }
