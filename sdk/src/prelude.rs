use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast_types_builder::build_file,
    file::File,
    symbol_table::{Scope, ScopeRef},
    utils::project::find_crate_root,
    NodesStorage,
};

pub(crate) struct ExternalCrate {
    id: u32,
    name: String,
    root_file: PathBuf,
    root_scope: ScopeRef,
    file: Rc<File>,
    storage: NodesStorage,
}

pub(crate) type ExternPrelude = HashMap<String, ExternalCrate>; // key = crate name

pub(crate) fn insert_into_extern_prelude(
    dir: &Path,
    name: String,
    prelude: &mut ExternPrelude,
    next_id: &mut u32,
) {
    let name = name.replace('-', "_");
    if prelude.contains_key(&name) {
        return;
    }
    if let Some(root) = find_crate_root(dir) {
        let scope = Scope::new(*next_id, None);
        let (storage, ast_file) = parse_file(&root);
        let ec = ExternalCrate {
            id: *next_id,
            name: name.clone(),
            root_file: root,
            root_scope: scope,
            file: ast_file,
            storage,
        };
        *next_id += 1;
        prelude.insert(name, ec);
    }
}

fn parse_file(path: &Path) -> (NodesStorage, Rc<File>) {
    let content = std::fs::read_to_string(path)
        .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));

    let ast_file = syn::parse_file(content.as_str())
        .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));
    let mut storage = NodesStorage::default();
    let ast: Rc<File> = build_file(&mut storage, path.to_string_lossy().to_string(), ast_file);
    (storage, ast)
}
