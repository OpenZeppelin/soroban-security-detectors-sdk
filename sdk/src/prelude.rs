use std::{collections::HashMap, path::Path};

use crate::{
    ast_types_builder::ParserCtx,
    symbol_table::{Scope, ScopeRef},
    utils::project::{find_crate_root, FileProvider},
    NodesStorage, SymbolTable,
};

#[derive(Clone)]
pub(crate) struct ExternalCrate {
    pub(crate) root_scope: ScopeRef,
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
        parser.parse();
        drop(parser);
        let ec = ExternalCrate {
            root_scope: scope.clone(),
        };
        prelude.insert(name, ec);
        table.insert_scope(scope.clone());
        *next_id += 1;
    }
}
