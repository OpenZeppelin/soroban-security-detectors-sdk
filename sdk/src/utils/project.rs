use std::{
    fs,
    path::{Path, PathBuf},
};

pub fn find_crate_root(crate_dir: &Path) -> Option<PathBuf> {
    let src = crate_dir.join("src");
    let lib = src.join("lib.rs");
    if lib.is_file() {
        return Some(lib);
    }

    let main = src.join("main.rs");
    if main.is_file() {
        return Some(main);
    }

    fs::read_dir(&src)
        .ok()?
        .filter_map(std::result::Result::ok)
        .find(|e| e.path().extension().is_some_and(|ext| ext == "rs"))
        .map(|e| e.path())
}
