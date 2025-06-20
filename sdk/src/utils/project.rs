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

pub(crate) fn find_user_crate_root(files: &[(PathBuf, String)]) -> anyhow::Result<PathBuf> {
    if let Some((p, _)) = files
        .iter()
        .find(|(p, _)| p.file_name() == Some("lib.rs".as_ref()))
    {
        return Ok(p.clone());
    }
    if let Some((p, _)) = files
        .iter()
        .find(|(p, _)| p.file_name() == Some("main.rs".as_ref()))
    {
        return Ok(p.clone());
    }
    let root = files
        .iter()
        .map(|(p, _)| p.parent().unwrap().to_path_buf())
        .reduce(|a, b| common_prefix(&a, &b))
        .unwrap()
        .join("synthetic_root.rs");
    std::fs::write(&root, "// synthetic root")?;
    Ok(root)
}

fn common_prefix(a: &Path, b: &Path) -> PathBuf {
    let mut prefix = PathBuf::new();
    for (a, b) in a.components().zip(b.components()) {
        if a == b {
            prefix.push(a);
        } else {
            break;
        }
    }
    prefix
}

/// Returns the canonical path of the sub‑module file.
/// The caller can then push it onto the parser queue.
///
/// # Errors
/// * `Err(anyhow!("sub‑module {:?} not found", mod_name))` if no file exists.
/// * `Err` bubbled up from `std::fs::canonicalize` when it fails.
pub fn find_submodule_path(parent_file: &Path, mod_name: &str) -> anyhow::Result<PathBuf> {
    use anyhow::{anyhow, Context};

    let parent_dir = parent_file
        .parent()
        .ok_or_else(|| anyhow!("parent file has no directory: {parent_file:?}"))?;

    // 1.  <parent_dir>/foo.rs
    let cand1 = parent_dir.join(format!("{mod_name}.rs"));
    if cand1.is_file() {
        return std::fs::canonicalize(&cand1).with_context(|| format!("canonicalizing {cand1:?}"));
    }

    // 2.  <parent_dir>/foo/mod.rs
    let cand2 = parent_dir.join(mod_name).join("mod.rs");
    if cand2.is_file() {
        return std::fs::canonicalize(&cand2).with_context(|| format!("canonicalizing {cand2:?}"));
    }

    Err(anyhow!(
        "sub‑module {mod_name} not found next to {parent_file:?}"
    ))
}
