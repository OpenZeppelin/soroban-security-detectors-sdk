use std::{
    cell::RefCell,
    fs, io,
    path::{Path, PathBuf},
    rc::Rc,
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

pub(crate) fn find_user_crate_root(
    files: &Rc<RefCell<Vec<(PathBuf, String)>>>,
    file_provider: &FileProvider,
) -> anyhow::Result<PathBuf> {
    if let Some((p, _)) = files
        .borrow()
        .iter()
        .find(|(p, _)| p.file_name() == Some("lib.rs".as_ref()))
    {
        return Ok(p.clone());
    }
    if let Some((p, _)) = files
        .borrow()
        .iter()
        .find(|(p, _)| p.file_name() == Some("main.rs".as_ref()))
    {
        return Ok(p.clone());
    }
    let root = files
        .borrow()
        .iter()
        .map(|(p, _)| p.parent().unwrap().to_path_buf())
        .reduce(|a, b| common_prefix(&a, &b))
        .unwrap()
        .join("synthetic_root.rs");
    file_provider.write(&root, "// synthetic root")?;
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
#[allow(clippy::unnecessary_debug_formatting)]
pub fn find_submodule_path(
    parent_file: &Path,
    mod_name: &str,
    file_provider: &FileProvider,
) -> anyhow::Result<PathBuf> {
    use anyhow::{anyhow, Context};

    let parent_dir = parent_file
        .parent()
        .ok_or_else(|| anyhow!("parent file has no directory: {parent_file:?}"))?;

    // 1.  <parent_dir>/foo.rs
    let cand1 = parent_dir.join(format!("{mod_name}.rs"));
    match file_provider {
        FileProvider::Fs(_) => {
            if cand1.is_file() {
                return std::fs::canonicalize(&cand1)
                    .with_context(|| format!("canonicalizing {cand1:?}"));
            }
        }
        FileProvider::Mem(loader) => {
            if loader.exists(&cand1) {
                return Ok(cand1);
            }
        }
    }
    // if file_provider.exists(&cand1) || cand1.is_file() {
    //     return std::fs::canonicalize(&cand1).with_context(|| format!("canonicalizing {cand1:?}"));
    // }

    // 2.  <parent_dir>/foo/mod.rs
    let cand2 = parent_dir.join(mod_name).join("mod.rs");
    if cand2.is_file() {
        return std::fs::canonicalize(&cand2).with_context(|| format!("canonicalizing {cand2:?}"));
    }

    Err(anyhow!(
        "sub‑module {mod_name} not found next to {parent_file:?}"
    ))
}

pub(crate) enum FileProvider {
    Fs(StdFsWrapper),
    Mem(Box<dyn FileLoader>),
}

impl Default for FileProvider {
    fn default() -> Self {
        FileProvider::Fs(StdFsWrapper {})
    }
}

impl FileProvider {
    #[allow(clippy::unnecessary_debug_formatting)]
    pub(crate) fn read(&self, path: &Path) -> io::Result<String> {
        match self {
            FileProvider::Fs(_) => StdFsWrapper::std_fs_read_to_string_wapper(path),
            FileProvider::Mem(loader) => match path.to_str() {
                Some(s) => loader.read(s),
                None => Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Invalid path: {path:?}"),
                )),
            },
        }
    }

    #[allow(dead_code)]
    pub(crate) fn exists(&self, path: &Path) -> bool {
        match self {
            FileProvider::Fs(_) => std::path::Path::new(path).exists(),
            FileProvider::Mem(loader) => loader.exists(path),
        }
    }

    pub(crate) fn write(&self, path: &Path, content: &str) -> io::Result<()> {
        match self {
            FileProvider::Fs(_) => StdFsWrapper::write_to_string_wapper(path, content),
            FileProvider::Mem(loader) => loader.write(path, content),
        }
    }

    // pub fn append(&self, path: &Path, content: &str) -> io::Result<()> {
    //     match self {
    //         FileProvider::Fs(_) => StdFsWrapper::append_to_file(path, content),
    //         FileProvider::Mem(loader) => loader.append(path, content),
    //     }
    // }
}

#[derive(Default)]
pub(crate) struct StdFsWrapper {}

impl StdFsWrapper {
    fn std_fs_read_to_string_wapper(path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn write_to_string_wapper(path: &Path, content: &str) -> io::Result<()> {
        std::fs::write(path, content)
    }

    // fn append_to_file(path: &Path, content: &str) -> io::Result<()> {
    //     let mut file = OpenOptions::new()
    //         .append(true)
    //         .open(path)
    //         .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
    //     file.write_all(content.as_bytes())
    //         .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    // }
}

pub trait FileLoader {
    fn exists(&self, path: &Path) -> bool;
    /// Reads the contents of the file at the given path.
    ///
    /// # Errors
    /// Returns an error if the file does not exist or cannot be read.
    fn read(&self, path: &str) -> io::Result<String>;

    fn write(&self, path: &Path, content: &str) -> io::Result<()>;

    // fn append(&self, path: &Path, content: &str) -> io::Result<()>;
}

#[derive(Clone)]
pub(crate) struct MemoryFS {
    pub(crate) files: Rc<RefCell<Vec<(PathBuf, String)>>>,
}

impl FileLoader for MemoryFS {
    fn exists(&self, path: &Path) -> bool {
        match path.to_str() {
            Some(_) => self.files.borrow().iter().any(|(p, _)| p == path),
            None => false,
        }
    }
    fn read(&self, path: &str) -> io::Result<String> {
        self.files
            .borrow()
            .iter()
            .find(|(p, _)| p.to_str() == Some(path))
            .map(|(_, content)| content.clone())
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::NotFound, format!("File not found: {path}"))
            })
    }
    fn write(&self, path: &Path, content: &str) -> io::Result<()> {
        self.files
            .borrow_mut()
            .push((PathBuf::from(path), content.to_string()));
        Ok(())
    }
    // fn append(&self, path: &Path, content: &str) -> io::Result<()> {
    //     let mut files = self.files.borrow_mut();
    //     if let Some((_, existing_content)) = files.iter_mut().find(|(p, _)| p == path) {
    //         existing_content.push_str(content);
    //     } else {
    //         files.push((PathBuf::from(path), content.to_string()));
    //     }
    //     Ok(())
    // }
}
