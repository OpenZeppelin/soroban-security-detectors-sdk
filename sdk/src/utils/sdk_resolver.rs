use semver::Version;

use std::{
    collections::HashMap,
    env, fs, io,
    path::{self, Path, PathBuf},
};

fn collect_files_in_dir(
    dir_path: &Path,
    files_map: &mut HashMap<String, String>,
) -> io::Result<()> {
    if !dir_path.is_dir() {
        return Ok(());
    }
    for entry in fs::read_dir(dir_path)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file()
            && path.extension().is_some_and(|ext| ext == "rs")
            && !path.ends_with("build.rs")
        {
            if let Ok(content) = fs::read_to_string(&path) {
                if let Some(abs_path_str) = path.to_str() {
                    //"/home/georgii/.cargo/registry/src/index.crates.io-6f17d22bba15001f/soroban-sdk-macros-22.0.4/build.rs"
                    if let Some((_, r)) = abs_path_str.split_once(&format!(
                        ".cargo{}registry{}src{}",
                        path::MAIN_SEPARATOR,
                        path::MAIN_SEPARATOR,
                        path::MAIN_SEPARATOR
                    )) {
                        if let Some((_, r)) = r.split_once(path::MAIN_SEPARATOR) {
                            if r.starts_with("soroban-sdk-macros") {
                                if let Some((_, r)) = r.split_once(&format!(
                                    "{}src{}",
                                    path::MAIN_SEPARATOR,
                                    path::MAIN_SEPARATOR
                                )) {
                                    files_map.insert(
                                        format!("soroban-sdk-macros{}{r}", path::MAIN_SEPARATOR),
                                        content,
                                    );
                                }
                            } else if r.starts_with("soroban-sdk") {
                                if let Some((_, r)) = r.split_once(&format!(
                                    "{}src{}",
                                    path::MAIN_SEPARATOR,
                                    path::MAIN_SEPARATOR
                                )) {
                                    if r.starts_with("test") {
                                        continue;
                                    }
                                    let new_path =
                                        format!("soroban-sdk{}{r}", path::MAIN_SEPARATOR);
                                    files_map.insert(new_path, content);
                                }
                            } else {
                                files_map.insert(abs_path_str.to_string(), content);
                            }
                        }
                    } else {
                        files_map.insert(abs_path_str.to_string(), content);
                    }
                }
            }
        } else if path.is_dir() {
            collect_files_in_dir(&path, files_map)?;
        }
    }
    Ok(())
}

/// Detects the filesystem location of the `soroban_sdk` crate and returns a `HashMap`
/// of its absolute file paths to their contents.
///
/// Returns `None` if:
/// - The Cargo home directory cannot be determined.
/// - The Cargo registry source path does not exist.
/// - The `soroban_sdk` crate directory cannot be found within the registry.
///
/// Returns `Some(HashMap<String, String>)` on success, even if some individual files
/// could not be read (those will be skipped).
pub(crate) fn find_soroban_sdk_files() -> Option<HashMap<String, String>> {
    let cargo_home = if let Ok(path) = env::var("CARGO_HOME") {
        PathBuf::from(path)
    } else {
        // If CARGO_HOME is not set, default to the standard location based on OS.
        let home_dir = if cfg!(windows) {
            // On Windows, use USERPROFILE
            env::var_os("USERPROFILE").map(PathBuf::from)
        } else {
            // On Unix-like systems, use HOME
            env::var_os("HOME").map(PathBuf::from)
        };

        match home_dir {
            Some(mut path) => {
                path.push(".cargo");
                path
            }
            None => {
                return None;
            }
        }
    };

    let mut registry_src_path = cargo_home.clone();
    registry_src_path.push("registry");
    registry_src_path.push("src");

    if !registry_src_path.exists() {
        return None;
    }

    let mut latest_dirs: HashMap<String, (Version, PathBuf)> = HashMap::new();

    if let Ok(entries) = fs::read_dir(&registry_src_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if let Ok(inner_entries) = fs::read_dir(&path) {
                    for inner_entry in inner_entries.flatten().filter(|e| e.path().is_dir()) {
                        if let Some(dir_name) =
                            inner_entry.path().file_name().and_then(|n| n.to_str())
                        {
                            // Match "soroban-sdk-<semver>" or "soroban-sdk-macros-<semver>"
                            if let Some((prefix, version_str)) = dir_name.rsplit_once('-') {
                                if prefix == "soroban-sdk" || prefix == "soroban-sdk-macros" {
                                    if let Ok(ver) = Version::parse(version_str) {
                                        let entry = latest_dirs
                                            .entry(prefix.to_string())
                                            .or_insert((ver.clone(), inner_entry.path()));
                                        if ver > entry.0 {
                                            *entry = (ver, inner_entry.path());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        return None;
    }

    let soroban_sdk_dirs: Vec<PathBuf> = latest_dirs.into_values().map(|(_, path)| path).collect();

    let mut files_content_map = HashMap::new();
    for path in soroban_sdk_dirs {
        let _ = collect_files_in_dir(&path, &mut files_content_map);
    }
    Some(files_content_map)
}
