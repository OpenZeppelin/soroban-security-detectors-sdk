//! Resolver for the Soroban SDK crate source files.
//!
//! Locates the installed `soroban_sdk` crate within the local Cargo registry
//! and returns its source file paths and versions for analysis.
use semver::Version;

use std::{collections::HashMap, env, fs, path::PathBuf};

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
pub(crate) fn find_soroban_sdk_files() -> Option<HashMap<String, (Version, PathBuf)>> {
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

    Some(latest_dirs)
}
