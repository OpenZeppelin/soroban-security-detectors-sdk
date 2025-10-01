#![warn(clippy::pedantic)]
//! Command-line runner for Soroban security detectors.
//!
//! Parses CLI arguments, loads detectors, and executes them against a
//! Soroban contract codebase, reporting findings in JSON format.
use clap::Parser;
use libloading::{Library, Symbol};
use parser::Cli;
use serde_json::{json, Map};
use soroban_security_detectors::all_detectors;
use soroban_security_detectors_sdk::{
    build_codebase, detector::DetectorResult, SealedCodebase, SorobanDetector,
};
use std::{collections::HashMap, path::PathBuf};

mod parser;

fn main() {
    let args = Cli::parse();

    match args.command {
        parser::Commands::Scan {
            code,
            detectors,
            project_root,
            load_lib,
        } => {
            let mut corpus = HashMap::new();
            for path in &code {
                if path.is_dir() {
                    let mut stack = vec![path.clone()];
                    while let Some(current_path) = stack.pop() {
                        for entry in std::fs::read_dir(current_path).unwrap() {
                            let entry = entry.unwrap();
                            let p = entry.path();
                            if p.is_dir() {
                                stack.push(p);
                            } else if p.is_file() && p.extension().unwrap_or_default() == "rs" {
                                let file_content = std::fs::read_to_string(&p).unwrap();
                                corpus.insert(p.to_string_lossy().to_string(), file_content);
                            }
                        }
                    }
                } else if path.is_file() {
                    if path.extension().unwrap_or_default() != "rs" {
                        continue;
                    }
                    let file_content = std::fs::read_to_string(path).unwrap();
                    corpus.insert(path.to_string_lossy().to_string(), file_content);
                }
            }
            let mut files_scanned = Vec::new();
            let mut detector_responses = Map::new();
            if !corpus.is_empty() {
                let result = execute_detectors(&corpus, detectors.as_ref(), load_lib);

                files_scanned = corpus
                    .keys()
                    .map(|k| relative_file_path(k, project_root.as_ref()))
                    .collect();

                for (detector_name, errors) in result {
                    let instances = detector_result_to_json(errors, project_root.as_ref());

                    let detector_response = json!({
                        "findings": [
                            {
                                "instances": instances
                            }
                        ],
                        "errors": [],
                        "metadata": {}
                    });
                    detector_responses.insert(detector_name, detector_response);
                }
            }
            let res = json!({
                "errors": [],
                "scanned": files_scanned,
                "detector_responses": detector_responses,
            });

            println!("{}", serde_json::to_string_pretty(&res).unwrap());
        }
        parser::Commands::Metadata => {
            println!("{}", get_scanner_metadata());
        }
    }
}

fn execute_detectors(
    files: &HashMap<String, String>,
    rules: Option<&Vec<String>>,
    load_lib: Option<std::path::PathBuf>,
) -> HashMap<String, Vec<DetectorResult>> {
    let codebase = build_codebase(files).unwrap();
    let mut results = HashMap::new();
    if let Some(load_lib) = load_lib {
        unsafe {
            let lib = Library::new(load_lib).unwrap();
            let constructor: Symbol<unsafe extern "C" fn() -> SorobanDetector<SealedCodebase>> =
                lib.get(b"external_detector").unwrap();
            let detector = constructor();
            let detector_result = detector.check(codebase.as_ref());
            if let Some(errors) = detector_result {
                results.insert(detector.id(), errors);
            }
        }
    }
    let selected_detectors: Vec<_> = available_detectors()
        .into_iter()
        .filter(|detector| {
            if let Some(rules) = rules {
                rules.contains(&detector.id())
                    || (rules.len() == 1 && rules[0].eq_ignore_ascii_case("all"))
            } else {
                true
            }
        })
        .collect();

    for detector in selected_detectors {
        let detector_result = detector.check(codebase.as_ref());
        if let Some(errors) = detector_result {
            results.insert(detector.id(), errors);
        }
    }
    results
}

fn detector_result_to_json(
    errors: Vec<DetectorResult>,
    project_root: Option<&PathBuf>,
) -> serde_json::Value {
    let mut json_errors = Vec::new();
    for error in errors {
        let path = relative_file_path(&error.file_path, project_root);

        let json_error = json!({
            "path": path,
            "offset_start": error.offset_start,
            "offset_end": error.offset_end,
            "fixes": [],
            "extra": {"metavars": error.extra},
        });
        json_errors.push(json_error);
    }
    json!(json_errors)
}

fn relative_file_path(file_path: &str, project_root: Option<&PathBuf>) -> String {
    if let Some(root) = project_root {
        if let Ok(relative_path) = std::path::Path::new(file_path).strip_prefix(root) {
            relative_path.to_string_lossy().to_string()
        } else {
            file_path.to_string()
        }
    } else {
        file_path.to_string()
    }
}

fn available_detectors() -> Vec<SorobanDetector<SealedCodebase>> {
    all_detectors()
        .into_iter()
        .chain(custom_detectors())
        .collect()
}

#[allow(clippy::let_and_return, unused_mut)]
fn custom_detectors() -> Vec<SorobanDetector<SealedCodebase>> {
    let mut detectors: Vec<SorobanDetector<SealedCodebase>> = Vec::new();
    detectors
        .into_iter()
        .map(|detector| detector as SorobanDetector<SealedCodebase>)
        .collect()
}

fn get_scanner_metadata() -> String {
    let version = env!("CARGO_PKG_VERSION");
    let org = "OpenZeppelin";
    let description = "Static analyzer for Stellar Soroban source code files";
    let mut detectors = Vec::new();
    for detector in available_detectors() {
        let json_detector = json!({
            "id": detector.id(),
            "uid": detector.uid(),
            "description": detector.description(),
            "report": {
                "severity": detector.severity(),
                "tags": detector.tags(),
                "template": yml_string_to_json(&detector.template())
            }
        });
        detectors.push(json_detector);
    }
    let scanner_json = json!({
        "name": "soroban-scanner",
        "description": description,
        "version": version,
        "org": org,
        "extensions": [".rs"],
        "detectors": detectors
    });
    serde_json::to_string_pretty(&scanner_json).unwrap()
}

fn yml_string_to_json(yml_string: &str) -> Option<serde_json::Value> {
    serde_yaml::from_str::<serde_json::Value>(yml_string).ok()
}
