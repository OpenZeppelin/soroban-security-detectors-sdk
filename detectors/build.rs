use serde_yaml::Value;
use std::env;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;

const EXPLICITLY_NOT_DETECTORS_FILE_NAMES: &[&str] = &["lib.rs", "utils.rs"];
const DO_NOT_INCLUDE_MARKER: &str = "// do-not-include-in-build";

fn detector_is_skipped(detector_rs_path: &Path) -> bool {
    if let Ok(file) = File::open(detector_rs_path) {
        let mut reader = BufReader::new(file);
        let mut first_line = String::new();
        if reader.read_line(&mut first_line).is_ok() {
            return first_line.trim_start().starts_with(DO_NOT_INCLUDE_MARKER);
        }
    }
    false
}

fn main() {
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let mod_file_path = Path::new(&out_dir).join("mod_includes.rs");
    let register_path = Path::new(&out_dir).join("register.rs");
    let template_path = Path::new(&out_dir).join("detector_report_templates.rs");

    let src_dir = Path::new("src");
    let metadata_dir = Path::new("metadata");

    let mut mods = String::new();
    let mut detector_type_names = Vec::new();
    let mut templates =
        String::from("use soroban_security_detectors_sdk::detector::DetectorReportTemplate;\n");

    let mut metadata_map = std::collections::HashMap::new();
    if metadata_dir.exists() && metadata_dir.is_dir() {
        for entry in fs::read_dir(metadata_dir).expect("Failed to read metadata directory") {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("yml") {
                let content = fs::read_to_string(&path)
                    .unwrap_or_else(|_| panic!("Failed to read file {}", path.display()));
                let yaml: Value = serde_yaml::from_str(&content)
                    .unwrap_or_else(|_| panic!("Failed to parse YAML in file {}", path.display()));
                if let Some(id) = yaml["metadata"]["id"].as_str() {
                    metadata_map.insert(id.to_string(), yaml);
                }
            }
        }
    }

    for entry in fs::read_dir(src_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().unwrap() != "rs" {
            continue;
        }
        let file_name = path.file_name().unwrap().to_str().unwrap();
        if EXPLICITLY_NOT_DETECTORS_FILE_NAMES.contains(&file_name) {
            continue;
        }

        if detector_is_skipped(&path) {
            println!("cargo:warning=Skipping detector {file_name} due to skip marker.",);
            continue;
        }

        let mod_name = file_name.trim_end_matches(".rs");
        let type_name = to_type_name(mod_name);

        mods.push_str(&format!(
            "pub mod {mod_name} {{ include!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/src/{mod_name}.rs\")); }}\n",
        ));
        mods.push_str(&format!("pub use {mod_name}::{type_name};\n"));
        detector_type_names.push(type_name.clone());

        let id_dash = mod_name.replace("_", "-");
        let id_underscore = mod_name.replace("-", "_");
        let yaml = metadata_map
            .get(&id_dash)
            .or_else(|| metadata_map.get(&id_underscore));
        if let Some(yaml) = yaml {
            let metadata = &yaml["metadata"];
            let id = metadata["id"].as_str().unwrap();
            let uid = metadata["uid"].as_str().unwrap();
            let description = metadata["description"].as_str().unwrap_or("");
            let report = &metadata["report"];
            let severity = report["severity"].as_str().unwrap_or("note");
            let tags = report["tags"]
                .as_sequence()
                .unwrap_or(&Vec::new())
                .iter()
                .filter_map(|tag| tag.as_str().map(|s| format!("\"{s}\".to_string()")))
                .collect::<Vec<_>>()
                .join(",");
            let template = &metadata["report"]["template"];
            let template_yaml =
                serde_yaml::to_string(&template).unwrap_or_else(|_| String::from("{}"));
            let title = template["title"].as_str().unwrap_or_default();
            let opening = template["body-list-item-intro"]
                .as_str()
                .unwrap_or_default();
            let body_list_item = template["body-list-item"].as_str().unwrap_or_default();
            let closing = template["closing"].as_str().unwrap_or_default();
            let type_def = format!(
                r#"
#[allow(clippy::manual_string_new)]
impl DetectorReportTemplate for {type_name} {{
    fn id(&self) -> String {{ "{id}".to_string() }}
    fn uid(&self) -> String {{ "{uid}".to_string() }}
    fn description(&self) -> String {{ "{description}".to_string() }}
    fn severity(&self) -> String {{ "{severity}".to_string() }}
    fn tags(&self) -> Vec<String> {{ vec![{tags}] }}
    fn title_single_instance(&self) -> String {{ "{title}".to_string() }}
    fn title_multiple_instance(&self) -> String {{ "{title}".to_string() }}
    fn opening(&self) -> String {{ "{opening}".to_string() }}
    fn body_single_file_single_instance(&self) -> String {{ String::new() }}
    fn body_single_file_multiple_instance(&self) -> String {{ String::new() }}
    fn body_multiple_file_multiple_instance(&self) -> String {{ String::new() }}
    fn body_list_item_single_file(&self) -> String {{ "{body_list_item}".to_string() }}
    fn body_list_item_multiple_file(&self) -> String {{ "{body_list_item}".to_string() }}
    fn closing(&self) -> String {{ "{closing}".to_string() }}
    fn template(&self) -> String {{ "{template_yaml}".to_string() }}
}}
"#,
                type_name = type_name,
                id = escape_rust_string(id),
                uid = escape_rust_string(uid),
                description = escape_rust_string(description),
                severity = escape_rust_string(severity),
                tags = tags,
                title = escape_rust_string(title),
                opening = escape_rust_string(opening),
                body_list_item = escape_rust_string(body_list_item),
                closing = escape_rust_string(closing),
                template_yaml = escape_rust_string(&template_yaml),
            );
            templates.push_str(&type_def);
        }
    }

    fs::write(&mod_file_path, mods).unwrap();
    let mut register_code = String::from(
        "#[must_use] pub fn all_detectors() -> Vec<soroban_security_detectors_sdk::detector::SorobanDetector<soroban_security_detectors_sdk::detector::SealedCodebase>> {\n    vec![\n",
    );
    for type_name in &detector_type_names {
        register_code.push_str(&format!("        Box::new({type_name}),\n",));
    }
    register_code.push_str("    ]\n}\n");
    fs::write(&register_path, register_code).unwrap();
    fs::write(&template_path, templates).unwrap();
}

fn to_type_name(id: &str) -> String {
    id.split(|c: char| !c.is_alphanumeric())
        .filter(|s| !s.is_empty())
        .map(|s| {
            let mut chars = s.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                #[allow(non_snake_case)]
                None => String::new(),
            }
        })
        .collect::<String>()
}

fn escape_rust_string(s: &str) -> String {
    s.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "")
}
