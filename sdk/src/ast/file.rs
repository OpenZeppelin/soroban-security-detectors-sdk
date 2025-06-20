use std::{cell::RefCell, path, rc::Rc};

use crate::{
    ast_node_impl,
    directive::{Directive, Use},
    node::Location,
};

use super::{node::Node, node_type::NodeKind};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct File {
    pub id: u32,
    pub children: RefCell<Vec<NodeKind>>,
    pub name: String,
    pub path: String,
    pub attributes: Vec<String>,
    pub source_code: String,
    pub location: Location,
}

ast_node_impl! {
    impl Node for File {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.children.borrow().clone()
        }
    }
}
impl File {
    #[must_use]
    pub fn has_no_std(&self) -> bool {
        self.attributes.contains(&"no_std".to_string())
    }

    #[must_use]
    pub fn attributes_from_file_item(file: &syn::File) -> Vec<String> {
        file.attrs
            .iter()
            .map(|attr| attr.path().segments[0].ident.to_string())
            .collect()
    }

    #[must_use]
    pub fn file_module_name(&self) -> String {
        let res = self
            .path
            .split(path::MAIN_SEPARATOR)
            .collect::<Vec<_>>()
            .join("::")
            .replace(".rs", "")
            .replace('-', "_");
        let res = if let Some(stripped) = res.strip_prefix("::") {
            stripped.to_string()
        } else {
            res
        };
        res

        // if res.contains("soroban_sdk_macros") || res.contains("soroban_sdk") {
        //     if let Some(suffix_str) = res.split("soroban_sdk_macros").last() {
        //         if let Some(suffix) = suffix_str.split("::src::").last() {
        //             if suffix.is_empty() || suffix == "lib" {
        //                 "soroban_sdk_macros".to_string()
        //             } else {
        //                 format!("soroban_sdk_macros::{suffix}")
        //             }
        //         } else {
        //             res.clone()
        //         }
        //     } else if let Some(suffix_str) = res.split("soroban_sdk").last() {
        //         if let Some(suffix) = suffix_str.split("::src::").last() {
        //             if suffix.is_empty() || suffix == "lib" {
        //                 "soroban_sdk".to_string()
        //             } else {
        //                 format!("soroban_sdk::{suffix}")
        //             }
        //         } else {
        //             res.clone()
        //         }
        //     } else {
        //         res.clone()
        //     }
        // } else {
        //     res
        // }
    }

    #[must_use]
    pub fn is_soroban_sdk_file(&self) -> bool {
        self.path.contains("soroban-sdk")
            || self.path.contains("soroban-sdk-macros")
            || self.path.contains("soroban_security_detectors_sdk")
    }

    pub fn imports(&self) -> Vec<Rc<Use>> {
        self.children
            .borrow()
            .iter()
            .filter_map(|child| {
                if let NodeKind::Directive(Directive::Use(use_node)) = child {
                    Some(use_node.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::test::{create_mock_file, create_mock_file_with_inner_struct};

    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_children() {
        let file = create_mock_file();
        let children = file.children();
        assert!(children.is_empty(), "File node should have no children");
    }

    #[test]
    fn test_file_has_no_std() {
        let source = "#![no_std]\nfn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(&parsed_file);
        assert!(file.has_no_std(), "File should have no_std attribute");

        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(&parsed_file);
        assert!(!file.has_no_std(), "File should not have no_std attribute");
    }
}
