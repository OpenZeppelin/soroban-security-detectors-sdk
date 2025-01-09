#![warn(clippy::pedantic)]
use super::node::Node;
use super::node_type::{FileChildType, NodeType};
use std::rc::Rc;

pub struct File {
    pub id: usize,
    pub(crate) inner_struct: Rc<syn::File>,
    pub children: Vec<FileChildType>,

    pub name: String,
    pub path: String,
}

// The comment code below is the implementation of `Serialize` and `Deserialize` for `File`
// In order to enable serialization and deserialization for the `Codebase`, implement these for all types where required
// impl Serialize for File {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: serde::Serializer,
//     {
//         let mut state = serializer.serialize_struct("File", 5)?;
//         state.serialize_field("id", &self.id)?;
//         let inner_as_string = self.inner_struct.span().source_text().unwrap().to_string();
//         state.serialize_field("inner_struct", &inner_as_string)?;
//         state.serialize_field("name", &self.name)?;
//         state.serialize_field("path", &self.path)?;
//         state.serialize_field("children", &self.children)?;
//         state.end()
//     }
// }

// impl<'de> Deserialize<'de> for File {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         struct FileVisitor;

//         impl<'de> Visitor<'de> for FileVisitor {
//             type Value = File;

//             fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//                 formatter.write_str("a File struct with id, inner_struct, children, name, and path")
//             }

//             fn visit_map<V>(self, mut map: V) -> Result<File, V::Error>
//             where
//                 V: MapAccess<'de>,
//             {
//                 let mut id = None;
//                 let mut inner_struct = None;
//                 let mut children = None;
//                 let mut name = None;
//                 let mut path = None;

//                 while let Some(key) = map.next_key::<String>()? {
//                     match key.as_str() {
//                         "id" => id = Some(map.next_value()?),
//                         "inner_struct" => {
//                             let inner_str: String = map.next_value()?;
//                             inner_struct =
//                                 Some(syn::parse_str(&inner_str).map_err(|_| {
//                                     de::Error::custom("Failed to parse inner_struct")
//                                 })?);
//                         }
//                         "children" => children = Some(map.next_value()?),
//                         "name" => name = Some(map.next_value()?),
//                         "path" => path = Some(map.next_value()?),
//                         _ => return Err(de::Error::unknown_field(&key, FIELDS)),
//                     }
//                 }

//                 let id = id.ok_or_else(|| de::Error::missing_field("id"))?;
//                 let inner_struct =
//                     Rc::new(inner_struct.ok_or_else(|| de::Error::missing_field("inner_struct"))?);
//                 let children = children.ok_or_else(|| de::Error::missing_field("children"))?;
//                 let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
//                 let path = path.ok_or_else(|| de::Error::missing_field("path"))?;

//                 Ok(File {
//                     id,
//                     inner_struct,
//                     children,
//                     name,
//                     path,
//                 })
//             }
//         }

//         const FIELDS: &[&str] = &["id", "inner_struct", "children", "name", "path"];
//         deserializer.deserialize_struct("File", FIELDS, FileVisitor)
//     }
// }

impl Node for File {
    fn parent(&self) -> Option<NodeType> {
        None
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FileChildType> {
        self.children.iter().cloned()
    }
}

impl File {
    #[must_use]
    pub fn name(&self) -> String {
        self.name.clone()
    }

    #[must_use]
    pub fn has_no_std(&self) -> bool {
        self.inner_struct
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("no_std"))
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::test::{create_mock_file, create_mock_file_with_inner_struct};

    use super::*;
    use syn::parse_file;

    #[test]
    fn test_file_as_node_parent() {
        let file = create_mock_file();
        assert!(file.parent().is_none(), "File node should have no parent");
    }

    #[test]
    fn test_file_as_node_children() {
        let file = create_mock_file();
        let mut children = file.children();
        assert!(
            children.next().is_none(),
            "File node should have no children"
        );
    }

    #[test]
    fn test_file_has_no_std() {
        let source = "#![no_std]\nfn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(parsed_file.clone());
        assert!(file.has_no_std(), "File should have no_std attribute");

        let source = "fn main() {}";
        let parsed_file: syn::File = parse_file(source).expect("Failed to parse file");
        let file = create_mock_file_with_inner_struct(parsed_file.clone());
        assert!(!file.has_no_std(), "File should not have no_std attribute");
    }
}
