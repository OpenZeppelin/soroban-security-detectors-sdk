#![warn(clippy::pedantic)]
use core::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use macro_lib::node_location;
use quote::ToTokens;
use syn::{ItemFn, Type};

use super::node::{InnerStructIdentifier, Location, Node};
use super::node_type::{FunctionChildType, FunctionParentType, NodeType};

#[node_location(inner = "inner_struct")]
pub struct Function {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemFn>,
    pub parent: Rc<FunctionParentType>,
    pub children: Vec<Rc<FunctionChildType>>,
    pub parameters: Vec<Rc<FnParameter>>,
    // pub returns: FnReturns,
}

impl InnerStructIdentifier for ItemFn {
    fn identifier(&self) -> syn::Ident {
        self.sig.ident.clone()
    }
}

impl Node for Function {
    fn parent(&self) -> Option<Rc<NodeType>> {
        match self.parent.as_ref() {
            FunctionParentType::File(file) => Some(Rc::new(NodeType::File(file.clone()))),
            FunctionParentType::Contract(contract) => {
                Some(Rc::new(NodeType::Contract(contract.clone())))
            }
        }
    }

    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = Rc<FunctionChildType>> {
        self.children.clone().into_iter()
    }
}

impl Function {
    #[must_use]
    pub fn name(&self) -> String {
        self.inner_struct.sig.ident.to_string()
    }

    #[must_use]
    pub fn visibility(&self) -> syn::Visibility {
        self.inner_struct.vis.clone()
    }

    #[must_use]
    pub fn is_public(&self) -> bool {
        matches!(self.visibility(), syn::Visibility::Public(_))
    }

    #[must_use]
    pub fn is_private(&self) -> bool {
        !self.is_public()
    }
}

pub struct FnParameter {
    pub name: String,
    pub type_: Type,
    pub is_self: bool,
}

impl FnParameter {
    #[must_use]
    pub fn type_str(&self) -> String {
        self.type_.to_token_stream().to_string()
    }
}

impl Display for FnParameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.type_.to_token_stream())
    }
}

#[cfg(test)]
mod tests {
    use crate::function::FnParameter;
    use crate::node::{InnerStructIdentifier, Node};
    use crate::node_type::{FunctionChildType, FunctionParentType, NodeType};
    use crate::utils::test::{
        create_mock_contract, create_mock_file, create_mock_function_with_inner_item,
        create_mock_function_with_parameters, create_mock_function_with_parent,
    };
    use quote::ToTokens;
    use std::rc::Rc;
    use syn::{parse_quote, ItemFn, Visibility};

    #[test]
    fn test_function_name() {
        let item_fn: ItemFn = parse_quote! {
            pub fn test_function() {}
        };
        let function = create_mock_function_with_inner_item(1, item_fn);
        assert_eq!(function.name(), "test_function");
    }

    #[test]
    fn test_function_visibility_public() {
        let item_fn: ItemFn = parse_quote! {
            pub fn public_function() {}
        };
        let function = create_mock_function_with_inner_item(2, item_fn);

        match function.visibility() {
            Visibility::Public(_) => (),
            _ => panic!("Expected public visibility"),
        }
    }

    #[test]
    fn test_function_visibility_private() {
        let item_fn: ItemFn = parse_quote! {
            fn private_function() {}
        };
        let function = create_mock_function_with_inner_item(3, item_fn);

        match function.visibility() {
            Visibility::Inherited => (),
            _ => panic!("Expected inherited (private) visibility"),
        }
    }

    #[test]
    fn test_function_is_public() {
        let item_fn: ItemFn = parse_quote! {
            pub fn public_function() {}
        };
        let function = create_mock_function_with_inner_item(4, item_fn);

        assert!(function.is_public(), "Function should be public");
    }

    #[test]
    fn test_function_is_private() {
        let item_fn: ItemFn = parse_quote! {
            fn private_function() {}
        };
        let function = create_mock_function_with_inner_item(5, item_fn);

        assert!(function.is_private(), "Function should be private");
    }

    #[test]
    fn test_function_parent_file() {
        let item_fn: ItemFn = parse_quote! {
            pub fn file_parent_function() {}
        };
        let parent_file = create_mock_file();
        let rc_parent_file = Rc::new(parent_file);
        let function = create_mock_function_with_parent(
            5,
            item_fn,
            Rc::new(FunctionParentType::File(rc_parent_file.clone())),
        );

        let parent_node = function.parent().expect("Parent should exist");
        match &*parent_node {
            NodeType::File(file) => {
                assert_eq!(Rc::as_ptr(file), Rc::as_ptr(&rc_parent_file));
            }
            _ => panic!("Expected parent node to be of type File"),
        }
    }

    #[test]
    fn test_function_parent_contract() {
        let item_fn: ItemFn = parse_quote! {
            pub fn contract_parent_function() {}
        };
        let rc_contract_parent = Rc::new(create_mock_contract(1));
        let parent = FunctionParentType::Contract(rc_contract_parent.clone());
        let function = create_mock_function_with_parent(5, item_fn, Rc::new(parent));

        let parent_node = function.parent().expect("Parent should exist");
        match &*parent_node {
            NodeType::Contract(contract) => {
                assert_eq!(Rc::as_ptr(contract), Rc::as_ptr(&rc_contract_parent));
            }
            _ => panic!("Expected parent node to be of type Contract"),
        }
    }

    #[test]
    fn test_function_children_empty() {
        let item_fn: ItemFn = parse_quote! {
            pub fn function_with_no_children() {}
        };
        let function = create_mock_function_with_inner_item(6, item_fn);

        let children_iter: Vec<Rc<FunctionChildType>> = function.children().collect();
        assert!(
            children_iter.is_empty(),
            "Function should have no children initially"
        );
    }

    // #[test]
    // fn test_function_children_non_empty() {
    //     let item_fn: ItemFn = parse_quote! {
    //         pub fn function_with_children() {}
    //     };
    //     let function = Function {
    //         id: 7,
    //         inner_struct: Rc::new(item_fn),
    //         parent: Rc::new(FunctionParentType::File(Rc::new(create_mock_file()))),
    //         children: vec![
    //             Rc::new(FunctionChildType::Statement("let x = 10;".to_string())),
    //             Rc::new(FunctionChildType::Expression("x + 20".to_string())),
    //         ],
    //         parameters: vec![],
    //     };

    //     let children_iter: Vec<Rc<FunctionChildType>> = function.children().collect();
    //     assert_eq!(children_iter.len(), 2, "Function should have two children");
    //     assert_eq!(
    //         *children_iter[0],
    //         FunctionChildType::Statement("let x = 10;".to_string())
    //     );
    //     assert_eq!(
    //         *children_iter[1],
    //         FunctionChildType::Expression("x + 20".to_string())
    //     );
    // }

    #[test]
    fn test_inner_struct_identifier() {
        let item_fn: ItemFn = parse_quote! {
            fn identifier_test_function() {}
        };
        let function = create_mock_function_with_inner_item(1, item_fn.clone());
        let ident = function.inner_struct.identifier();
        assert_eq!(
            ident, item_fn.sig.ident,
            "Identifier should match the function's identifier"
        );
    }

    #[test]
    fn test_function_parameters() {
        let item_fn: ItemFn = parse_quote! {
            pub fn function_with_parameters(x: u32, y: String) {}
        };
        let parameters = vec![
            Rc::new(FnParameter {
                name: "x".to_string(),
                type_: parse_quote! { u32 },
                is_self: false,
            }),
            Rc::new(FnParameter {
                name: "y".to_string(),
                type_: parse_quote! { String },
                is_self: false,
            }),
        ];
        let function = create_mock_function_with_parameters(1, item_fn, parameters);

        let parameters = function.parameters;
        assert_eq!(parameters.len(), 2, "Function should have two parameters");

        let first_parameter = &parameters[0];
        assert_eq!(
            first_parameter.name, "x",
            "First parameter should be named 'x'"
        );
        assert_eq!(
            first_parameter.type_.to_token_stream().to_string(),
            "u32",
            "First parameter should be of type u32"
        );
        assert!(
            !first_parameter.is_self,
            "First parameter should not be self"
        );

        let second_parameter = &parameters[1];
        assert_eq!(
            second_parameter.name, "y",
            "Second parameter should be named 'y'"
        );
        assert_eq!(
            second_parameter.type_.to_token_stream().to_string(),
            "String",
            "Second parameter should be of type String"
        );
        assert!(
            !second_parameter.is_self,
            "Second parameter should not be self"
        );
    }

    #[test]
    fn test_fn_parameter_name() {
        let parameter = FnParameter {
            name: "x".to_string(),
            type_: parse_quote! { u32 },
            is_self: false,
        };
        assert_eq!(parameter.name, "x");
    }

    #[test]
    fn test_fn_parameter_is_self() {
        let mut parameter = FnParameter {
            name: "self".to_string(),
            type_: parse_quote! { u32 },
            is_self: true,
        };
        assert!(parameter.is_self);
        parameter.is_self = false;
        assert!(!parameter.is_self);
    }

    #[test]
    fn test_fn_parameter_type() {
        let parameter = FnParameter {
            name: "x".to_string(),
            type_: parse_quote! { u32 },
            is_self: false,
        };
        let type_: syn::Type = parse_quote! { u32 };
        assert_eq!(parameter.type_str(), type_.to_token_stream().to_string());
    }

    #[test]
    fn test_fn_parameter_type_str() {
        let parameter = FnParameter {
            name: "x".to_string(),
            type_: parse_quote! { u32 },
            is_self: false,
        };
        assert_eq!(parameter.type_str(), "u32");
    }

    #[test]
    fn test_fn_parameter_display() {
        let parameter = FnParameter {
            name: "x".to_string(),
            type_: parse_quote! { u32 },
            is_self: false,
        };
        assert_eq!(parameter.to_string(), "x: u32");
    }
}
