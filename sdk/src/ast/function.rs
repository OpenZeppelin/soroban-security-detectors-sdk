#![warn(clippy::pedantic)]
use super::node::{Location, Node};
use super::node_type::{FunctionChildType, FunctionParentType, NodeType};
use core::fmt;
use quote::ToTokens;
use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use syn::spanned::Spanned;
use syn::{ItemFn, Type};

#[node_location(inner = "inner_struct")]
pub struct Function {
    pub id: usize,
    pub(crate) inner_struct: Rc<ItemFn>,
    pub parent: Rc<FunctionParentType>,
    pub children: RefCell<Vec<Rc<FunctionChildType>>>,
    pub parameters: Vec<Rc<FnParameter>>,
    pub returns: Option<Type>,
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
        self.children.borrow().clone().into_iter()
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
    use crate::expression::{Expression, FunctionCall};
    use crate::function::FnParameter;
    use crate::node::Node;
    use crate::node_type::{
        FunctionCallParentType, FunctionChildType, FunctionParentType, NodeType,
    };
    use crate::statement::Statement;
    use crate::utils::test::{
        create_mock_contract, create_mock_file, create_mock_function,
        create_mock_function_with_inner_item, create_mock_function_with_parameters,
        create_mock_function_with_parent,
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

    #[test]
    fn test_function_children_non_empty() {
        let function_rc = Rc::new(create_mock_function(0));

        let expr_call_1 = parse_quote! {
            execute("Hello, world!")
        };

        let stmt1 = FunctionCall {
            id: 1,
            inner_struct: Rc::new(expr_call_1),
            parent: Rc::new(FunctionCallParentType::Function(function_rc.clone())),
            children: vec![],
            is_tried: false,
        };

        let expr_call_2 = parse_quote! {
            rise("Goodbye, world!")
        };

        let stmt2 = FunctionCall {
            id: 2,
            inner_struct: Rc::new(expr_call_2),
            parent: Rc::new(FunctionCallParentType::Function(function_rc.clone())),
            children: vec![],
            is_tried: false,
        };

        function_rc
            .children
            .borrow_mut()
            .push(Rc::new(FunctionChildType::Statement(
                Statement::Expression(Expression::FunctionCall(stmt1)),
            )));
        function_rc
            .children
            .borrow_mut()
            .push(Rc::new(FunctionChildType::Statement(
                Statement::Expression(Expression::FunctionCall(stmt2)),
            )));

        let children_iter: Vec<Rc<FunctionChildType>> = function_rc.children().collect();
        assert_eq!(children_iter.len(), 2, "Function should have two children");
        match children_iter[0].as_ref() {
            FunctionChildType::Statement(stmt) => match stmt {
                Statement::Expression(Expression::FunctionCall(function_call)) => {
                    assert_eq!(function_call.id, 1);
                }
                _ => {}
            },
        }
        match children_iter[1].as_ref() {
            FunctionChildType::Statement(stmt) => match stmt {
                Statement::Expression(Expression::FunctionCall(function_call)) => {
                    assert_eq!(function_call.id, 2);
                }
                _ => {}
            },
        }
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

    #[test]
    fn test_function_returns_none() {
        let item_fn: ItemFn = parse_quote! {
            pub fn function_with_no_returns() {}
        };
        let function = create_mock_function_with_inner_item(1, item_fn);
        assert!(
            function.returns.is_none(),
            "Function should have no return type"
        );
    }

    #[test]
    fn test_function_returns_some() {
        let item_fn: ItemFn = parse_quote! {
            pub fn function_with_returns() -> u32 {}
        };
        let mut function = create_mock_function_with_inner_item(2, item_fn);
        function.returns = Some(parse_quote! { u32 });
        assert!(
            function.returns.is_some(),
            "Function should have a return type"
        );
    }
}
