#![warn(clippy::pedantic)]
use super::node::{Location, Node, TLocation, Visibility};
use super::node_type::{FunctionChildType, TypeNode};
use core::fmt;
use quote::ToTokens;
use soroban_security_rules_macro_lib::node_location;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use syn::{ItemFn, Type};

type RcFnParameter = Rc<FnParameter>;

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Function {
    pub id: u128,
    pub location: Location,
    pub visibility: Visibility,
    pub name: String,
    pub children: RefCell<Vec<FunctionChildType>>,
    pub parameters: Vec<RcFnParameter>,
    pub returns: TypeNode,
}

impl Node for Function {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FunctionChildType> {
        self.children.borrow().clone().into_iter()
    }
}

impl Function {
    #[must_use]
    pub fn function_name_from_syn_fnitem(item: &ItemFn) -> String {
        item.sig.ident.to_string()
    }

    #[must_use]
    pub fn function_name_from_syn_impl_item(item: &syn::ImplItemFn) -> String {
        item.sig.ident.to_string()
    }

    #[must_use]
    pub fn visibility_from_syn_item(item: &ItemFn) -> Visibility {
        Visibility::from_syn_visibility(&item.vis)
    }

    #[must_use]
    pub fn visibility_from_syn_impl_item(item: &syn::ImplItemFn) -> Visibility {
        Visibility::from_syn_visibility(&item.vis)
    }

    #[must_use]
    pub fn is_public(&self) -> bool {
        matches!(self.visibility, Visibility::Public)
    }

    #[must_use]
    pub fn is_private(&self) -> bool {
        !self.is_public()
    }
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct FnParameter {
    pub name: String,
    pub type_name: String,
    pub location: Location,
    pub is_self: bool,
}

impl FnParameter {
    #[must_use]
    pub fn type_name_from_syn_item(type_: &Type) -> String {
        type_.to_token_stream().to_string()
    }
}

impl Display for FnParameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.type_name)
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::{Expression, FunctionCall};
    use crate::function::FnParameter;
    use crate::location;
    use crate::node::{Node, Visibility};
    use crate::node_type::{FunctionChildType, TypeNode};
    use crate::statement::Statement;
    use crate::utils::test::{create_mock_function, create_mock_function_with_parameters};
    use quote::ToTokens;
    use std::rc::Rc;
    use syn::parse_quote;
    use syn::{ExprCall, Type};

    #[test]
    fn test_function_name() {
        let function = create_mock_function(1);
        assert_eq!(function.name, "test_function");
    }

    #[test]
    fn test_function_visibility_public() {
        let function = create_mock_function(2);
        match function.visibility {
            Visibility::Public => (),
            _ => panic!("Expected public visibility"),
        }
    }

    #[test]
    fn test_function_visibility_private() {
        let mut function = create_mock_function(3);
        function.visibility = Visibility::Private;
        match function.visibility {
            Visibility::Private => (),
            _ => panic!("Expected inherited (private) visibility"),
        }
    }

    #[test]
    fn test_function_is_public() {
        let mut function = create_mock_function(4);
        function.visibility = Visibility::Public;
        assert!(function.is_public(), "Function should be public");
    }

    #[test]
    fn test_function_is_private() {
        let mut function = create_mock_function(5);
        function.visibility = Visibility::Private;
        assert!(function.is_private(), "Function should be private");
    }

    #[test]
    fn test_function_children_empty() {
        let function = create_mock_function(6);
        let children_iter: Vec<FunctionChildType> = function.children().collect();
        assert!(
            children_iter.is_empty(),
            "Function should have no children initially"
        );
    }

    #[allow(clippy::single_match, clippy::match_wildcard_for_single_variants)]
    #[test]
    fn test_function_children_non_empty() {
        let function_rc = Rc::new(create_mock_function(0));
        let expr_call_1: ExprCall = parse_quote! {
            execute("Hello, world!")
        };

        let stmt1 = FunctionCall {
            id: 1,
            location: location!(expr_call_1),
            function_name: FunctionCall::function_name_from_syn_item(&expr_call_1),
            children: vec![],
            is_tried: false,
        };

        let expr_call_2: ExprCall = parse_quote! {
            rise("Goodbye, world!")
        };

        let stmt2 = FunctionCall {
            id: 2,
            location: location!(expr_call_2),
            function_name: FunctionCall::function_name_from_syn_item(&expr_call_2),
            children: vec![],
            is_tried: false,
        };

        function_rc
            .children
            .borrow_mut()
            .push(FunctionChildType::Statement(Statement::Expression(
                Expression::FunctionCall(Rc::new(stmt1)),
            )));
        function_rc
            .children
            .borrow_mut()
            .push(FunctionChildType::Statement(Statement::Expression(
                Expression::FunctionCall(Rc::new(stmt2)),
            )));

        let children_iter: Vec<FunctionChildType> = function_rc.children().collect();
        assert_eq!(children_iter.len(), 2, "Function should have two children");
        match &children_iter[0] {
            FunctionChildType::Statement(stmt) => match stmt {
                Statement::Expression(Expression::FunctionCall(function_call)) => {
                    assert_eq!(function_call.id, 1);
                }
                _ => {}
            },
        }
        match &children_iter[1] {
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
        let t1: Type = parse_quote! { u32 };
        let t2: Type = parse_quote! { String };
        let parameters = vec![
            Rc::new(FnParameter {
                name: "x".to_string(),
                location: location!(t1),
                type_name: FnParameter::type_name_from_syn_item(&t1),
                is_self: false,
            }),
            Rc::new(FnParameter {
                name: "y".to_string(),
                location: location!(t2),
                type_name: FnParameter::type_name_from_syn_item(&t2),
                is_self: false,
            }),
        ];
        let function = create_mock_function_with_parameters(1, parameters);

        let parameters = function.parameters;
        assert_eq!(parameters.len(), 2, "Function should have two parameters");

        let first_parameter = &parameters[0];
        assert_eq!(
            first_parameter.name, "x",
            "First parameter should be named 'x'"
        );
        assert_eq!(
            first_parameter.type_name, "u32",
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
            second_parameter.type_name, "String",
            "Second parameter should be of type String"
        );
        assert!(
            !second_parameter.is_self,
            "Second parameter should not be self"
        );
    }

    #[test]
    fn test_fn_parameter_name() {
        let t: Type = parse_quote! { u32 };
        let parameter = FnParameter {
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
        };
        assert_eq!(parameter.name, "x");
    }

    #[test]
    fn test_fn_parameter_is_self() {
        let t: Type = parse_quote! { u32 };
        let mut parameter = FnParameter {
            name: "self".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: true,
        };
        assert!(parameter.is_self);
        parameter.is_self = false;
        assert!(!parameter.is_self);
    }

    #[test]
    fn test_fn_parameter_type() {
        let t: Type = parse_quote! { u32 };
        let parameter = FnParameter {
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
        };
        let type_: syn::Type = parse_quote! { u32 };
        assert_eq!(parameter.type_name, type_.to_token_stream().to_string());
    }

    #[test]
    fn test_fn_parameter_type_name() {
        let t: Type = parse_quote! { u32 };
        let parameter = FnParameter {
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
        };
        assert_eq!(parameter.type_name, "u32");
    }

    #[test]
    fn test_fn_parameter_display() {
        let t: Type = parse_quote! { u32 };
        let parameter = FnParameter {
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
        };
        assert_eq!(parameter.to_string(), "x: u32");
    }

    #[test]
    fn test_function_returns_none() {
        let function = create_mock_function(1);
        assert!(
            matches!(function.returns, TypeNode::Empty),
            "Function should have no return type"
        );
    }
}
