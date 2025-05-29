use crate::{ast_nodes, ast_nodes_impl};

use super::expression::Expression;
use super::misc::Misc;
use super::node::{Location, Node, Visibility};
use super::node_type::NodeKind;
use super::custom_type::Type;
use super::pattern::Pattern;
use super::statement::{Block, Statement};
use core::fmt;
use quote::ToTokens;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use syn::ItemFn;
use syn::Type as SynType;

type RcFnParameter = Rc<FnParameter>;

ast_nodes! {
    pub struct Function {
        /// Attributes on the function (e.g., `inline`, `no_mangle`)
        pub attributes: Vec<String>,
        pub visibility: Visibility,
        pub name: String,
        pub generics: Vec<String>,
        pub parameters: Vec<RcFnParameter>,
        pub body: Option<Rc<Block>>,
        pub returns: Type,
    }

    pub struct FnParameter {
        pub name: String,
        pub type_name: String,
        pub is_self: bool,
        pub is_mut: bool,
    }
}

ast_nodes_impl! {
    impl Node for Function {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            let parameters = self
                .parameters
                .iter()
                .map(|param| NodeKind::Misc(Misc::FnParameter((*param).clone())));
            let statements = self.body.as_ref().into_iter().flat_map(|body| {
                body.statements
                    .clone()
                    .into_iter()
                    .map(NodeKind::from)
            });
            let returns = std::iter::once(NodeKind::Type(self.returns.clone()));
            parameters
                .chain(statements)
                .chain(returns)
                .collect()
        }
    }
    impl Node for FnParameter {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
             vec![]
        }
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

    #[must_use = "Use this method to list function parameters"]
    #[allow(clippy::match_wildcard_for_single_variants)]
    pub fn parameters(&self) -> impl Iterator<Item = RcFnParameter> + '_ {
        self.parameters.iter().cloned()
    }

    /// Return the generic parameters of this function
    #[must_use = "Use this method to retrieve the generic parameters of the function"]
    pub fn generics(&self) -> impl Iterator<Item = String> + '_ {
        self.generics.clone().into_iter()
    }

    #[must_use = "Use this method to iterate over function body statements"]
    #[allow(clippy::match_wildcard_for_single_variants)]
    pub fn body(&self) -> Option<Rc<Block>> {
        self.body.clone()
    }

    #[must_use = "Use this method to check if function is public"]
    pub fn is_public(&self) -> bool {
        matches!(self.visibility, Visibility::Public)
    }

    #[must_use = "Use this method to check if function is private"]
    pub fn is_private(&self) -> bool {
        !self.is_public()
    }

    #[must_use = "Use this method to check if function is actually a method for an implemented type"]
    pub fn is_method(&self) -> bool {
        self.parameters.iter().any(|parameter| parameter.is_self)
    }

    #[must_use = "Use this method to check if function will panic"]
    pub fn will_panic(&self) -> bool {
        for stmt in self.body.iter().flat_map(|b| &b.statements) {
            println!("Checking statement: {stmt:?}");
            match stmt {
                Statement::Macro(macro_stmt) => {
                    if macro_stmt.name == "panic" {
                        return true;
                    }
                }
                Statement::Expression(Expression::MemberAccess(member_access)) => {
                    if ["unwrap", "expect"].contains(&member_access.member_name.as_str()) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }
}

impl FnParameter {
    #[must_use]
    pub fn type_name_from_syn_item(type_: &SynType) -> String {
        type_.to_token_stream().to_string().replace(' ', "")
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
    use crate::function::{FnParameter, Function, RcFnParameter};
    use crate::location;
    use crate::node::{Location, Node, Visibility};
    use crate::node_type::NodeKind;
    use crate::ast::custom_type::Type;
    use crate::statement::{Block, Statement};
    use crate::utils::test::{create_mock_function, create_mock_function_with_parameters};
    use quote::ToTokens;
    use std::rc::Rc;
    use syn::{parse_quote, ItemFn};
    use syn::ExprCall;

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
        let children_iter: Vec<Rc<NodeKind>> =
            function.children().into_iter().map(Rc::from).collect();
        assert_eq!(
            children_iter.len(),
            1,
            "Function should have no children initially"
        );
    }

    #[allow(clippy::single_match, clippy::match_wildcard_for_single_variants)]
    #[test]
    fn test_function_children_non_empty() {
        let mut function_rc = create_mock_function(0);
        let expr_call_1: ExprCall = parse_quote! {
            execute("Hello, world!")
        };

        let stmt1 = FunctionCall {
            id: 1,
            location: Location::default(),
            function_name: FunctionCall::function_name_from_syn_item(&expr_call_1),
            parameters: vec![],
        };

        let expr_call_2: ExprCall = parse_quote! {
            rise("Goodbye, world!")
        };

        let stmt2 = FunctionCall {
            id: 2,
            location: Location::default(),
            function_name: FunctionCall::function_name_from_syn_item(&expr_call_2),
            parameters: vec![],
        };
        let body = Rc::new(Block {
            id: 1,
            location: Location::default(),
            statements: vec![
                Statement::Expression(Expression::FunctionCall(Rc::new(stmt1))),
                Statement::Expression(Expression::FunctionCall(Rc::new(stmt2))),
            ],
        });
        function_rc.body = Some(body);
        let children_iter: Vec<Rc<NodeKind>> =
            function_rc.children().into_iter().map(Rc::from).collect();
        assert_eq!(
            children_iter.len(),
            3,
            "Function should have three children"
        );
        match &*children_iter[0] {
            NodeKind::Statement(Statement::Expression(Expression::FunctionCall(function_call))) => {
                assert_eq!(function_call.id, 1);
            }
            _ => {}
        }
        match &*children_iter[1] {
            NodeKind::Statement(Statement::Expression(Expression::FunctionCall(function_call))) => {
                assert_eq!(function_call.id, 2);
            }
            _ => {}
        }
    }

    #[test]
    fn test_function_parameters() {
        let t1: syn::Type = parse_quote! { u32 };
        let t2: syn::Type = parse_quote! { String };
        let parameters = [
            Rc::new(FnParameter {
                id: 1,
                name: "x".to_string(),
                location: location!(t1),
                type_name: FnParameter::type_name_from_syn_item(&t1),
                is_self: false,
                is_mut: false,
            }),
            Rc::new(FnParameter {
                id: 2,
                name: "y".to_string(),
                location: location!(t2),
                type_name: FnParameter::type_name_from_syn_item(&t2),
                is_self: false,
                is_mut: false,
            }),
        ];
        let function = create_mock_function_with_parameters(1, &parameters);

        let parameters = function.parameters().collect::<Vec<_>>();
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
        let t: syn::Type = parse_quote! { u32 };
        let parameter = FnParameter {
            id: 1,
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
            is_mut: false,
        };
        assert_eq!(parameter.name, "x");
    }

    #[test]
    fn test_fn_parameter_is_self() {
        let t: syn::Type = parse_quote! { u32 };
        let mut parameter = FnParameter {
            id: 1,
            name: "self".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: true,
            is_mut: false,
        };
        assert!(parameter.is_self);
        parameter.is_self = false;
        assert!(!parameter.is_self);
    }

    #[test]
    fn test_fn_parameter_type() {
        let t: syn::Type = parse_quote! { u32 };
        let parameter = FnParameter {
            id: 1,
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
            is_mut: false,
        };
        let type_: syn::Type = parse_quote! { u32 };
        assert_eq!(parameter.type_name, type_.to_token_stream().to_string());
    }

    #[test]
    fn test_fn_parameter_type_name() {
        let t: syn::Type = parse_quote! { u32 };
        let parameter = FnParameter {
            id: 1,
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
            is_mut: false,
        };
        assert_eq!(parameter.type_name, "u32");
    }

    #[test]
    fn test_fn_parameter_display() {
        let t: syn::Type = parse_quote! { u32 };
        let parameter = FnParameter {
            id: 1,
            name: "x".to_string(),
            location: location!(t),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
            is_mut: false,
        };
        assert_eq!(parameter.to_string(), "x: u32");
    }

    #[test]
    fn test_function_returns_none() {
        let function = create_mock_function(1);
        assert!(
            matches!(function.returns, Type::Typename(_)),
            "Function should have default unit return type"
        );
    }

    #[test]
    fn test_function_name_from_syn_fnitem() {
        let item: ItemFn = parse_quote! {
            fn test_function() {}
        };
        assert_eq!(
            Function::function_name_from_syn_fnitem(&item),
            "test_function"
        );
    }

    #[test]
    fn test_function_name_from_syn_impl_item() {
        let item: syn::ImplItemFn = parse_quote! {
            fn test_function() {}
        };
        assert_eq!(
            Function::function_name_from_syn_impl_item(&item),
            "test_function"
        );
    }

    #[test]
    fn test_visibility_from_syn_item() {
        let item: ItemFn = parse_quote! {
            pub fn test_function() {}
        };
        assert!(matches!(
            Function::visibility_from_syn_item(&item),
            Visibility::Public
        ));
    }

    #[test]
    fn test_visibility_from_syn_impl_item() {
        let item: syn::ImplItemFn = parse_quote! {
            pub fn test_function() {}
        };
        assert!(matches!(
            Function::visibility_from_syn_impl_item(&item),
            Visibility::Public
        ));
    }

    #[test]
    fn test_fn_parameter_type_name_from_syn_item() {
        let t: syn::Type = parse_quote! { u32 };
        assert_eq!(FnParameter::type_name_from_syn_item(&t), "u32");
    }

    #[test]
    fn test_function_body_none() {
        let function = create_mock_function(1);
        assert!(function.body().is_none(), "Function body should be None");
    }

    #[test]
    fn test_function_body_some() {
        let mut function = create_mock_function(1);
        let body = Rc::new(Block {
            id: 1,
            location: Location::default(),
            statements: vec![],
        });
        function.body = Some(body.clone());
        assert!(
            function
                .body()
                .as_ref()
                .is_some_and(|b| Rc::ptr_eq(b, &body)),
            "Function body should be Some"
        );
    }

    #[test]
    fn test_function_parameters_empty() {
        let function = create_mock_function(1);
        let parameters: Vec<RcFnParameter> = function.parameters().collect();
        assert!(parameters.is_empty(), "Function should have no parameters");
    }

    #[test]
    fn test_function_parameters_non_empty() {
        let t: syn::Type = parse_quote! { u32 };
        let parameter = Rc::new(FnParameter {
            id: 1,
            name: "x".to_string(),
            location: Location::default(),
            type_name: FnParameter::type_name_from_syn_item(&t),
            is_self: false,
            is_mut: false,
        });
        let function = create_mock_function_with_parameters(1, &[parameter.clone()]);
        let parameters: Vec<RcFnParameter> = function.parameters().collect();
        assert_eq!(parameters.len(), 1, "Function should have one parameter");
        assert_eq!(parameters[0].name, "x", "Parameter name should be 'x'");
    }
    #[test]
    fn test_function_generics() {
        use syn::parse_quote;
        // define a function with lifetimes and type parameters
        let item_fn: syn::ItemFn = parse_quote! {
            pub fn foo<'a, T: Clone, U>(x: T) -> U { unimplemented!() }
        };
        let mut codebase = crate::Codebase::<crate::OpenState>::default();
        let function =
            crate::ast_types_builder::build_function_from_item_fn(&mut codebase, &item_fn, 0);
        let gens: Vec<String> = function.generics().collect();
        assert_eq!(
            gens,
            vec!["'a".to_string(), "T : Clone".to_string(), "U".to_string()]
        );
    }
}
