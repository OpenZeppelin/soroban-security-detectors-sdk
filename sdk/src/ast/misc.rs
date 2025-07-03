//! Miscellaneous AST node types.
//!
//! Defines the `Misc` enum and common node structs for struct fields,
//! macro invocations, and function parameters.
use std::rc::Rc;

use crate::{ast_enum, ast_nodes, ast_nodes_impl};

use super::node::Node;
use super::{
    custom_type::Type,
    function::FnParameter,
    node::{Location, Mutability, Visibility},
    node_type::NodeKind,
};

ast_enum! {
    pub enum Misc {
        Field(Rc<Field>),
        Macro(Rc<Macro>),
        FnParameter(Rc<FnParameter>),
    }
}

ast_nodes! {
    pub struct Field {
        pub name: Option<String>,
        pub visibility: Visibility,
        pub mutability: Mutability,
        pub ty: Type,
    }
    pub struct Macro {
        pub name: String,
        pub text: String,
    }
}

ast_nodes_impl! {
    impl Node for Field {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Type(self.ty.clone())]
        }
    }
    impl Node for Macro {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
}
