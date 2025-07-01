use crate::node::{Location, Node, Visibility};
use crate::{ast_enum, ast_nodes, ast_nodes_impl};
use std::rc::Rc;

use super::node_type::NodeKind;

ast_enum! {
    pub enum Type {
        Typename(Rc<Typename>),
        Alias(Rc<TypeAlias>),
        Struct(Rc<TStruct>),
    }
}

ast_nodes! {
    pub struct Typename {
        pub name: String,
    }

    /// Associated type alias in an `impl` block: `type Foo = Bar;`.
    pub struct TypeAlias {
        pub name: String,
        pub visibility: Visibility,
        pub ty: Type,
    }

    pub struct TStruct {
        pub name: String,
        pub visibility: Visibility,
        pub ty: Box<Typename>,
    }
}

ast_nodes_impl! {
    impl Node for Typename {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for TypeAlias {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Type(self.ty.clone())]
        }
    }
    impl Node for TStruct {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Type(Type::Typename(Rc::new((*self.ty).clone())))]
        }
    }
}

use crate::ast::node_type::NodeType;
impl Type {
    #[must_use]
    pub fn to_type_node(&self) -> NodeType {
        match self {
            Type::Typename(s) => match syn::parse_str::<syn::Type>(&s.name) {
                Ok(ty) => NodeType::from_syn_item(&ty),
                Err(_) => NodeType::Path(s.name.clone()),
            },
            Type::Alias(alias) => alias.ty.to_type_node(),
            Type::Struct(tstruct) => match syn::parse_str::<syn::Type>(&tstruct.ty.name) {
                Ok(ty) => NodeType::from_syn_item(&ty),
                Err(_) => NodeType::Path(tstruct.name.clone()),
            },
        }
    }
}
