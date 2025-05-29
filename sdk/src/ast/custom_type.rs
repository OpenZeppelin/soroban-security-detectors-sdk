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

// Helper: convert custom AST Type to semantic TypeNode
use crate::ast::node_type::TypeNode;
impl Type {
    /// Convert this AST `Type` into a semantic `TypeNode`, parsing raw tokens as needed.
    #[must_use]
    pub fn to_type_node(&self) -> TypeNode {
        match self {
            Type::Typedef(s) => {
                match syn::parse_str::<syn::Type>(s) {
                    Ok(ty) => TypeNode::from_syn_item(&ty),
                    Err(_) => TypeNode::Path(s.clone()),
                }
            }
            Type::Alias(alias) => alias.ty.to_type_node(),
            Type::Struct(tstruct) => {
                // Struct alias; parse underlying type if available, else path
                match syn::parse_str::<syn::Type>(&tstruct.ty) {
                    Ok(ty) => TypeNode::from_syn_item(&ty),
                    Err(_) => TypeNode::Path(tstruct.name.clone()),
                }
            }
        }
    }
}
