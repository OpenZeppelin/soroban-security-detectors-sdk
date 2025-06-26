use super::node_type::NodeKind;
use std::{any::Any, cmp::Reverse};

#[derive(Clone, PartialEq, Eq, Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct Location {
    pub offset_start: u32,
    pub offset_end: u32,
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub source: String,
}

pub trait Node: Any + std::fmt::Debug {
    fn id(&self) -> u32;
    fn location(&self) -> Location;
    fn node_type_name(&self) -> String {
        std::any::type_name::<Self>()
            .split("::")
            .last()
            .unwrap_or_default()
            .to_string()
    }
    fn children(&self) -> Vec<NodeKind>;
    fn sorted_children(&self) -> Vec<NodeKind> {
        let mut children = self.children();
        children.sort_by_key(|c| Reverse(c.id()));
        children
    }
}

impl dyn Node {
    pub fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Default, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Visibility {
    #[default]
    Private,
    Public,
    Inherited,
    PubCrate,
    PubSuper,
    PubIn(String),
}

impl Visibility {
    #[must_use]
    pub fn from_syn_visibility(visibility: &syn::Visibility) -> Self {
        match visibility {
            syn::Visibility::Public(_) => Visibility::Public,
            syn::Visibility::Inherited => Visibility::Inherited,
            syn::Visibility::Restricted(_) => Visibility::Private,
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Mutability {
    #[default]
    Immutable,
    Mutable,
    Constant,
}

#[macro_export]
macro_rules! location {
    ($item:expr) => {{
        use syn::spanned::Spanned;
        #[allow(clippy::cast_possible_truncation)]
        $crate::node::Location {
            offset_start: $item.span().byte_range().start as u32,
            offset_end: $item.span().byte_range().end as u32,
            source: $item.span().source_text().unwrap_or_default(),
            start_line: $item.span().start().line as u32,
            start_column: $item.span().start().column as u32,
            end_line: $item.span().end().line as u32,
            end_column: $item.span().end().column as u32,
        }
    }};
}

#[macro_export]
macro_rules! source_code {
    ($item:expr) => {{
        use syn::spanned::Spanned;
        $item.span().source_text().unwrap_or_default()
    }};
}

#[macro_export]
macro_rules! ast_enum {
    (
        $(#[$outer:meta])*
        $enum_vis:vis enum $name:ident {
            $(
                $(#[$arm_attr:meta])*
                $(@$conv:ident)? $arm:ident $( ( $($tuple:tt)* ) )? $( { $($struct:tt)* } )? ,
            )*
        }
    ) => {
        $(#[$outer])*
        #[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
        $enum_vis enum $name {
            $(
                $(#[$arm_attr])*
                $arm $( ( $($tuple)* ) )? $( { $($struct)* } )? ,
            )*
        }

        impl $name {
            #[must_use]
            $enum_vis fn id(&self) -> u32 {
                match self {
                    $(
                        $name::$arm(ref _n) => { ast_enum!(@id_arm _n, $( $conv )?) }
                    )*
                }
            }

            #[must_use]
            $enum_vis fn location(&self) -> $crate::node::Location {
                match self {
                    $(
                        $name::$arm(ref _n) => { ast_enum!(@location_arm _n, $( $conv )?) }
                    )*
                }
            }

            #[must_use]
            pub fn children(&self) -> Vec<NodeKind> {
                match self {
                    $(
                        $name::$arm(_a) => {
                            _a.children()
                        }
                    )*
                }
            }
        }

        impl From<&$name> for $crate::ast::node_type::NodeKind {
            fn from(n: &$name) -> Self {
                match n {
                    $(
                        $name::$arm(a) => {
                            $crate::ast::node_type::NodeKind::$name($name::$arm(a.clone()))
                        }
                    )*
                }
            }
        }

        impl From<$name> for $crate::ast::node_type::NodeKind {
            fn from(n: $name) -> Self {
                match n {
                    $(
                        $name::$arm(inner) => $crate::ast::node_type::NodeKind::$name($name::$arm(inner.clone())),
                    )*
                }
            }
        }

    };

    (@id_arm $inner:ident, ) => {
        $inner.id()
    };

    (@location_arm $inner:ident, ) => {
        $inner.location().clone()
    };

    (@location_arm $inner:ident, skip) => {
        $crate::node::Location::default()
    };
}

#[macro_export]
macro_rules! ast_node {
    (
        $(#[$outer:meta])*
        $struct_vis:vis struct $name:ident {
            $(
                $(#[$field_attr:meta])*
                $field_vis:vis $field_name:ident : $field_ty:ty
            ),* $(,)?
        }
    ) => {
        $(#[$outer])*
        #[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
        $struct_vis struct $name {
            pub id: u32,
            pub location: Location,
            $(
                $(#[$field_attr])*
                $field_vis $field_name : $field_ty,
            )*
        }
    };
}

#[macro_export]
macro_rules! ast_nodes {
    (
        $(
            $(#[$outer:meta])*
            $struct_vis:vis struct $name:ident { $($fields:tt)* }
        )+
    ) => {
        $(
            $crate::ast_node! {
                $(#[$outer])*
                $struct_vis struct $name { $($fields)* }
            }
        )+
    };
}

#[macro_export]
macro_rules! ast_node_impl {
    (
        $(#[$outer:meta])*
        impl Node for $name:ident {
            $(
                $(#[$method_attr:meta])*
                fn $method:ident ( $($args:tt)* ) -> $ret:ty $body:block
            )*
        }
    ) => {
        $(#[$outer])*
        impl Node for $name {
            fn id(&self) -> u32 {
                self.id
            }

            fn location(&self) -> $crate::node::Location {
                self.location.clone()
            }

            $(
                $(#[$method_attr])*
                fn $method ( $($args)* ) -> $ret $body
            )*
        }
    };
}

#[macro_export]
macro_rules! ast_nodes_impl {
    (
        $(
            $(#[$outer:meta])*
            impl Node for $name:ident {
                $(
                    $(#[$method_attr:meta])*
                    fn $method:ident ( $($args:tt)* ) -> $ret:ty $body:block
                )*
            }
        )+
    ) => {
        $(
            $crate::ast_node_impl! {
                $(#[$outer])*
                impl Node for $name {
                    $(
                        $(#[$method_attr])*
                        fn $method ( $($args)* ) -> $ret $body
                    )*
                }
            }
        )+
    };
}
