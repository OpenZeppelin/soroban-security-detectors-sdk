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

pub trait Node {
    fn children(&self) -> impl Iterator;
}

#[derive(Clone, Default, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Visibility {
    #[default]
    Private,
    Public,
    Restricted,
}

impl Visibility {
    #[must_use]
    pub fn from_syn_visibility(visibility: &syn::Visibility) -> Self {
        match visibility {
            syn::Visibility::Public(_) => Visibility::Public,
            syn::Visibility::Inherited => Visibility::Private,
            syn::Visibility::Restricted(_) => Visibility::Restricted,
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
                       $name::$arm(n) => { ast_enum!(@id_arm n, $( $conv )?) }
                    )*
                }
            }

            #[must_use]
            $enum_vis fn location(&self) -> $crate::node::Location {
                match self {
                    $(
                        $name::$arm(n) => { ast_enum!(@location_arm n, $( $conv )?) }
                    )*
                }
            }
        }

    };

    (@id_arm $inner:ident, ty) => {
        $inner.id()
    };

    (@id_arm $inner:ident, skip) => {
        0
    };

    (@id_arm $inner:ident, ) => {
        $inner.id
    };

    (@location_arm $inner:ident, ) => {
        $inner.location.clone()
    };

    (@location_arm $inner:ident, ty) => {
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
