#![warn(clippy::pedantic)]
use soroban_security_rules_macro_lib::node_location;

#[derive(Clone, PartialEq, Eq, Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct Location {
    pub source_code: String,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

//TODO merge TLocation into
pub trait Node {
    fn children(&self) -> impl Iterator;
}

pub trait TLocation {
    fn location(&self) -> Location;
    fn source_code(&self) -> String;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
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
        $crate::node::Location {
            source_code: $item.span().source_text().unwrap_or_default(),
            start_line: $item.span().start().line as usize,
            start_col: $item.span().start().column as usize,
            end_line: $item.span().end().line as usize,
            end_col: $item.span().end().column as usize,
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
            $enum_vis fn id(&self) -> u128 {
                match self {
                    $(
                       $name::$arm(n) => { ast_enum!(@id_arm n, $( $conv )?) }
                    )*
                }
            }

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
        $inner.location().clone()
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
        #[node_location]
        #[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
        $struct_vis struct $name {
            pub id: u128,
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

#[cfg(test)]
mod tests {
    use crate::utils::test::create_mock_location;

    #[test]
    fn test_mock_location() {
        let location = create_mock_location();

        assert_eq!(location.source_code, "fn main() {}".to_string());
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_col, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_col, 14);
    }
}
