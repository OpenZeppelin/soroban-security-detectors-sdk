//! AST nodes for custom types (type aliases and simple textual types).
//!
//! Defines the `Type` enum for type annotations, and AST nodes for `type` declarations.

use crate::{ast_enum, ast_nodes};
use crate::node::{Location, Visibility};
use std::rc::Rc;

ast_enum! {
    /// Represents a Rust type in the AST: a textual annotation, an associated alias, or struct alias.
    pub enum Type {
        /// A raw type as a token stream (e.g., "u32", "Foo<T>").
        @skip T(String),
        /// Associated type alias in an `impl` block.
        Alias(Rc<TypeAlias>),
        /// Struct alias (reserved for future use).
        Struct(Rc<TStruct>),
    }
}

ast_nodes! {
    /// File-level `type Name = Type;` definition.
    pub struct T {
        pub name: String,
        pub visibility: Visibility,
        pub ty: String,
    }

    /// Associated type alias in an `impl` block: `type Foo = Bar;`.
    pub struct TypeAlias {
        pub name: String,
        pub visibility: Visibility,
        pub ty: Box<Type>,
    }

    /// Placeholder for struct-type alias (unused currently).
    pub struct TStruct {
        pub name: String,
        pub visibility: Visibility,
        pub ty: String,
    }
}