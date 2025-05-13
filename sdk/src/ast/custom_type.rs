use crate::node::{Location, Visibility};
use crate::{ast_enum, ast_nodes};
use std::rc::Rc;

ast_enum! {
    /// Represents a Rust type in the AST: a textual annotation, an associated alias, or struct alias.
    pub enum Type {
        /// A raw type as a token stream (e.g., "u32", "Foo<T>").
        @skip Typedef(String),
        /// Associated type alias in an `impl` block.
        Alias(Rc<TypeAlias>),
        /// Struct alias (reserved for future use).
        Struct(Rc<TStruct>),
    }
}

ast_nodes! {
    pub struct Typedef {
        pub attributes: Vec<String>,
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

    /// TODO implement?
    pub struct TStruct {
        pub name: String,
        pub visibility: Visibility,
        pub ty: String,
    }
}
