use crate::node::{Location, Visibility};
use crate::{ast_enum, ast_nodes};
use std::rc::Rc;

ast_enum! {
    /// Represents a Rust type in the AST: a textual annotation, an associated alias, or struct alias.
    pub enum Type {
        /// A raw or aliased type as a token stream (e.g., "u32", "Foo<T>").
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

    pub struct TStruct {
        pub name: String,
        pub visibility: Visibility,
        pub ty: String,
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
