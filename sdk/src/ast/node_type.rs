use serde::{Deserialize, Serialize};

use super::{
    contract::{Contract, Struct},
    custom_type::{Type, TypeAlias},
    definition::{Const, Definition, Enum, Plane},
    directive::Directive,
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    literal::Literal,
    misc::{Macro, Misc},
    node::{Location, Node},
    pattern::Pattern,
    statement::Statement,
};
use quote::ToTokens;
use std::{any::Any, default, rc::Rc, vec};
pub type RcFile = Rc<File>;
pub type RcContract = Rc<Struct>;
pub type RcFunction = Rc<Function>;
pub type RcFnParameter = Rc<FnParameter>;
pub type RcExpression = Rc<Expression>;
pub type RcFunctionCall = Rc<FunctionCall>;
pub type RcMethodCall = Rc<MethodCall>;
pub type RcEnum = Rc<Enum>;
pub type RcStruct = Rc<Struct>;

#[derive(Clone, PartialEq, Eq, Debug, Default, serde::Serialize, serde::Deserialize)]
pub enum TypeNode {
    #[default]
    Empty,
    /// A named type or path, including any generics as represented in the token stream
    Path(String),
    /// A reference `&T` or `&mut T`
    Reference { inner: Box<TypeNode>, mutable: bool },
    /// A raw pointer `*const T` or `*mut T`
    Ptr { inner: Box<TypeNode>, mutable: bool },
    /// A tuple type `(T1, T2, ...)`
    Tuple(Vec<TypeNode>),
    /// An array type `[T; len]`, with optional length if parseable
    Array {
        inner: Box<TypeNode>,
        len: Option<usize>,
    },
    /// A slice type `[T]`
    Slice(Box<TypeNode>),
    /// A bare function pointer `fn(a, b) -> R`
    BareFn {
        inputs: Vec<TypeNode>,
        output: Box<TypeNode>,
    },
    /// A generic type annotation, e.g., `Option<T>`, `Result<A, B>`
    Generic {
        base: Box<TypeNode>,
        args: Vec<TypeNode>,
    },
    /// A trait object type `dyn Trait1 + Trait2`
    TraitObject(Vec<String>),
    /// An `impl Trait` type
    ImplTrait(Vec<String>),
}

impl TypeNode {
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn from_syn_item(ty: &syn::Type) -> TypeNode {
        match ty {
            syn::Type::Path(type_path) => {
                // detect generics on the last segment
                use syn::{GenericArgument, PathArguments};
                if let Some(last) = type_path.path.segments.last() {
                    if let PathArguments::AngleBracketed(br) = &last.arguments {
                        // reconstruct base path without generic args
                        let base_str = type_path
                            .path
                            .segments
                            .iter()
                            .map(|seg| seg.ident.to_string())
                            .collect::<Vec<_>>()
                            .join("::");
                        let args = br
                            .args
                            .iter()
                            .filter_map(|arg| {
                                if let GenericArgument::Type(ty) = arg {
                                    Some(TypeNode::from_syn_item(ty))
                                } else {
                                    None
                                }
                            })
                            .collect();
                        return TypeNode::Generic {
                            base: Box::new(TypeNode::Path(base_str)),
                            args,
                        };
                    }
                }
                TypeNode::Path(type_path.to_token_stream().to_string())
            }
            syn::Type::Reference(type_ref) => {
                let inner = TypeNode::from_syn_item(&type_ref.elem);
                TypeNode::Reference {
                    inner: Box::new(inner),
                    mutable: type_ref.mutability.is_some(),
                }
            }
            syn::Type::Ptr(type_ptr) => {
                let inner = TypeNode::from_syn_item(&type_ptr.elem);
                TypeNode::Ptr {
                    inner: Box::new(inner),
                    mutable: type_ptr.mutability.is_some(),
                }
            }
            syn::Type::Array(type_array) => {
                let inner = TypeNode::from_syn_item(&type_array.elem);
                // Array length parsing not supported; default to None
                let len = None;
                TypeNode::Array {
                    inner: Box::new(inner),
                    len,
                }
            }
            syn::Type::Slice(type_slice) => {
                let inner = TypeNode::from_syn_item(&type_slice.elem);
                TypeNode::Slice(Box::new(inner))
            }
            syn::Type::Tuple(type_tuple) => {
                let elems = type_tuple
                    .elems
                    .iter()
                    .map(TypeNode::from_syn_item)
                    .collect();
                TypeNode::Tuple(elems)
            }
            syn::Type::BareFn(type_fn) => {
                let inputs = type_fn
                    .inputs
                    .iter()
                    .map(|arg| TypeNode::from_syn_item(&arg.ty))
                    .collect();
                let output = if let syn::ReturnType::Type(_, ty) = &type_fn.output {
                    TypeNode::from_syn_item(ty)
                } else {
                    TypeNode::Empty
                };
                TypeNode::BareFn {
                    inputs,
                    output: Box::new(output),
                }
            }
            syn::Type::TraitObject(obj) => {
                let bounds = obj
                    .bounds
                    .iter()
                    .map(|b| b.to_token_stream().to_string())
                    .collect();
                TypeNode::TraitObject(bounds)
            }
            syn::Type::ImplTrait(it) => {
                let bounds = it
                    .bounds
                    .iter()
                    .map(|b| b.to_token_stream().to_string())
                    .collect();
                TypeNode::ImplTrait(bounds)
            }
            syn::Type::Group(type_group) => TypeNode::from_syn_item(&type_group.elem),
            syn::Type::Paren(type_paren) => TypeNode::from_syn_item(&type_paren.elem),
            syn::Type::Infer(_) => TypeNode::Path("_".to_string()),
            syn::Type::Never(_) => TypeNode::Path("!".to_string()),
            syn::Type::Macro(mac) => TypeNode::Path(mac.mac.path.to_token_stream().to_string()),
            _ => TypeNode::Path(ty.to_token_stream().to_string()),
        }
    }
}

#[cfg(test)]
mod generic_tests {
    use super::TypeNode;
    use syn::parse_str;

    #[test]
    fn test_generic_simple() {
        let ty: syn::Type = parse_str("Option<u32>").unwrap();
        let node = TypeNode::from_syn_item(&ty);
        assert_eq!(
            node,
            TypeNode::Generic {
                base: Box::new(TypeNode::Path("Option".to_string())),
                args: vec![TypeNode::Path("u32".to_string())],
            }
        );
    }

    #[test]
    fn test_generic_multi_arg() {
        let ty: syn::Type = parse_str("Result<A, B>").unwrap();
        let node = TypeNode::from_syn_item(&ty);
        assert_eq!(
            node,
            TypeNode::Generic {
                base: Box::new(TypeNode::Path("Result".to_string())),
                args: vec![
                    TypeNode::Path("A".to_string()),
                    TypeNode::Path("B".to_string())
                ],
            }
        );
    }
}

#[cfg(test)]
mod tests {
    use super::TypeNode;
    use syn::parse_str;

    #[test]
    fn test_trait_object() {
        let ty: syn::Type = parse_str("dyn Foo + Bar").unwrap();
        let node = TypeNode::from_syn_item(&ty);
        assert_eq!(
            node,
            TypeNode::TraitObject(vec!["Foo".to_string(), "Bar".to_string()])
        );
    }

    #[test]
    fn test_impl_trait() {
        let ty: syn::Type = parse_str("impl Foo + Bar").unwrap();
        let node = TypeNode::from_syn_item(&ty);
        assert_eq!(
            node,
            TypeNode::ImplTrait(vec!["Foo".to_string(), "Bar".to_string()])
        );
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum ContractType {
    Struct(Rc<Contract>),
    Enum(Rc<Contract>),
}

impl ContractType {
    #[must_use = "Use this method to get the id of the contract"]
    pub fn id(&self) -> u32 {
        match self {
            ContractType::Struct(c) | ContractType::Enum(c) => c.id,
        }
    }

    #[must_use = "Use this method to get the location of the contract"]
    pub fn location(&self) -> Location {
        match self {
            ContractType::Struct(c) | ContractType::Enum(c) => c.location.clone(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum NodeKind {
    File(Rc<File>),
    Directive(Directive),
    Definition(Definition),
    Statement(Statement),
    Expression(Expression),
    Pattern(Pattern),
    Literal(Literal),
    Type(Type),
    Misc(Misc),
}

#[must_use]
pub fn get_expression_parent_type_id(node: &ExpressionParentType) -> u32 {
    match node {
        ExpressionParentType::Function(f) => f.id,
        ExpressionParentType::Expression(e) => e.id(),
    }
}

impl NodeKind {
    #[must_use]
    pub fn id(&self) -> u32 {
        match self {
            NodeKind::File(f) => f.id,
            NodeKind::Definition(d) => d.id(),
            NodeKind::Directive(d) => d.id(),
            NodeKind::Statement(s) => s.id(),
            NodeKind::Expression(e) => e.id(),
            NodeKind::Pattern(p) => p.id,
            NodeKind::Literal(l) => l.id(),
            NodeKind::Type(t) => t.id(),
            NodeKind::Misc(m) => m.id(),
        }
    }

    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn location(&self) -> Location {
        match self {
            NodeKind::File(f) => Location {
                source: f.source_code.clone(),
                offset_start: 0,
                offset_end: f.source_code.len() as u32,
                start_column: 0,
                start_line: 0,
                end_column: f.source_code.lines().last().unwrap_or_default().len() as u32,
                end_line: f.source_code.lines().count() as u32,
            },
            NodeKind::Definition(d) => d.location().clone(),
            NodeKind::Directive(d) => d.location(),
            NodeKind::Statement(s) => s.location(),
            NodeKind::Expression(e) => e.location(),
            NodeKind::Literal(l) => l.location(),
            NodeKind::Pattern(p) => p.location.clone(),
            NodeKind::Type(t) => t.location(),
            NodeKind::Misc(m) => m.location(),
        }
    }

    #[must_use]
    pub fn children(&self) -> Vec<NodeKind> {
        match self {
            NodeKind::File(file) => file.children(),
            NodeKind::Definition(definition) => definition.children(),
            NodeKind::Directive(directive) => directive.children(),
            NodeKind::Statement(statement) => statement.children(),
            NodeKind::Expression(expression) => expression.children(),
            NodeKind::Pattern(pattern) => pattern.children(),
            NodeKind::Literal(literal) => literal.children(),
            NodeKind::Type(ty) => ty.children(),
            NodeKind::Misc(misc) => misc.children(),
        }
    }
}
