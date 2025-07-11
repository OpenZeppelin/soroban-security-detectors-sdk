//! AST node types for Rust type annotations.
//!
//! Defines the `NodeType` enum representing parsed type expressions such as paths,
//! references, pointers, tuples, arrays, and more.
use serde::{Deserialize, Serialize};
use syn::parse_str;

use super::{
    contract::{Contract, Struct},
    custom_type::Type,
    definition::{Definition, Enum},
    directive::Directive,
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    literal::Literal,
    misc::Misc,
    node::{Location, Node},
    pattern::Pattern,
    statement::Statement,
};
use quote::ToTokens;
use std::rc::Rc;
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
pub enum NodeType {
    #[default]
    Empty,
    /// A named type or path, including any generics as represented in the token stream
    Path(String),
    /// A reference `&T` or `&mut T`, with explicit flag
    Reference {
        inner: Box<NodeType>,
        mutable: bool,
        is_explicit_reference: bool,
    },
    /// A raw pointer `*const T` or `*mut T`
    Ptr { inner: Box<NodeType>, mutable: bool },
    /// A tuple type `(T1, T2, ...)`
    Tuple(Vec<NodeType>),
    /// An array type `[T; len]`, with optional length if parseable
    Array {
        inner: Box<NodeType>,
        len: Option<usize>,
    },
    /// A slice type `[T]`
    Slice(Box<NodeType>),
    /// A bare function pointer `fn(a, b) -> R`
    BareFn {
        inputs: Vec<NodeType>,
        output: Box<NodeType>,
    },
    /// A generic type annotation, e.g., `Option<T>`, `Result<A, B>`
    Generic {
        base: Box<NodeType>,
        args: Vec<NodeType>,
    },
    /// A trait object type `dyn Trait1 + Trait2`
    TraitObject(Vec<String>),
    /// An `impl Trait` type
    ImplTrait(Vec<String>),
    Closure {
        inputs: Vec<NodeType>,
        output: Box<NodeType>,
    },
}

impl NodeType {
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            NodeType::Path(name) => name.clone(),
            NodeType::Reference {
                inner,
                is_explicit_reference,
                ..
            } => {
                if *is_explicit_reference {
                    format!("&{}", inner.name())
                } else {
                    inner.name()
                }
            }
            NodeType::Ptr { inner, mutable } => {
                let star = if *mutable { "*mut" } else { "*const" };
                format!("{} {}", star, inner.name())
            }
            NodeType::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(NodeType::name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            NodeType::Array { inner, len } => format!(
                "[{}; {}]",
                inner.name(),
                len.map_or("..".to_string(), |l| l.to_string())
            ),
            NodeType::Slice(inner) => format!("[{}]", inner.name()),
            NodeType::BareFn { inputs, output } => {
                let mut result = inputs
                    .iter()
                    .map(NodeType::name)
                    .collect::<Vec<_>>()
                    .join(", ");
                if result.is_empty() {
                    result = "_".to_string();
                }
                let output = if output.name().is_empty() {
                    "_".to_string()
                } else {
                    output.name()
                };
                format!("fn({result}) -> {output}")
            }
            NodeType::Closure { inputs, output } => {
                let mut result = inputs
                    .iter()
                    .map(NodeType::name)
                    .collect::<Vec<_>>()
                    .join(", ");
                if result.is_empty() {
                    result = "_".to_string();
                }
                let output = if output.name().is_empty() {
                    "_".to_string()
                } else {
                    output.name()
                };
                format!("{result} || -> {output}")
            }
            NodeType::Generic { base, args } => format!(
                "{}<{}>",
                base.name(),
                args.iter()
                    .map(NodeType::name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            NodeType::TraitObject(bounds) => format!("dyn {}", bounds.join(" + ")),
            NodeType::ImplTrait(bounds) => format!("impl {}", bounds.join(" + ")),
            NodeType::Empty => String::from("_"),
        }
    }

    #[must_use]
    pub fn pure_name(&self) -> String {
        match self {
            NodeType::Path(name) => name.clone(),
            NodeType::Reference { inner, .. }
            | NodeType::Ptr { inner, .. }
            | NodeType::Array { inner, len: _ }
            | NodeType::Slice(inner) => inner.pure_name(),
            NodeType::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(NodeType::pure_name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            NodeType::BareFn { inputs, output } => {
                let mut result = inputs
                    .iter()
                    .map(NodeType::pure_name)
                    .collect::<Vec<_>>()
                    .join(", ");
                if result.is_empty() {
                    result = "_".to_string();
                }
                let output = output.pure_name();
                format!("fn({result}) -> {output}")
            }
            NodeType::Closure { inputs, output } => {
                let mut result = inputs
                    .iter()
                    .map(NodeType::pure_name)
                    .collect::<Vec<_>>()
                    .join(", ");
                if result.is_empty() {
                    result = "_".to_string();
                }
                let output = output.pure_name();
                format!("{result} || -> {output}")
            }
            NodeType::Generic { base, args } => format!(
                "{}<{}>",
                base.pure_name(),
                args.iter()
                    .map(NodeType::pure_name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            NodeType::TraitObject(bounds) | NodeType::ImplTrait(bounds) => bounds.join(" + "),
            NodeType::Empty => String::from("_"),
        }
    }

    #[must_use]
    pub fn is_self(&self) -> bool {
        match self {
            NodeType::Path(name) => name.to_lowercase() == "self",
            NodeType::Reference { inner, .. }
            | NodeType::Ptr { inner, .. }
            | NodeType::Array { inner, .. }
            | NodeType::Slice(inner) => inner.is_self(),
            NodeType::Tuple(elems) => elems.iter().any(NodeType::is_self),
            NodeType::BareFn { inputs, output } | NodeType::Closure { inputs, output } => {
                inputs.iter().any(NodeType::is_self) || output.is_self()
            }
            NodeType::Generic { base, args } => {
                base.is_self() || args.iter().any(NodeType::is_self)
            }
            NodeType::TraitObject(bounds) | NodeType::ImplTrait(bounds) => {
                bounds.iter().any(|b| b.to_lowercase() == "self")
            }
            NodeType::Empty => false,
        }
    }

    #[allow(clippy::assigning_clones)]
    pub fn replace_path(&mut self, new_path: String) {
        match self {
            NodeType::Path(_) => {
                *self = NodeType::Path(new_path);
            }
            NodeType::Reference { inner, .. }
            | NodeType::Ptr { inner, .. }
            | NodeType::Array { inner, .. }
            | NodeType::Slice(inner) => {
                inner.replace_path(new_path);
            }
            NodeType::Tuple(elems) => {
                for elem in elems {
                    elem.replace_path(new_path.clone());
                }
            }
            NodeType::BareFn { inputs, output } | NodeType::Closure { inputs, output } => {
                for input in inputs {
                    input.replace_path(new_path.clone());
                }
                output.replace_path(new_path);
            }
            NodeType::Generic { base, args } => {
                base.replace_path(new_path.clone());
                for arg in args {
                    arg.replace_path(new_path.clone());
                }
            }
            NodeType::TraitObject(bounds) | NodeType::ImplTrait(bounds) => {
                for bound in bounds.iter_mut() {
                    if bound.to_lowercase() == "self" {
                        *bound = new_path.clone();
                    }
                }
            }
            NodeType::Empty => {}
        }
    }

    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn from_syn_item(ty: &syn::Type) -> NodeType {
        match ty {
            syn::Type::Path(type_path) => {
                /// detect generics on the last segment
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
                                    Some(NodeType::from_syn_item(ty))
                                } else {
                                    None
                                }
                            })
                            .collect();
                        return NodeType::Generic {
                            base: Box::new(NodeType::Path(base_str)),
                            args,
                        };
                    }
                }
                NodeType::Path(type_path.to_token_stream().to_string())
            }
            syn::Type::Reference(type_ref) => {
                let inner = NodeType::from_syn_item(&type_ref.elem);
                NodeType::Reference {
                    inner: Box::new(inner),
                    mutable: type_ref.mutability.is_some(),
                    is_explicit_reference: true,
                }
            }
            syn::Type::Ptr(type_ptr) => {
                let inner = NodeType::from_syn_item(&type_ptr.elem);
                NodeType::Ptr {
                    inner: Box::new(inner),
                    mutable: type_ptr.mutability.is_some(),
                }
            }
            syn::Type::Array(type_array) => {
                let inner = NodeType::from_syn_item(&type_array.elem);
                let len = if let syn::Expr::Lit(expr_lit) = &type_array.len {
                    if let syn::Lit::Int(lit_int) = &expr_lit.lit {
                        lit_int.base10_parse::<usize>().ok()
                    } else {
                        None
                    }
                } else {
                    None
                };
                NodeType::Array {
                    inner: Box::new(inner),
                    len,
                }
            }
            syn::Type::Slice(type_slice) => {
                let inner = NodeType::from_syn_item(&type_slice.elem);
                NodeType::Slice(Box::new(inner))
            }
            syn::Type::Tuple(type_tuple) => {
                let elems = type_tuple
                    .elems
                    .iter()
                    .map(NodeType::from_syn_item)
                    .collect();
                NodeType::Tuple(elems)
            }
            syn::Type::BareFn(type_fn) => {
                let inputs = type_fn
                    .inputs
                    .iter()
                    .map(|arg| NodeType::from_syn_item(&arg.ty))
                    .collect();
                let output = if let syn::ReturnType::Type(_, ty) = &type_fn.output {
                    NodeType::from_syn_item(ty)
                } else {
                    NodeType::Empty
                };
                NodeType::BareFn {
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
                NodeType::TraitObject(bounds)
            }
            syn::Type::ImplTrait(it) => {
                let bounds = it
                    .bounds
                    .iter()
                    .map(|b| b.to_token_stream().to_string())
                    .collect();
                NodeType::ImplTrait(bounds)
            }
            syn::Type::Group(type_group) => NodeType::from_syn_item(&type_group.elem),
            syn::Type::Paren(type_paren) => NodeType::from_syn_item(&type_paren.elem),
            syn::Type::Infer(_) => NodeType::Path("_".to_string()),
            syn::Type::Never(_) => NodeType::Path("!".to_string()),
            syn::Type::Macro(mac) => NodeType::Path(mac.mac.path.to_token_stream().to_string()),
            _ => NodeType::Path(ty.to_token_stream().to_string()),
        }
    }

    pub(crate) fn from_string(type_name: &str) -> NodeType {
        match parse_str::<syn::Type>(type_name) {
            Ok(ty) => NodeType::from_syn_item(&ty),
            Err(_) => NodeType::Path(type_name.to_string()),
        }
    }
}

#[cfg(test)]
mod generic_tests {
    use super::NodeType;
    use syn::parse_str;

    #[test]
    fn test_generic_simple() {
        let ty: syn::Type = parse_str("Option<u32>").unwrap();
        let node = NodeType::from_syn_item(&ty);
        assert_eq!(
            node,
            NodeType::Generic {
                base: Box::new(NodeType::Path("Option".to_string())),
                args: vec![NodeType::Path("u32".to_string())],
            }
        );
    }

    #[test]
    fn test_generic_multi_arg() {
        let ty: syn::Type = parse_str("Result<A, B>").unwrap();
        let node = NodeType::from_syn_item(&ty);
        assert_eq!(
            node,
            NodeType::Generic {
                base: Box::new(NodeType::Path("Result".to_string())),
                args: vec![
                    NodeType::Path("A".to_string()),
                    NodeType::Path("B".to_string())
                ],
            }
        );
    }
}

#[cfg(test)]
mod tests {
    use super::NodeType;
    use syn::parse_str;

    #[test]
    fn test_trait_object() {
        let ty: syn::Type = parse_str("dyn Foo + Bar").unwrap();
        let node = NodeType::from_syn_item(&ty);
        assert_eq!(
            node,
            NodeType::TraitObject(vec!["Foo".to_string(), "Bar".to_string()])
        );
    }

    #[test]
    fn test_impl_trait() {
        let ty: syn::Type = parse_str("impl Foo + Bar").unwrap();
        let node = NodeType::from_syn_item(&ty);
        assert_eq!(
            node,
            NodeType::ImplTrait(vec!["Foo".to_string(), "Bar".to_string()])
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
