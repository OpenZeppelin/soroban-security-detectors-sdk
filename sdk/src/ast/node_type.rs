use serde::{Deserialize, Serialize};

use super::{
    contract::{Contract, Struct},
    custom_type::{Type, TypeAlias},
    definition::{Const, Definition, Enum, Plane},
    expression::{Expression, ExpressionParentType, FunctionCall, MethodCall},
    file::File,
    function::{FnParameter, Function},
    literal::Literal,
    misc::{Macro, Misc},
    node::Location,
    pattern::Pattern,
    statement::Statement,
};
use quote::ToTokens;
use std::{default, rc::Rc};

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
    /// A reference `&T` or `&mut T`, with explicit flag
    Reference {
        inner: Box<TypeNode>,
        mutable: bool,
        is_explicit_reference: bool,
    },
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
    pub fn name(&self) -> String {
        match self {
            TypeNode::Path(name) => name.clone(),
            TypeNode::Reference {
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
            TypeNode::Ptr { inner, mutable } => {
                let star = if *mutable { "*mut" } else { "*const" };
                format!("{} {}", star, inner.name())
            }
            TypeNode::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(TypeNode::name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeNode::Array { inner, len } => format!(
                "[{}; {}]",
                inner.name(),
                len.map_or("..".to_string(), |l| l.to_string())
            ),
            TypeNode::Slice(inner) => format!("[{}]", inner.name()),
            TypeNode::BareFn { inputs, output } => format!(
                "fn({}) -> {}",
                inputs
                    .iter()
                    .map(TypeNode::name)
                    .collect::<Vec<_>>()
                    .join(", "),
                output.name()
            ),
            TypeNode::Generic { base, args } => format!(
                "{}<{}>",
                base.name(),
                args.iter()
                    .map(TypeNode::name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeNode::TraitObject(bounds) => format!("dyn {}", bounds.join(" + ")),
            TypeNode::ImplTrait(bounds) => format!("impl {}", bounds.join(" + ")),
            TypeNode::Empty => String::from("_"),
        }
    }

    #[must_use]
    pub fn is_self(&self) -> bool {
        match self {
            TypeNode::Path(name) => name.to_lowercase() == "self",
            TypeNode::Reference { inner, .. }
            | TypeNode::Ptr { inner, .. }
            | TypeNode::Array { inner, .. }
            | TypeNode::Slice(inner) => inner.is_self(),
            TypeNode::Tuple(elems) => elems.iter().any(TypeNode::is_self),
            TypeNode::BareFn { inputs, output } => {
                inputs.iter().any(TypeNode::is_self) || output.is_self()
            }
            TypeNode::Generic { base, args } => {
                base.is_self() || args.iter().any(TypeNode::is_self)
            }
            TypeNode::TraitObject(bounds) | TypeNode::ImplTrait(bounds) => {
                bounds.iter().any(|b| b.to_lowercase() == "self")
            }
            TypeNode::Empty => false,
        }
    }

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
                    is_explicit_reference: true,
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
                let len = if let syn::Expr::Lit(expr_lit) = &type_array.len {
                    if let syn::Lit::Int(lit_int) = &expr_lit.lit {
                        lit_int.base10_parse::<usize>().ok()
                    } else {
                        None
                    }
                } else {
                    None
                };
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
    FnParameter(RcFnParameter),
    Statement(Statement),
    Pattern(Pattern),
    Literal(Literal),
    Misc(Misc),
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub enum FileChildType {
    Definition(Definition),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum ContractParentType {
    File(RcFile),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum StructChildType {
    Function(RcFunction),
    TypeAlias(Rc<TypeAlias>),
    Constant(Rc<Const>),
    Macro(Rc<Macro>),
    Plane(Rc<Plane>),
}

pub type CustomTypeChildType = StructChildType;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionChildType {
    Expression(Expression),
    Statement(Statement),
    Parameter(RcFnParameter),
    Type(TypeNode),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallParentType {
    Function(RcFunction),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum FunctionCallChildType {
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MethodCallParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MethodCallChildType {
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MemberAccessParentType {
    Function(RcFunction),
    Expression(RcExpression),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum MemberAccessChildType {
    Expression(RcExpression),
}

impl NodeKind {
    #[must_use]
    pub fn id(&self) -> u32 {
        match self {
            NodeKind::File(f) => f.id,
            NodeKind::FnParameter(p) => p.id,
            NodeKind::Statement(s) => s.id(),
            NodeKind::Pattern(p) => p.id,
            NodeKind::Literal(l) => l.id(),
            NodeKind::Misc(m) => m.id(),
        }
    }

    pub fn children(&self) -> Vec<NodeKind> {
        vec![] //TODO: Implement this method
    }
}

#[must_use]
pub fn get_expression_parent_type_id(node: &ExpressionParentType) -> u32 {
    match node {
        ExpressionParentType::Function(f) => f.id,
        ExpressionParentType::Expression(e) => e.id(),
    }
}

#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub fn get_node_location(node: &NodeKind) -> Location {
    match node {
        NodeKind::File(f) => Location {
            source: f.source_code.clone(),
            offset_start: 0,
            offset_end: f.source_code.len() as u32,
            start_column: 0,
            start_line: 0,
            end_column: f.source_code.lines().last().unwrap_or_default().len() as u32,
            end_line: f.source_code.lines().count() as u32,
        },
        NodeKind::FnParameter(p) => p.location.clone(),
        NodeKind::Statement(s) => s.location(),
        NodeKind::Pattern(p) => p.location.clone(),
        NodeKind::Literal(l) => l.location(),
        NodeKind::Misc(m) => m.location(),
    }
}
