#![warn(clippy::pedantic)]
use super::{
    contract::Struct,
    custom_type::{Type, TypeAlias, T},
    directive::Directive,
    expression::Expression,
    function::Function,
    misc::{Field, Macro},
    node::{Location, TLocation, Visibility},
    node_type::{ContractType, RcFunction},
};
use soroban_security_rules_macro_lib::node_location;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Definition {
    Const(Rc<Const>),
    ExternCrate(Rc<ExternCrate>),
    Enum(Rc<Enum>),
    Contract(ContractType),
    Struct(Rc<Struct>),
    Function(Rc<Function>),
    Directive(Directive),
    CustomType(Type),
    Macro(Rc<Macro>),
    Module(Rc<Module>),
    Static(Rc<Static>),
    Type(Rc<T>),
    Trait(Rc<Trait>),
    TraitAlias(Rc<TraitAlias>),
    Plane(Rc<Plane>),
    Union(Rc<Union>),
    Empty, // For items we do not instantiate directly, like impl blocks becase we stitch functions with iteir types
}

impl Definition {
    #[must_use = "Use this method to get the id of the definition"]
    pub fn id(&self) -> u128 {
        match self {
            Definition::Const(const_) => const_.id,
            Definition::ExternCrate(extern_crate) => extern_crate.id,
            Definition::Enum(enum_) => enum_.id,
            Definition::Function(function) => function.id,
            Definition::Contract(contract) => match contract {
                ContractType::Contract(contract) => contract.id,
                ContractType::Enum(enum_) => enum_.id,
                ContractType::Struct(struct_) => struct_.id,
            },
            Definition::Struct(struct_) => struct_.id,
            Definition::Directive(directive) => directive.id(),
            Definition::CustomType(type_) => type_.id(),
            Definition::Macro(macro_) => macro_.id,
            Definition::Module(module) => module.id,
            Definition::Static(static_) => static_.id,
            Definition::Type(type_) => type_.id,
            Definition::Trait(trait_) => trait_.id,
            Definition::TraitAlias(trait_alias) => trait_alias.id,
            Definition::Plane(plane) => plane.id,
            Definition::Union(union) => union.id,
            Definition::Empty => 0,
        }
    }

    #[must_use = "Use this method to get the location of the definition"]
    pub fn location(&self) -> Location {
        match self {
            Definition::Const(const_) => const_.location(),
            Definition::ExternCrate(extern_crate) => extern_crate.location(),
            Definition::Enum(enum_) => enum_.location(),
            Definition::Function(function) => function.location(),
            Definition::Contract(contract) => match contract {
                ContractType::Contract(contract) => contract.location(),
                ContractType::Enum(enum_) => enum_.location(),
                ContractType::Struct(struct_) => struct_.location(),
            },
            Definition::Struct(struct_) => struct_.location(),
            Definition::Directive(directive) => directive.location(),
            Definition::CustomType(type_) => type_.location(),
            Definition::Macro(macro_) => macro_.location(),
            Definition::Module(module) => module.location(),
            Definition::Static(static_) => static_.location(),
            Definition::Type(type_) => type_.location(),
            Definition::Trait(trait_) => trait_.location(),
            Definition::TraitAlias(trait_alias) => trait_alias.location(),
            Definition::Plane(plane) => plane.location(),
            Definition::Union(union) => union.location(),
            Definition::Empty => Location::default(),
        }
    }
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Const {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub type_: Type,
    pub value: Option<Expression>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Enum {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub variants: Vec<String>,
    pub methods: RefCell<Vec<RcFunction>>,
    pub functions: RefCell<Vec<RcFunction>>,
    pub type_aliases: RefCell<Vec<Rc<TypeAlias>>>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct ExternCrate {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub alias: Option<String>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Static {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub mutable: bool,
    pub ty: Type,
    pub value: Expression,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Module {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub definitions: Option<Vec<Definition>>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Plane {
    pub id: u128,
    pub location: Location,
    pub value: String,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Union {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub fields: Vec<Rc<Field>>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Trait {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub supertraits: String,
    pub items: Vec<Definition>,
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct TraitAlias {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub visibility: Visibility,
    pub bounds: String,
}
