#![warn(clippy::pedantic)]
use crate::{ast_enum, ast_nodes};

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

ast_enum! {
    pub enum Definition {
        Empty, // For items we do not instantiate directly, like impl blocks becase we stitch functions with iteir types
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
    }
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

ast_nodes! {
    pub struct Const {
        pub name: String,
        pub visibility: Visibility,
        pub type_: Type,
        pub value: Option<Expression>,
    }

    pub struct Enum {
        pub name: String,
        pub visibility: Visibility,
        pub variants: Vec<String>,
        pub methods: RefCell<Vec<RcFunction>>,
        pub functions: RefCell<Vec<RcFunction>>,
        pub type_aliases: RefCell<Vec<Rc<TypeAlias>>>,
        pub constants: RefCell<Vec<Rc<Const>>>,
        pub macros: RefCell<Vec<Rc<Macro>>>,
        pub plane_defs: RefCell<Vec<Rc<Plane>>>,
    }

    pub struct ExternCrate {
        pub name: String,
        pub visibility: Visibility,
        pub alias: Option<String>,
    }

    pub struct Static {
        pub name: String,
        pub visibility: Visibility,
        pub mutable: bool,
        pub ty: Type,
        pub value: Expression,
    }

    pub struct Module {
        pub name: String,
        pub visibility: Visibility,
        pub definitions: Option<Vec<Definition>>,
    }

    pub struct Plane {
        pub value: String,
    }

    pub struct Union {
        pub name: String,
        pub visibility: Visibility,
        pub fields: Vec<Rc<Field>>,
    }

    pub struct Trait {
        pub name: String,
        pub visibility: Visibility,
        pub supertraits: String,
        pub items: Vec<Definition>,
    }

    pub struct TraitAlias {
        pub name: String,
        pub visibility: Visibility,
        pub bounds: String,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        directive::Use,
        expression::Lit,
        literal::{LInt, Literal},
    };

    use super::*;

    #[test]
    fn test_const_id() {
        let const_ = Const {
            id: 1,
            location: Location::default(),
            name: "CONST".to_string(),
            visibility: Visibility::Public,
            type_: Type::T(String::new()),
            value: None,
        };
        assert_eq!(const_.id, 1);
    }

    #[test]
    fn test_enum_id() {
        let enum_ = Enum {
            id: 2,
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        };
        assert_eq!(enum_.id, 2);
    }

    #[test]
    fn test_extern_crate_id() {
        let extern_crate = ExternCrate {
            id: 3,
            location: Location::default(),
            name: "extern_crate".to_string(),
            visibility: Visibility::Public,
            alias: None,
        };
        assert_eq!(extern_crate.id, 3);
    }

    #[test]
    fn test_static_id() {
        let static_ = Static {
            id: 4,
            location: Location::default(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::T(String::new()),
            value: Expression::Lit(Rc::new(Lit {
                id: 0,
                value: Literal::Int(Rc::new(LInt {
                    id: 0,
                    location: Location::default(),
                    value: 0,
                })),
                location: Location::default(),
            })),
        };
        assert_eq!(static_.id, 4);
    }

    #[test]
    fn test_module_id() {
        let module = Module {
            id: 5,
            location: Location::default(),
            name: "MODULE".to_string(),
            visibility: Visibility::Public,
            definitions: None,
        };
        assert_eq!(module.id, 5);
    }

    #[test]
    fn test_plane_id() {
        let plane = Plane {
            id: 6,
            location: Location::default(),
            value: "PLANE".to_string(),
        };
        assert_eq!(plane.id, 6);
    }

    #[test]
    fn test_union_id() {
        let union = Union {
            id: 7,
            location: Location::default(),
            name: "UNION".to_string(),
            visibility: Visibility::Public,
            fields: vec![],
        };
        assert_eq!(union.id, 7);
    }

    #[test]
    fn test_trait_id() {
        let trait_ = Trait {
            id: 8,
            location: Location::default(),
            name: "TRAIT".to_string(),
            visibility: Visibility::Public,
            supertraits: "SuperTrait".to_string(),
            items: vec![],
        };
        assert_eq!(trait_.id, 8);
    }

    #[test]
    fn test_trait_alias_id() {
        let trait_alias = TraitAlias {
            id: 9,
            location: Location::default(),
            name: "TRAIT_ALIAS".to_string(),
            visibility: Visibility::Public,
            bounds: "Bounds".to_string(),
        };
        assert_eq!(trait_alias.id, 9);
    }

    #[test]
    fn test_node_location_attribute() {
        #[node_location]
        #[derive(Clone, serde::Serialize, serde::Deserialize)]
        struct TestStruct {
            id: u128,
            location: Location,
        }

        let test_struct = TestStruct {
            id: 10,
            location: Location::default(),
        };

        assert_eq!(test_struct.id, 10);
        assert_eq!(test_struct.location, Location::default());
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_definition_id() {
        let const_ = Definition::Const(Rc::new(Const {
            id: 1,
            location: Location::default(),
            name: "CONST".to_string(),
            visibility: Visibility::Public,
            type_: Type::T(String::new()),
            value: None,
        }));
        assert_eq!(const_.id(), 1);

        let extern_crate = Definition::ExternCrate(Rc::new(ExternCrate {
            id: 3,
            location: Location::default(),
            name: "extern_crate".to_string(),
            visibility: Visibility::Public,
            alias: None,
        }));
        assert_eq!(extern_crate.id(), 3);

        let enum_ = Definition::Enum(Rc::new(Enum {
            id: 2,
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        }));
        assert_eq!(enum_.id(), 2);

        let contract = Definition::Contract(ContractType::Contract(Rc::new(Struct {
            id: 10,
            location: Location::default(),
            name: "CONTRACT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract.id(), 10);

        let contract_enum = Definition::Contract(ContractType::Enum(Rc::new(Enum {
            id: 11,
            location: Location::default(),
            name: "CONTRACT_ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract_enum.id(), 11);

        let contract_struct = Definition::Contract(ContractType::Struct(Rc::new(Struct {
            id: 12,
            location: Location::default(),
            name: "CONTRACT_STRUCT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract_struct.id(), 12);

        let struct_ = Definition::Struct(Rc::new(Struct {
            id: 13,
            location: Location::default(),
            name: "STRUCT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        }));
        assert_eq!(struct_.id(), 13);

        let directive = Definition::Directive(Directive::Use(Rc::new(Use {
            id: 14,
            location: Location::default(),
            visibility: Visibility::Public,
            path: String::new(),
        })));
        assert_eq!(directive.id(), 14);

        let custom_type = Definition::CustomType(Type::T("CustomType".to_string()));
        assert_eq!(custom_type.id(), 0); // Assuming Type::T has id 0

        let macro_ = Definition::Macro(Rc::new(Macro {
            id: 15,
            location: Location::default(),
            name: "MACRO".to_string(),
            text: String::new(),
        }));
        assert_eq!(macro_.id(), 15);

        let module = Definition::Module(Rc::new(Module {
            id: 5,
            location: Location::default(),
            name: "MODULE".to_string(),
            visibility: Visibility::Public,
            definitions: None,
        }));
        assert_eq!(module.id(), 5);

        let static_ = Definition::Static(Rc::new(Static {
            id: 4,
            location: Location::default(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::T(String::new()),
            value: Expression::Lit(Rc::new(Lit {
                id: 0,
                value: Literal::Int(Rc::new(LInt {
                    id: 0,
                    location: Location::default(),
                    value: 0,
                })),
                location: Location::default(),
            })),
        }));
        assert_eq!(static_.id(), 4);

        let type_ = Definition::Type(Rc::new(T {
            id: 16,
            location: Location::default(),
            name: "TYPE".to_string(),
            visibility: Visibility::Public,
            ty: String::new(),
        }));
        assert_eq!(type_.id(), 16);

        let trait_ = Definition::Trait(Rc::new(Trait {
            id: 8,
            location: Location::default(),
            name: "TRAIT".to_string(),
            visibility: Visibility::Public,
            supertraits: "SuperTrait".to_string(),
            items: vec![],
        }));
        assert_eq!(trait_.id(), 8);

        let trait_alias = Definition::TraitAlias(Rc::new(TraitAlias {
            id: 9,
            location: Location::default(),
            name: "TRAIT_ALIAS".to_string(),
            visibility: Visibility::Public,
            bounds: "Bounds".to_string(),
        }));
        assert_eq!(trait_alias.id(), 9);

        let plane = Definition::Plane(Rc::new(Plane {
            id: 6,
            location: Location::default(),
            value: "PLANE".to_string(),
        }));
        assert_eq!(plane.id(), 6);

        let union = Definition::Union(Rc::new(Union {
            id: 7,
            location: Location::default(),
            name: "UNION".to_string(),
            visibility: Visibility::Public,
            fields: vec![],
        }));
        assert_eq!(union.id(), 7);

        let empty = Definition::Empty;
        assert_eq!(empty.id(), 0);
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_definition_location() {
        let const_ = Definition::Const(Rc::new(Const {
            id: 1,
            location: Location::default(),
            name: "CONST".to_string(),
            visibility: Visibility::Public,
            type_: Type::T(String::new()),
            value: None,
        }));
        assert_eq!(const_.location(), Location::default());

        let enum_ = Definition::Enum(Rc::new(Enum {
            id: 2,
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        }));
        assert_eq!(enum_.location(), Location::default());

        let extern_crate = Definition::ExternCrate(Rc::new(ExternCrate {
            id: 3,
            location: Location::default(),
            name: "extern_crate".to_string(),
            visibility: Visibility::Public,
            alias: None,
        }));
        assert_eq!(extern_crate.location(), Location::default());

        let static_ = Definition::Static(Rc::new(Static {
            id: 4,
            location: Location::default(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::T(String::new()),
            value: Expression::Lit(Rc::new(Lit {
                id: 0,
                value: Literal::Int(Rc::new(LInt {
                    id: 0,
                    location: Location::default(),
                    value: 0,
                })),
                location: Location::default(),
            })),
        }));
        assert_eq!(static_.location(), Location::default());

        let module = Definition::Module(Rc::new(Module {
            id: 5,
            location: Location::default(),
            name: "MODULE".to_string(),
            visibility: Visibility::Public,
            definitions: None,
        }));
        assert_eq!(module.location(), Location::default());

        let plane = Definition::Plane(Rc::new(Plane {
            id: 6,
            location: Location::default(),
            value: "PLANE".to_string(),
        }));
        assert_eq!(plane.location(), Location::default());

        let union = Definition::Union(Rc::new(Union {
            id: 7,
            location: Location::default(),
            name: "UNION".to_string(),
            visibility: Visibility::Public,
            fields: vec![],
        }));
        assert_eq!(union.location(), Location::default());

        let trait_ = Definition::Trait(Rc::new(Trait {
            id: 8,
            location: Location::default(),
            name: "TRAIT".to_string(),
            visibility: Visibility::Public,
            supertraits: "SuperTrait".to_string(),
            items: vec![],
        }));
        assert_eq!(trait_.location(), Location::default());

        let trait_alias = Definition::TraitAlias(Rc::new(TraitAlias {
            id: 9,
            location: Location::default(),
            name: "TRAIT_ALIAS".to_string(),
            visibility: Visibility::Public,
            bounds: "Bounds".to_string(),
        }));
        assert_eq!(trait_alias.location(), Location::default());

        let contract = Definition::Contract(ContractType::Contract(Rc::new(Struct {
            id: 10,
            location: Location::default(),
            name: "CONTRACT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract.location(), Location::default());

        let contract_enum = Definition::Contract(ContractType::Enum(Rc::new(Enum {
            id: 11,
            location: Location::default(),
            name: "CONTRACT_ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract_enum.location(), Location::default());

        let contract_struct = Definition::Contract(ContractType::Struct(Rc::new(Struct {
            id: 12,
            location: Location::default(),
            name: "CONTRACT_STRUCT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        })));
        assert_eq!(contract_struct.location(), Location::default());

        let struct_ = Definition::Struct(Rc::new(Struct {
            id: 13,
            location: Location::default(),
            name: "STRUCT".to_string(),
            fields: vec![],
            methods: RefCell::new(vec![]),
            functions: RefCell::new(vec![]),
            type_aliases: RefCell::new(vec![]),
            constants: RefCell::new(vec![]),
            macros: RefCell::new(vec![]),
            plane_defs: RefCell::new(vec![]),
        }));
        assert_eq!(struct_.location(), Location::default());

        let directive = Definition::Directive(Directive::Use(Rc::new(Use {
            id: 14,
            location: Location::default(),
            visibility: Visibility::Public,
            path: String::new(),
        })));
        assert_eq!(directive.location(), Location::default());

        let custom_type = Definition::CustomType(Type::T("CustomType".to_string()));
        assert_eq!(custom_type.location(), Location::default());

        let macro_ = Definition::Macro(Rc::new(Macro {
            id: 15,
            location: Location::default(),
            name: "MACRO".to_string(),
            text: String::new(),
        }));
        assert_eq!(macro_.location(), Location::default());

        let function = Definition::Function(Rc::new(Function {
            id: 16,
            location: Location::default(),
            name: "FUNCTION".to_string(),
            visibility: Visibility::Public,
            body: None,
            parameters: Vec::new(),
            returns: crate::node_type::TypeNode::Empty,
        }));
        assert_eq!(function.location(), Location::default());

        let type_ = Definition::Type(Rc::new(T {
            id: 17,
            location: Location::default(),
            name: "TYPE".to_string(),
            visibility: Visibility::Public,
            ty: String::new(),
        }));
        assert_eq!(type_.location(), Location::default());

        let empty = Definition::Empty;
        assert_eq!(empty.location(), Location::default());
    }
}
