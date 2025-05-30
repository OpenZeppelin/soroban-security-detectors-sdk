use crate::{ast_enum, ast_nodes, ast_nodes_impl};

use super::{
    contract::Struct,
    custom_type::{Type, TypeAlias, Typename},
    directive::Directive,
    expression::Expression,
    function::Function,
    misc::{Field, Macro, Misc},
    node::{Location, Node, Visibility},
    node_type::{ContractType, NodeKind, RcFunction},
    statement::Statement,
};
use std::{cell::RefCell, rc::Rc};

ast_enum! {
    pub enum Definition {
        Const(Rc<Const>),
        ExternCrate(Rc<ExternCrate>),
        Enum(Rc<Enum>),
        Contract(Rc<Struct>),
        Struct(Rc<Struct>),
        Function(Rc<Function>),
        CustomType(Rc<CustomType>),
        Type(Type),
        Directive(Directive),
        Macro(Rc<Macro>),
        Module(Rc<Module>),
        Static(Rc<Static>),
        @skip Implementation(Rc<Implementation>),
        Trait(Rc<Trait>),
        TraitAlias(Rc<TraitAlias>),
        Plane(Rc<Plane>),
        Union(Rc<Union>),
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
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub variants: Vec<String>,
    }

    pub struct ExternCrate {
        pub name: String,
        pub visibility: Visibility,
        pub alias: Option<String>,
    }

    pub struct Static {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub mutable: bool,
        pub ty: Type,
        pub value: Expression,
    }

    pub struct Module {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub definitions: Option<Vec<Definition>>,
    }

    pub struct Plane {
        pub value: String,
    }

    pub struct Union {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub fields: Vec<Rc<Field>>,
    }

    pub struct Trait {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub supertraits: String,
        pub items: Vec<Definition>,
    }

    pub struct TraitAlias {
        pub attributes: Vec<String>,
        pub name: String,
        pub visibility: Visibility,
        pub bounds: String,
    }

    pub struct Implementation {
        pub attributes: Vec<String>,
        pub for_type: Option<Type>,
        pub functions: Vec<RcFunction>,
        pub constants: Vec<Rc<Const>>,
        pub type_aliases: Vec<Rc<TypeAlias>>,
        pub macros: Vec<Rc<Macro>>,
        pub plane_defs: Vec<Rc<Plane>>,
    }

    pub struct CustomType {
        pub name: String,
        pub visibility: Visibility,
        pub attributes: Vec<String>,
        pub ty: Type,
    }
}

ast_nodes_impl! {
    impl Node for Const {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            let mut children = vec![NodeKind::Type(self.type_.clone())];
            if let Some(expr) = &self.value {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for Enum {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for ExternCrate {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for Static {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Type(self.ty.clone()),
                NodeKind::Statement(Statement::Expression(self.value.clone())),
            ]
        }
    }
    impl Node for Module {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            if let Some(defs) = &self.definitions {
                defs.iter().cloned().map(NodeKind::Definition).collect()
            } else {
                vec![]
            }
        }
    }
    impl Node for Plane {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for Union {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.fields
                .iter()
                .map(|f| NodeKind::Misc(Misc::Field(f.clone())))
                .collect()
        }
    }
    impl Node for Trait {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.items
                .iter()
                .cloned()
                .map(NodeKind::Definition)
                .collect()
        }
    }
    impl Node for TraitAlias {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for Implementation {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            if let Some(ty) = &self.for_type {
                children.push(NodeKind::Type(ty.clone()));
            }
            children.extend(
                self.functions
                    .iter()
                    .cloned()
                    .map(|f| NodeKind::Definition(Definition::Function(f))),
            );
            children.extend(
                self.constants
                    .iter()
                    .cloned()
                    .map(|c| NodeKind::Definition(Definition::Const(c))),
            );
            children.extend(
                self.type_aliases
                    .iter()
                    .cloned()
                    .map(|ta| NodeKind::Definition(Definition::Type(Type::Alias(ta)))),
            );
            children.extend(
                self.macros
                    .iter()
                    .cloned()
                    .map(|m| NodeKind::Definition(Definition::Macro(m))),
            );
            children.extend(
                self.plane_defs
                    .iter()
                    .cloned()
                    .map(|p| NodeKind::Definition(Definition::Plane(p))),
            );
            children
        }
    }
    impl Node for CustomType {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Type(self.ty.clone())]
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        contract::Contract,
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
            type_: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: None,
        };
        assert_eq!(const_.id, 1);
    }

    #[test]
    fn test_enum_id() {
        let enum_ = Enum {
            id: 2,
            attributes: Vec::new(),
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
        };
        assert_eq!(enum_.id, 2);
    }
    #[test]
    fn test_enum_attrs() {
        use syn::parse_quote;
        // enum with two attributes
        let item: syn::ItemEnum = parse_quote! {
            #[contracterror]
            #[derive(Copy)]
            enum E { A, B }
        };
        let mut cb = crate::Codebase::<crate::OpenState>::default();
        let e = crate::ast_types_builder::build_enum(&mut cb, &item, 0);
        assert_eq!(
            e.attributes,
            vec!["contracterror".to_string(), "derive".to_string()]
        );
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
            attributes: Vec::new(),
            id: 4,
            location: Location::default(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: Expression::Literal(Rc::new(Lit {
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
            id: 9,
            location: Location::default(),
            name: "TRAIT_ALIAS".to_string(),
            visibility: Visibility::Public,
            bounds: "Bounds".to_string(),
        };
        assert_eq!(trait_alias.id, 9);
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_definition_id() {
        let const_ = Definition::Const(Rc::new(Const {
            id: 1,
            location: Location::default(),
            name: "CONST".to_string(),
            visibility: Visibility::Public,
            type_: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: None,
        }));
        assert_eq!(const_.id(), 1);

        let implementation = Definition::Implementation(Rc::new(Implementation {
            attributes: Vec::new(),
            id: 18,
            location: Location::default(),
            for_type: Some(Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: "ImplType".to_string(),
            }))),
            functions: vec![],
            constants: vec![],
            type_aliases: vec![],
            macros: vec![],
            plane_defs: vec![],
        }));
        assert_eq!(implementation.id(), 0); // Assuming Type::T has id 0

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
            attributes: Vec::new(),
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
        }));
        assert_eq!(enum_.id(), 2);

        let contract = Definition::Contract(Rc::new(Struct {
            id: 10,
            attributes: Vec::new(),
            location: Location::default(),
            name: "CONTRACT".to_string(),
            fields: vec![],
            is_contract: true,
        }));
        assert_eq!(contract.id(), 10);

        let contract_struct = Definition::Contract(Rc::new(Struct {
            id: 12,
            attributes: Vec::new(),
            location: Location::default(),
            name: "CONTRACT_STRUCT".to_string(),
            fields: vec![],
            is_contract: true,
        }));
        assert_eq!(contract_struct.id(), 12);

        let struct_ = Definition::Struct(Rc::new(Struct {
            id: 13,
            attributes: Vec::new(),
            location: Location::default(),
            name: "STRUCT".to_string(),
            fields: vec![],
            is_contract: false,
        }));
        assert_eq!(struct_.id(), 13);

        let directive = Definition::Directive(Directive::Use(Rc::new(Use {
            id: 14,
            location: Location::default(),
            visibility: Visibility::Public,
            path: String::new(),
            target: std::cell::RefCell::new(None),
        })));
        assert_eq!(directive.id(), 14);

        let custom_type = Definition::CustomType(Rc::new(CustomType {
            id: 0,
            location: Location::default(),
            name: "CustomType".to_string(),
            visibility: Visibility::Public,
            attributes: Vec::new(),
            ty: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: "CustomType".to_string(),
            })),
        }));
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
            attributes: Vec::new(),
            name: "MODULE".to_string(),
            visibility: Visibility::Public,
            definitions: None,
        }));
        assert_eq!(module.id(), 5);

        let static_ = Definition::Static(Rc::new(Static {
            id: 4,
            location: Location::default(),
            attributes: Vec::new(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: Expression::Literal(Rc::new(Lit {
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

        let type_ = Definition::Type(Type::Typename(Rc::new(Typename {
            id: 16,
            location: Location::default(),
            name: "TYPE".to_string(),
        })));
        assert_eq!(type_.id(), 16);

        let trait_ = Definition::Trait(Rc::new(Trait {
            id: 8,
            location: Location::default(),
            attributes: Vec::new(),
            name: "TRAIT".to_string(),
            visibility: Visibility::Public,
            supertraits: "SuperTrait".to_string(),
            items: vec![],
        }));
        assert_eq!(trait_.id(), 8);

        let trait_alias = Definition::TraitAlias(Rc::new(TraitAlias {
            id: 9,
            location: Location::default(),
            attributes: Vec::new(),
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
            attributes: Vec::new(),
            name: "UNION".to_string(),
            visibility: Visibility::Public,
            fields: vec![],
        }));
        assert_eq!(union.id(), 7);
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_definition_location() {
        let const_ = Definition::Const(Rc::new(Const {
            id: 1,
            location: Location::default(),
            name: "CONST".to_string(),
            visibility: Visibility::Public,
            type_: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: None,
        }));
        assert_eq!(const_.location(), Location::default());

        let implementation = Definition::Implementation(Rc::new(Implementation {
            attributes: Vec::new(),
            id: 18,
            location: Location::default(),
            for_type: Some(Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: "ImplType".to_string(),
            }))),
            functions: vec![],
            constants: vec![],
            type_aliases: vec![],
            macros: vec![],
            plane_defs: vec![],
        }));
        assert_eq!(implementation.location(), Location::default());

        let enum_ = Definition::Enum(Rc::new(Enum {
            id: 2,
            attributes: Vec::new(),
            location: Location::default(),
            name: "ENUM".to_string(),
            visibility: Visibility::Public,
            variants: vec!["Variant1".to_string()],
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
            attributes: Vec::new(),
            id: 4,
            location: Location::default(),
            name: "STATIC".to_string(),
            visibility: Visibility::Public,
            mutable: false,
            ty: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
            value: Expression::Literal(Rc::new(Lit {
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
            id: 7,
            location: Location::default(),
            name: "UNION".to_string(),
            visibility: Visibility::Public,
            fields: vec![],
        }));
        assert_eq!(union.location(), Location::default());

        let trait_ = Definition::Trait(Rc::new(Trait {
            attributes: Vec::new(),
            id: 8,
            location: Location::default(),
            name: "TRAIT".to_string(),
            visibility: Visibility::Public,
            supertraits: "SuperTrait".to_string(),
            items: vec![],
        }));
        assert_eq!(trait_.location(), Location::default());

        let trait_alias = Definition::TraitAlias(Rc::new(TraitAlias {
            attributes: Vec::new(),
            id: 9,
            location: Location::default(),
            name: "TRAIT_ALIAS".to_string(),
            visibility: Visibility::Public,
            bounds: "Bounds".to_string(),
        }));
        assert_eq!(trait_alias.location(), Location::default());

        let contract = Definition::Contract(Rc::new(Struct {
            id: 10,
            attributes: Vec::new(),
            location: Location::default(),
            name: "CONTRACT".to_string(),
            fields: vec![],
            is_contract: true,
        }));
        assert_eq!(contract.location(), Location::default());

        let contract_struct = Definition::Contract(Rc::new(Struct {
            id: 12,
            attributes: Vec::new(),
            location: Location::default(),
            name: "CONTRACT_STRUCT".to_string(),
            fields: vec![],
            is_contract: true,
        }));
        assert_eq!(contract_struct.location(), Location::default());

        let struct_ = Definition::Struct(Rc::new(Struct {
            id: 13,
            attributes: Vec::new(),
            location: Location::default(),
            name: "STRUCT".to_string(),
            fields: vec![],
            is_contract: false,
        }));
        assert_eq!(struct_.location(), Location::default());

        let directive = Definition::Directive(Directive::Use(Rc::new(Use {
            id: 14,
            location: Location::default(),
            visibility: Visibility::Public,
            path: String::new(),
            target: std::cell::RefCell::new(None),
        })));
        assert_eq!(directive.location(), Location::default());

        let custom_type = Definition::CustomType(Rc::new(CustomType {
            id: 0,
            location: Location::default(),
            name: "CustomType".to_string(),
            visibility: Visibility::Public,
            attributes: Vec::new(),
            ty: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: "CustomType".to_string(),
            })),
        }));
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
            attributes: Vec::new(),
            location: Location::default(),
            name: "FUNCTION".to_string(),
            visibility: Visibility::Public,
            generics: Vec::new(),
            parameters: Vec::new(),
            body: None,
            returns: Type::Typename(Rc::new(Typename {
                id: 0,
                location: Location::default(),
                name: String::new(),
            })),
        }));
        assert_eq!(function.location(), Location::default());

        let type_ = Definition::Type(Type::Typename(Rc::new(Typename {
            id: 17,
            location: Location::default(),
            name: "TYPE".to_string(),
        })));
        assert_eq!(type_.location(), Location::default());
    }
}
