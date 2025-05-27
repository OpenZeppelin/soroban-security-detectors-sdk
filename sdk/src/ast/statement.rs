use crate::{ast_enum, ast_nodes, ast_nodes_impl};

use super::{
    definition::Definition,
    expression::Expression,
    misc::Macro,
    node::{Location, Node},
    node_type::NodeKind,
    pattern::Pattern,
};
use std::rc::Rc;

ast_enum! {
    pub enum Statement {
        @ty Definition(Definition),
        @ty Expression(Expression),
        Block(Rc<Block>),
        Let(Rc<Let>),
        Macro(Rc<Macro>),
    }
}

ast_nodes! {
    pub struct Block {
        pub statements: Vec<Statement>,
    }

    pub struct Let {
        pub name: String,
        pub pattern: Pattern,
        pub initial_value: Option<Expression>,
        pub initial_value_alternative: Option<Expression>,
    }
}

ast_nodes_impl! {
    impl Node for Block {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = NodeKind> {
            self.statements.iter().map(|s| NodeKind::Statement(s.clone()))
        }
    }
    impl Node for Let {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = NodeKind> {
            vec![].into_iter()
        }
    }
}
