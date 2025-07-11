//! AST node types for statements and blocks.
//!
//! Defines the `Statement` enum and node structs for `Block` and `Let` statements.
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
        Definition(Definition),
        Expression(Expression),
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
        fn children(&self) -> Vec<NodeKind> {
            self.statements.iter().map(|s| NodeKind::Statement(s.clone())).collect()
        }
    }
    impl Node for Let {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            children.push(NodeKind::Pattern(self.pattern.clone()));
            if let Some(expr) = &self.initial_value {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            if let Some(expr) = &self.initial_value_alternative {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
}

impl From<NodeKind> for Rc<Let> {
    fn from(node: NodeKind) -> Rc<Let> {
        if let NodeKind::Statement(Statement::Let(inner)) = node {
            inner
        } else {
            panic!("expected NodeKind::Statement::Let, got {node:?}");
        }
    }
}
