use crate::{ast_node, ast_node_impl};

use super::{
    node::{Location, Node},
    node_type::NodeKind,
};

ast_node! {
    pub struct Pattern {
        pub kind: String,
    }
}
ast_node_impl! {
    impl Node for Pattern {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
}
