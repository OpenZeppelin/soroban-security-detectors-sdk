use crate::ast_node;

use super::node::Location;

ast_node! {
    pub struct Pattern {
        pub kind: String,
    }
}
