use soroban_security_rules_macro_lib::node_location;

use crate::ast_node;

use super::node::{Location, TLocation};

ast_node! {
    pub struct Pattern {
        pub kind: String,
    }
}
