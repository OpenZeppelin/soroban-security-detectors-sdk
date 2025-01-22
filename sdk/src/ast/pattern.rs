use soroban_security_rules_macro_lib::node_location;

use super::node::{Location, TLocation};

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Pattern {
    pub kind: String, //TODO implement me
    pub location: Location,
}
