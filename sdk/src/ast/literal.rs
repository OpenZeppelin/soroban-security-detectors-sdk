use std::rc::Rc;

use soroban_security_rules_macro_lib::node_location;

use super::node::{Location, TLocation};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Literal {
    String(Rc<LString>),
    BString(Rc<LBString>),
    CString(Rc<LCString>),
    Char(Rc<LChar>),
    Int(Rc<LInt>),
    Float(Rc<LFloat>),
    Bool(Rc<LBool>),
    Byte(Rc<LByte>),
}

impl Literal {
    #[must_use = "This function to take Literal id"]
    pub fn id(&self) -> u128 {
        match self {
            Literal::String(l_string) => l_string.id,
            Literal::BString(l_bstring) => l_bstring.id,
            Literal::CString(l_cstring) => l_cstring.id,
            Literal::Char(l_char) => l_char.id,
            Literal::Int(l_int) => l_int.id,
            Literal::Float(l_float) => l_float.id,
            Literal::Bool(l_bool) => l_bool.id,
            Literal::Byte(l_byte) => l_byte.id,
        }
    }

    #[must_use = "This function to take Literal location"]
    pub fn location(&self) -> Location {
        match self {
            Literal::String(l_string) => l_string.location.clone(),
            Literal::BString(l_bstring) => l_bstring.location.clone(),
            Literal::CString(l_cstring) => l_cstring.location.clone(),
            Literal::Char(l_char) => l_char.location.clone(),
            Literal::Int(l_int) => l_int.location.clone(),
            Literal::Float(l_float) => l_float.location.clone(),
            Literal::Bool(l_bool) => l_bool.location.clone(),
            Literal::Byte(l_byte) => l_byte.location.clone(),
        }
    }
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LString {
    pub id: u128,
    pub location: Location,
    pub value: String,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LBString {
    pub id: u128,
    pub location: Location,
    pub value: String,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LCString {
    pub id: u128,
    pub location: Location,
    pub value: String,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LChar {
    pub id: u128,
    pub location: Location,
    pub value: char,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LInt {
    pub id: u128,
    pub location: Location,
    pub value: i128,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LFloat {
    pub id: u128,
    pub location: Location,
    pub value: f64,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LBool {
    pub id: u128,
    pub location: Location,
    pub value: bool,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LByte {
    pub id: u128,
    pub location: Location,
    pub value: u8,
}
