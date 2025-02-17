use std::rc::Rc;

use soroban_security_rules_macro_lib::node_location;

use crate::ast_nodes;

use super::node::{Location, TLocation};

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
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

ast_nodes! {
    pub struct LString {
        pub value: String,
    }

    pub struct LBString {
        pub value: String,
    }

    pub struct LCString {
        pub value: String,
    }

    pub struct LChar {
        pub value: char,
    }

    pub struct LInt {
        pub value: i128,
    }

    pub struct LFloat {
        pub value: String,
    }

    pub struct LBool {
        pub value: bool,
    }

    pub struct LByte {
        pub value: u8,
    }
}
