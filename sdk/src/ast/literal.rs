use std::rc::Rc;

use crate::{ast_enum, ast_nodes};

use super::node::Location;

ast_enum! {
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
