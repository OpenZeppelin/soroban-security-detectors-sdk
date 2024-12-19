#![warn(clippy::pedantic)]

use std::rc::Rc;

use super::contract::Contract;

pub enum NodeType {
    File(Rc<syn::File>),
    Contract(Rc<Contract>),
    Function,
    Struct,
    Enum,
}
