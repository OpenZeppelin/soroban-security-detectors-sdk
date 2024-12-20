#![warn(clippy::pedantic)]

use std::rc::Rc;

use super::{contract::Contract, function::Function};

pub type RcFile = Rc<syn::File>;
pub type RcContract = Rc<Contract>;
pub type RcFunction = Rc<Function>;

pub enum NodeType {
    File(RcFile),
    Contract(RcContract),
    Function(RcFunction),
    Struct,
    Enum,
}

pub enum FileChildType {
    Contract(RcContract),
}

pub enum ContractParentType {
    File(RcFile),
}

pub enum ContractChildType {
    Function(RcFunction),
}

pub enum FunctionParentType {
    File(RcFile),
    Contract(RcContract),
}

pub enum FunctionChildType {
    Statement,
}
