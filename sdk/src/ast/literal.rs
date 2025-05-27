use std::rc::Rc;

use crate::{ast_enum, ast_nodes, ast_nodes_impl};

use super::{node::Location, node::Node, node_type::NodeKind};

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

ast_nodes_impl! {
    impl Node for LString {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LBString {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LCString {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LChar {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LInt {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LFloat {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LBool {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
    impl Node for LByte {
        #[allow(refining_impl_trait)]
        fn children(&self) -> impl Iterator<Item = Rc<NodeKind>> {
            vec![].into_iter()
        }
    }
}
