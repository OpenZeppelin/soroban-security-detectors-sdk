//! AST node types for expressions.
//!
//! Defines the `Expression` enum and associated node structs representing
//! all Rust expression forms (literals, methods, operations, and control flow).
use crate::{ast_enum, ast_nodes, ast_nodes_impl};

use super::custom_type::Type;
use super::function::Function;
use super::literal::Literal;
use super::misc::Macro;
use super::node::{Location, Mutability, Node};
use super::node_type::NodeKind;
use super::pattern::Pattern;
use super::statement::{Block, Statement};
use std::rc::Rc;
use syn::{Expr, ExprCall, ExprMethodCall};

ast_enum! {
    pub enum Expression {
        Addr(Rc<Addr>),
        Array(Rc<Array>),
        Assign(Rc<Assign>),
        Binary(Rc<Binary>),
        Unary(Rc<Unary>),
        Break(Rc<Break>),
        EBlock(Rc<EBlock>),
        Cast(Rc<Cast>),
        Closure(Rc<Closure>),
        Const(Rc<ConstBlock>),
        Continue(Rc<Continue>),
        ForLoop(Rc<ForLoop>),
        FunctionCall(Rc<FunctionCall>),
        If(Rc<If>),
        IndexAccess(Rc<IndexAccess>),
        LetGuard(Rc<LetGuard>),
        MethodCall(Rc<MethodCall>),
        MemberAccess(Rc<MemberAccess>),
        Reference(Rc<Reference>),
        Identifier(Rc<Identifier>),
        Literal(Rc<Lit>),
        Loop(Rc<Loop>),
        Macro(Rc<Macro>),
        Match(Rc<Match>),
        Parenthesized(Rc<Parenthesized>),
        Range(Rc<Range>),
        Repeat(Rc<Repeat>),
        Return(Rc<Return>),
        EStruct(Rc<EStruct>),
        Try(Rc<Try>),
        TryBlock(Rc<TryBlock>),
        Tuple(Rc<Tuple>),
        Unsafe(Rc<Unsafe>),
        While(Rc<While>),
    }
}

impl Expression {
    #[must_use]
    pub fn is_ret(&self) -> bool {
        match self {
            Expression::Addr(addr) => addr.is_ret,
            Expression::Array(array) => array.is_ret,
            Expression::Assign(assign) => assign.is_ret,
            Expression::Binary(binary) => binary.is_ret,
            Expression::Unary(unary) => unary.is_ret,
            Expression::Break(_)
            | Expression::EBlock(_)
            | Expression::Const(_)
            | Expression::Continue(_)
            | Expression::TryBlock(_)
            | Expression::Unsafe(_)
            | Expression::Macro(_) => false,
            Expression::Cast(cast) => cast.is_ret,
            Expression::Closure(closure) => closure.is_ret,
            Expression::ForLoop(for_loop) => for_loop.is_ret,
            Expression::FunctionCall(function_call) => function_call.is_ret,
            Expression::If(if_expr) => if_expr.is_ret,
            Expression::IndexAccess(index_access) => index_access.is_ret,
            Expression::LetGuard(let_guard) => let_guard.is_ret,
            Expression::MethodCall(method_call) => method_call.is_ret,
            Expression::MemberAccess(member_access) => member_access.is_ret,
            Expression::Reference(reference) => reference.is_ret,
            Expression::Identifier(identifier) => identifier.is_ret,
            Expression::Literal(lit) => lit.is_ret,
            Expression::Loop(loop_expr) => loop_expr.is_ret,
            Expression::Match(m) => m.is_ret,
            Expression::Parenthesized(parenthesized) => parenthesized.is_ret,
            Expression::Range(range) => range.is_ret,
            Expression::Repeat(repeat) => repeat.is_ret,
            Expression::Return(_) => true,
            Expression::EStruct(estruct) => estruct.is_ret,
            Expression::Try(t) => t.is_ret,
            Expression::Tuple(tuple) => tuple.is_ret,
            Expression::While(while_expr) => while_expr.is_ret,
        }
    }
}

#[allow(clippy::module_name_repetitions)]
pub enum ExpressionParentType {
    Function(Rc<Function>),
    Expression(Rc<Expression>),
}

ast_nodes! {
    pub struct FunctionCall {
        pub function_name: String,
        pub expression: Expression,
        pub parameters: Vec<Expression>,
        pub is_ret: bool,
    }

    pub struct MethodCall {
        pub method_name: String,
        pub base: Expression,
        pub parameters: Vec<Expression>,
        pub is_ret: bool,
    }

    pub struct MemberAccess {
        pub base: Expression,
        pub member_name: String,
        pub is_ret: bool,
    }

    pub struct Reference {
        pub inner: Expression,
        pub is_mutable: bool,
        pub is_ret: bool,
    }

    pub struct Identifier {
        pub name: String,
        pub is_ret: bool,
    }

    pub struct Array {
        pub elements: Vec<Expression>,
        pub is_ret: bool,
    }

    pub struct Assign {
        pub left: Expression,
        pub right: Expression,
        pub is_ret: bool,
    }

    pub struct Try {
        pub expression: Expression,
        pub is_ret: bool,
    }

    pub struct TryBlock {
        pub block: Rc<Block>,
    }

    pub struct Binary {
        pub left: Expression,
        pub right: Expression,
        pub operator: BinOp,
        pub is_ret: bool,
    }

    pub struct Unary {
        pub expression: Expression,
        pub operator: UnOp,
        pub is_ret: bool,
    }

    pub struct Break {
        pub expression: Option<Expression>,
        pub is_ret: bool,
    }

    pub struct EBlock {
        pub block: Rc<Block>,
    }

    pub struct Cast {
        pub base: Expression,
        pub target_type: Type,
        pub is_ret: bool,
    }

    pub struct Closure {
        pub captures: Vec<Rc<Identifier>>,
        pub body: Expression,
        pub returns: Type,
        pub is_ret: bool,
    }

    pub struct ConstBlock {
        pub block: Rc<Block>,
    }

    pub struct Continue {
    }

    pub struct ForLoop {
        pub expression: Expression,
        pub block: Rc<Block>,
        pub is_ret: bool,
    }

    pub struct If {
        pub condition: Expression,
        pub then_branch: Rc<Block>,
        pub else_branch: Option<Expression>,
        pub is_ret: bool,
    }

    pub struct IndexAccess {
        pub base: Expression,
        pub index: Expression,
        pub is_ret: bool,
    }

    pub struct LetGuard {
        pub guard: Pattern,
        pub value: Expression,
        pub is_ret: bool,
    }

    pub struct Lit {
        pub value: Literal,
        pub is_ret: bool,
    }

    pub struct Loop {
        pub block: Rc<Block>,
        pub is_ret: bool,
    }

    pub struct Match {
        pub expression: Expression,
        pub arms: Vec<MatchArm>,
        pub is_ret: bool,
    }
    pub struct Parenthesized {
        pub expression: Expression,
        pub is_ret: bool,
    }

    pub struct Range {
        pub is_closed: bool,
        pub start: Option<Expression>,
        pub end: Option<Expression>,
        pub is_ret: bool,
    }

    pub struct Addr {
        pub mutability: Mutability,
        pub expression: Expression,
        pub is_ret: bool,
    }

    pub struct Repeat {
        pub expression: Expression,
        pub count: Expression,
        pub is_ret: bool,
    }

    pub struct Return {
        pub expression: Option<Expression>,
        pub is_ret: bool,
    }

    pub struct EStruct {
        pub name: String,
        pub fields: Vec<(String, Expression)>,
        pub rest_dots: Option<Expression>,
        pub is_ret: bool,
    }

    pub struct Tuple {
        pub elements: Vec<Expression>,
        pub is_ret: bool,
    }

    pub struct Unsafe {
        pub block: Rc<Block>,
    }

    pub struct While {
        pub label: Option<String>,
        pub condition: Expression,
        pub block: Rc<Block>,
        pub is_ret: bool,
    }
}

impl From<NodeKind> for Rc<If> {
    fn from(node: NodeKind) -> Rc<If> {
        match node {
             NodeKind::Statement(Statement::Expression(Expression::If(inner))) | NodeKind::Expression(Expression::If(inner)) => inner,
             _ => panic!("expected NodeKind::Expression::If or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Assign> {
    fn from(node: NodeKind) -> Rc<Assign> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Assign(inner))) | NodeKind::Expression(Expression::Assign(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Assign or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<MethodCall> {
    fn from(node: NodeKind) -> Rc<MethodCall> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::MethodCall(inner))) | NodeKind::Expression(Expression::MethodCall(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::MethodCall or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Addr> {
    fn from(node: NodeKind) -> Rc<Addr> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Addr(inner))) | NodeKind::Expression(Expression::Addr(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Addr or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Array> {
    fn from(node: NodeKind) -> Rc<Array> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Array(inner))) | NodeKind::Expression(Expression::Array(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Array or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Binary> {
    fn from(node: NodeKind) -> Rc<Binary> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Binary(inner))) | NodeKind::Expression(Expression::Binary(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Binary or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Unary> {
    fn from(node: NodeKind) -> Rc<Unary> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Unary(inner))) | NodeKind::Expression(Expression::Unary(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Unary or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Break> {
    fn from(node: NodeKind) -> Rc<Break> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Break(inner))) | NodeKind::Expression(Expression::Break(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Break or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<EBlock> {
    fn from(node: NodeKind) -> Rc<EBlock> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::EBlock(inner))) | NodeKind::Expression(Expression::EBlock(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::EBlock or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Cast> {
    fn from(node: NodeKind) -> Rc<Cast> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Cast(inner))) | NodeKind::Expression(Expression::Cast(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Cast or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Closure> {
    fn from(node: NodeKind) -> Rc<Closure> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Closure(inner))) | NodeKind::Expression(Expression::Closure(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Closure or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<ConstBlock> {
    fn from(node: NodeKind) -> Rc<ConstBlock> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Const(inner))) | NodeKind::Expression(Expression::Const(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Const or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Continue> {
    fn from(node: NodeKind) -> Rc<Continue> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Continue(inner))) | NodeKind::Expression(Expression::Continue(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Continue or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<ForLoop> {
    fn from(node: NodeKind) -> Rc<ForLoop> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::ForLoop(inner))) | NodeKind::Expression(Expression::ForLoop(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::ForLoop or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<FunctionCall> {
    fn from(node: NodeKind) -> Rc<FunctionCall> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::FunctionCall(inner))) | NodeKind::Expression(Expression::FunctionCall(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::FunctionCall or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<IndexAccess> {
    fn from(node: NodeKind) -> Rc<IndexAccess> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::IndexAccess(inner))) | NodeKind::Expression(Expression::IndexAccess(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::IndexAccess or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<LetGuard> {
    fn from(node: NodeKind) -> Rc<LetGuard> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::LetGuard(inner))) | NodeKind::Expression(Expression::LetGuard(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::LetGuard or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<MemberAccess> {
    fn from(node: NodeKind) -> Rc<MemberAccess> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::MemberAccess(inner))) | NodeKind::Expression(Expression::MemberAccess(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::MemberAccess or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Reference> {
    fn from(node: NodeKind) -> Rc<Reference> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Reference(inner))) | NodeKind::Expression(Expression::Reference(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Reference or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Identifier> {
    fn from(node: NodeKind) -> Rc<Identifier> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Identifier(inner))) | NodeKind::Expression(Expression::Identifier(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Identifier or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Lit> {
    fn from(node: NodeKind) -> Rc<Lit> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Literal(inner))) | NodeKind::Expression(Expression::Literal(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Literal or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Loop> {
    fn from(node: NodeKind) -> Rc<Loop> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Loop(inner))) | NodeKind::Expression(Expression::Loop(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Loop or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Macro> {
    fn from(node: NodeKind) -> Rc<Macro> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Macro(inner))) | NodeKind::Expression(Expression::Macro(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Macro or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Match> {
    fn from(node: NodeKind) -> Rc<Match> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Match(inner))) | NodeKind::Expression(Expression::Match(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Match or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Parenthesized> {
    fn from(node: NodeKind) -> Rc<Parenthesized> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Parenthesized(inner))) | NodeKind::Expression(Expression::Parenthesized(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Parenthesized or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Range> {
    fn from(node: NodeKind) -> Rc<Range> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Range(inner))) | NodeKind::Expression(Expression::Range(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Range or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Repeat> {
    fn from(node: NodeKind) -> Rc<Repeat> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Repeat(inner))) | NodeKind::Expression(Expression::Repeat(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Repeat or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Return> {
    fn from(node: NodeKind) -> Rc<Return> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Return(inner))) | NodeKind::Expression(Expression::Return(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Return or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<EStruct> {
    fn from(node: NodeKind) -> Rc<EStruct> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::EStruct(inner))) | NodeKind::Expression(Expression::EStruct(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::EStruct or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Try> {
    fn from(node: NodeKind) -> Rc<Try> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Try(inner))) | NodeKind::Expression(Expression::Try(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Try or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<TryBlock> {
    fn from(node: NodeKind) -> Rc<TryBlock> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::TryBlock(inner))) | NodeKind::Expression(Expression::TryBlock(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::TryBlock or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Tuple> {
    fn from(node: NodeKind) -> Rc<Tuple> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Tuple(inner))) | NodeKind::Expression(Expression::Tuple(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Tuple or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<Unsafe> {
    fn from(node: NodeKind) -> Rc<Unsafe> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::Unsafe(inner))) | NodeKind::Expression(Expression::Unsafe(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::Unsafe or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

impl From<NodeKind> for Rc<While> {
    fn from(node: NodeKind) -> Rc<While> {
        match node {
            NodeKind::Statement(Statement::Expression(Expression::While(inner))) | NodeKind::Expression(Expression::While(inner)) => inner,
            _ => panic!("expected NodeKind::Expression::While or NodeKind::Statement::Expression, got {node:?}"),
        }
    }
}

ast_nodes_impl! {
    impl Node for FunctionCall {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.parameters
                .iter()
                .map(|expr| NodeKind::Statement(Statement::Expression(expr.clone())))
                .collect()
        }
    }
    impl Node for MethodCall {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.base.clone()))]
        }
    }
    impl Node for MemberAccess {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.base.clone()))]
        }
    }
    impl Node for Reference {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.inner.clone()))]
        }
    }
    impl Node for Identifier {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for Array {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            self.elements
                .iter()
                .map(|expr| NodeKind::Statement(Statement::Expression(expr.clone())))
                .collect()
        }
    }
    impl Node for Assign {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.left.clone())),
                NodeKind::Statement(Statement::Expression(self.right.clone())),
            ]
        }
    }
    impl Node for Try {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.expression.clone()))]
        }
    }
    impl Node for TryBlock {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Block(self.block.clone()))]
        }
    }
    impl Node for Binary {
        #[allow(refining_impl_trait)]
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.left.clone())),
                NodeKind::Statement(Statement::Expression(self.right.clone())),
            ]
        }
    }
    impl Node for Unary {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.expression.clone()))]
        }
    }
    impl Node for Break {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            if let Some(expr) = &self.expression {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for EBlock {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Block(self.block.clone()))]
        }
    }
    impl Node for Cast {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.base.clone())),
                NodeKind::Type(self.target_type.clone()),
            ]
        }
    }
    impl Node for Closure {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            for cap in &self.captures {
                children.push(NodeKind::Statement(Statement::Expression(Expression::Identifier(cap.clone()))));
            }
            children.push(NodeKind::Statement(Statement::Expression(self.body.clone())));
            children.push(NodeKind::Type(self.returns.clone()));
            children
        }
    }
    impl Node for ConstBlock {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Block(self.block.clone()))]
        }
    }
    impl Node for Continue {
        fn children(&self) -> Vec<NodeKind> {
            vec![]
        }
    }
    impl Node for ForLoop {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.expression.clone())),
                NodeKind::Statement(Statement::Block(self.block.clone())),
            ]
        }
    }
    impl Node for If {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = vec![
                NodeKind::Statement(Statement::Expression(self.condition.clone())),
                NodeKind::Statement(Statement::Block(self.then_branch.clone())),
            ];
            if let Some(expr) = &self.else_branch {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for IndexAccess {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.base.clone())),
                NodeKind::Statement(Statement::Expression(self.index.clone())),
            ]
        }
    }
    impl Node for LetGuard {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Pattern(self.guard.clone()),
                NodeKind::Statement(Statement::Expression(self.value.clone())),
            ]
        }
    }
    impl Node for Lit {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Literal(self.value.clone())]
        }
    }
    impl Node for Loop {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Block(self.block.clone()))]
        }
    }
    impl Node for Match {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = vec![NodeKind::Statement(Statement::Expression(self.expression.clone()))];
            for arm in &self.arms {
                children.push(NodeKind::Pattern(arm.pattern.clone()));
                children.push(NodeKind::Statement(Statement::Expression(arm.expression.clone())));
            }
            children
        }
    }
    impl Node for Parenthesized {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.expression.clone()))]
        }
    }
    impl Node for Range {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            if let Some(expr) = &self.start {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            if let Some(expr) = &self.end {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for Addr {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Expression(self.expression.clone()))]
        }
    }
    impl Node for Repeat {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.expression.clone())),
                NodeKind::Statement(Statement::Expression(self.count.clone())),
            ]
        }
    }
    impl Node for Return {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = Vec::new();
            if let Some(expr) = &self.expression {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for EStruct {
        fn children(&self) -> Vec<NodeKind> {
            let mut children = self
                .fields
                .iter()
                .map(|(_, expr)| NodeKind::Statement(Statement::Expression(expr.clone())))
                .collect::<Vec<_>>();
            if let Some(expr) = &self.rest_dots {
                children.push(NodeKind::Statement(Statement::Expression(expr.clone())));
            }
            children
        }
    }
    impl Node for Tuple {
        fn children(&self) -> Vec<NodeKind> {
            self.elements
                .iter()
                .map(|expr| NodeKind::Statement(Statement::Expression(expr.clone())))
                .collect()
        }
    }
    impl Node for Unsafe {
        fn children(&self) -> Vec<NodeKind> {
            vec![NodeKind::Statement(Statement::Block(self.block.clone()))]
        }
    }
    impl Node for While {
        fn children(&self) -> Vec<NodeKind> {
            vec![
                NodeKind::Statement(Statement::Expression(self.condition.clone())),
                NodeKind::Statement(Statement::Block(self.block.clone())),
            ]
        }
    }
}

impl MethodCall {
    #[must_use]
    pub fn method_name_from_syn_item(method_call: &ExprMethodCall) -> String {
        method_call.method.to_string()
    }
}

impl FunctionCall {
    #[must_use]
    pub fn function_name_from_syn_item(function_call: &ExprCall) -> String {
        match function_call.func.as_ref() {
            Expr::Path(ref expr_path) => expr_path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        }
    }
}
impl MemberAccess {
    #[must_use]
    pub fn member_name_from_syn_item(item: &syn::ExprField) -> String {
        match &item.member {
            syn::Member::Named(ident) => ident.to_string(),
            syn::Member::Unnamed(index) => index.index.to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
}

impl BinOp {
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn from_syn_item(syn_binop: &syn::BinOp) -> Self {
        match syn_binop {
            syn::BinOp::Add(_plus) => BinOp::Add,
            syn::BinOp::Sub(_minus) => BinOp::Sub,
            syn::BinOp::Mul(_star) => BinOp::Mul,
            syn::BinOp::Div(_slash) => BinOp::Div,
            syn::BinOp::Rem(_percent) => BinOp::Mod,
            syn::BinOp::And(_and_and) => BinOp::And,
            syn::BinOp::Or(_or_or) => BinOp::Or,
            syn::BinOp::BitXor(_caret) => BinOp::BitXor,
            syn::BinOp::BitAnd(_and) => BinOp::BitAnd,
            syn::BinOp::BitOr(_or) => BinOp::BitOr,
            syn::BinOp::Shl(_shl) => BinOp::Shl,
            syn::BinOp::Shr(_shr) => BinOp::Shr,
            syn::BinOp::Eq(_eq_eq) => BinOp::Eq,
            syn::BinOp::Lt(_lt) => BinOp::Lt,
            syn::BinOp::Le(_le) => BinOp::Le,
            syn::BinOp::Ne(_ne) => BinOp::Ne,
            syn::BinOp::Ge(_ge) => BinOp::Ge,
            syn::BinOp::Gt(_gt) => BinOp::Gt,
            syn::BinOp::AddAssign(_plus_eq) => BinOp::AddAssign,
            syn::BinOp::SubAssign(_minus_eq) => BinOp::SubAssign,
            syn::BinOp::MulAssign(_star_eq) => BinOp::MulAssign,
            syn::BinOp::DivAssign(_slash_eq) => BinOp::DivAssign,
            syn::BinOp::RemAssign(_percent_eq) => BinOp::ModAssign,
            syn::BinOp::BitXorAssign(_caret_eq) => BinOp::BitXorAssign,
            syn::BinOp::BitAndAssign(_and_eq) => BinOp::BitAndAssign,
            syn::BinOp::BitOrAssign(_or_eq) => BinOp::BitOrAssign,
            syn::BinOp::ShlAssign(_shl_eq) => BinOp::ShlAssign,
            syn::BinOp::ShrAssign(_shr_eq) => BinOp::ShrAssign,
            _ => panic!("Unexpected binary operator"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum UnOp {
    Deref,
    Not,
    Neg,
}

impl UnOp {
    #[must_use]
    pub fn from_syn_item(syn_unop: &syn::UnOp) -> Self {
        match syn_unop {
            syn::UnOp::Deref(_) => UnOp::Deref,
            syn::UnOp::Not(_) => UnOp::Not,
            syn::UnOp::Neg(_) => UnOp::Neg,
            _ => todo!(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

#[cfg(test)]
mod tests {
    use crate::custom_type::Typename;
    use crate::literal::LBool;
    use crate::statement::Statement;

    use super::*;
    use std::rc::Rc;

    use syn::{parse_str, ExprCall, ExprField, ExprMethodCall};

    fn dummy_location() -> Location {
        Location::default()
    }

    fn dummy_expr() -> Expression {
        Expression::Identifier(Rc::new(Identifier {
            id: 100,
            location: dummy_location(),
            name: "dummy".into(),
            is_ret: false,
        }))
    }

    fn dummy_block() -> Rc<Block> {
        Rc::new(Block {
            id: 0,
            location: dummy_location(),
            statements: Vec::new(),
        })
    }

    fn dummy_type() -> crate::custom_type::Type {
        crate::custom_type::Type::Typename(Rc::new(Typename {
            id: 0,
            location: dummy_location(),
            name: "DummyType".into(),
        }))
    }

    fn dummy_literal() -> Literal {
        Literal::Bool(Rc::new(LBool {
            id: 0,
            location: dummy_location(),
            value: true,
        }))
    }

    fn dummy_pattern() -> Pattern {
        Pattern {
            id: 0,
            kind: String::new(),
            location: Location::default(),
        }
    }

    fn dummy_binex() -> Rc<Binary> {
        Rc::new(Binary {
            id: 123,
            location: dummy_location(),
            left: Expression::Identifier(Rc::new(Identifier {
                id: 1,
                location: dummy_location(),
                name: "left_name".to_string(),
                is_ret: false,
            })),
            right: Expression::Identifier(Rc::new(Identifier {
                id: 2,
                location: dummy_location(),
                name: "right_name".to_string(),
                is_ret: false,
            })),
            operator: BinOp::Add,
            is_ret: false,
        })
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_expression_id_and_location_all_variants() {
        let id = 42;
        let loc = dummy_location();

        let addr = Expression::Addr(Rc::new(Addr {
            id,
            location: loc.clone(),
            mutability: Mutability::Immutable,
            expression: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(addr.id(), id);
        assert_eq!(addr.location(), loc.clone());

        let array = Expression::Array(Rc::new(Array {
            id,
            location: loc.clone(),
            elements: vec![dummy_expr()],
            is_ret: false,
        }));
        assert_eq!(array.id(), id);
        assert_eq!(array.location(), loc.clone());

        let assign = Expression::Assign(Rc::new(Assign {
            id,
            location: loc.clone(),
            left: dummy_expr(),
            right: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(assign.id(), id);
        assert_eq!(assign.location(), loc.clone());

        let binary = Binary {
            id,
            location: loc.clone(),
            left: dummy_expr(),
            right: dummy_expr(),
            operator: BinOp::Add,
            is_ret: false,
        };

        assert_eq!(binary.id(), id);
        assert_eq!(binary.location, loc.clone());

        let unary = Unary {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            operator: UnOp::Not,
            is_ret: false,
        };
        assert_eq!(unary.id(), id);
        assert_eq!(unary.location, loc.clone());

        let break_expr = Expression::Break(Rc::new(crate::expression::Break {
            id,
            location: loc.clone(),
            expression: Some(dummy_expr()),
            is_ret: false,
        }));
        assert_eq!(break_expr.id(), id);
        assert_eq!(break_expr.location(), loc.clone());

        let eblock = Expression::EBlock(Rc::new(EBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(eblock.id(), id);
        assert_eq!(eblock.location(), loc.clone());

        let cast = Expression::Cast(Rc::new(Cast {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            target_type: dummy_type(),
            is_ret: false,
        }));
        assert_eq!(cast.id(), id);
        assert_eq!(cast.location(), loc.clone());

        let closure = Expression::Closure(Rc::new(Closure {
            id,
            location: loc.clone(),
            captures: vec![Rc::new(Identifier {
                id: 1,
                location: loc.clone(),
                name: "cap".into(),
                is_ret: false,
            })],
            body: dummy_expr(),
            returns: dummy_type(),
            is_ret: false,
        }));
        assert_eq!(closure.id(), id);
        assert_eq!(closure.location(), loc.clone());

        let cons = Expression::Const(Rc::new(ConstBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(cons.id(), id);
        assert_eq!(cons.location(), loc.clone());

        let continue_expr = Expression::Continue(Rc::new(crate::expression::Continue {
            id,
            location: loc.clone(),
        }));
        assert_eq!(continue_expr.id(), id);
        assert_eq!(continue_expr.location(), loc.clone());

        let for_loop = Expression::ForLoop(Rc::new(ForLoop {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            block: dummy_block(),
            is_ret: false,
        }));
        assert_eq!(for_loop.id(), id);
        assert_eq!(for_loop.location(), loc.clone());

        let func_call = Expression::FunctionCall(Rc::new(FunctionCall {
            id,
            location: loc.clone(),
            function_name: "foo".into(),
            expression: dummy_expr(),
            parameters: vec![dummy_expr()],
            is_ret: false,
        }));
        assert_eq!(func_call.id(), id);
        assert_eq!(func_call.location(), loc.clone());

        let if_expr = Expression::If(Rc::new(crate::expression::If {
            id,
            location: loc.clone(),
            condition: dummy_expr(),
            then_branch: dummy_block(),
            else_branch: Some(dummy_expr()),
            is_ret: false,
        }));
        assert_eq!(if_expr.id(), id);
        assert_eq!(if_expr.location(), loc.clone());

        let index_access = Expression::IndexAccess(Rc::new(IndexAccess {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            index: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(index_access.id(), id);
        assert_eq!(index_access.location(), loc.clone());

        let let_guard = Expression::LetGuard(Rc::new(LetGuard {
            id,
            location: loc.clone(),
            guard: dummy_pattern(),
            value: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(let_guard.id(), id);
        assert_eq!(let_guard.location(), loc.clone());

        let method_call = Expression::MethodCall(Rc::new(MethodCall {
            id,
            location: loc.clone(),
            method_name: "bar".into(),
            base: dummy_expr(),
            parameters: vec![dummy_expr()],
            is_ret: false,
        }));
        assert_eq!(method_call.id(), id);
        assert_eq!(method_call.location(), loc.clone());

        let member_access = Expression::MemberAccess(Rc::new(MemberAccess {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            member_name: "baz".into(),
            is_ret: true,
        }));
        assert_eq!(member_access.id(), id);
        assert_eq!(member_access.location(), loc.clone());

        let reference = Expression::Reference(Rc::new(Reference {
            id,
            location: loc.clone(),
            inner: dummy_expr(),
            is_mutable: true,
            is_ret: false,
        }));
        assert_eq!(reference.id(), id);
        assert_eq!(reference.location(), loc.clone());

        let identifier = Expression::Identifier(Rc::new(Identifier {
            id,
            location: loc.clone(),
            name: "id".into(),
            is_ret: false,
        }));
        assert_eq!(identifier.id(), id);
        assert_eq!(identifier.location(), loc.clone());

        let lit = Expression::Literal(Rc::new(Lit {
            id,
            location: loc.clone(),
            value: dummy_literal(),
            is_ret: false,
        }));
        assert_eq!(lit.id(), id);
        assert_eq!(lit.location(), loc.clone());

        let loop_expr = Expression::Loop(Rc::new(crate::expression::Loop {
            id,
            location: loc.clone(),
            block: dummy_block(),
            is_ret: false,
        }));
        assert_eq!(loop_expr.id(), id);
        assert_eq!(loop_expr.location(), loc.clone());

        let macro_expr = Expression::Macro(Rc::new(crate::expression::Macro {
            id,
            location: loc.clone(),
            name: String::new(),
            text: String::new(),
        }));
        assert_eq!(macro_expr.id(), id);
        assert_eq!(macro_expr.location(), loc.clone());

        let match_expr = Expression::Match(Rc::new(crate::expression::Match {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            arms: vec![MatchArm {
                pattern: dummy_pattern(),
                expression: dummy_expr(),
            }],
            is_ret: false,
        }));
        assert_eq!(match_expr.id(), id);
        assert_eq!(match_expr.location(), loc.clone());

        let paren = Expression::Parenthesized(Rc::new(Parenthesized {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(paren.id(), id);
        assert_eq!(paren.location(), loc.clone());

        let range = Expression::Range(Rc::new(Range {
            id,
            location: loc.clone(),
            is_closed: true,
            start: Some(dummy_expr()),
            end: Some(dummy_expr()),
            is_ret: false,
        }));
        assert_eq!(range.id(), id);
        assert_eq!(range.location(), loc.clone());

        let repeat = Expression::Repeat(Rc::new(Repeat {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            count: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(repeat.id(), id);
        assert_eq!(repeat.location(), loc.clone());

        let return_expr = Expression::Return(Rc::new(crate::expression::Return {
            id,
            location: loc.clone(),
            expression: Some(dummy_expr()),
            is_ret: false,
        }));
        assert_eq!(return_expr.id(), id);
        assert_eq!(return_expr.location(), loc.clone());

        let estruct = Expression::EStruct(Rc::new(EStruct {
            id,
            location: loc.clone(),
            name: "Struct".into(),
            fields: vec![("field".into(), dummy_expr())],
            rest_dots: Some(dummy_expr()),
            is_ret: false,
        }));
        assert_eq!(estruct.id(), id);
        assert_eq!(estruct.location(), loc.clone());

        let try_expr = Expression::Try(Rc::new(crate::expression::Try {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            is_ret: false,
        }));
        assert_eq!(try_expr.id(), id);
        assert_eq!(try_expr.location(), loc.clone());

        let try_block = Expression::TryBlock(Rc::new(TryBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(try_block.id(), id);
        assert_eq!(try_block.location(), loc.clone());

        let tuple = Expression::Tuple(Rc::new(Tuple {
            id,
            location: loc.clone(),
            elements: vec![dummy_expr()],
            is_ret: false,
        }));
        assert_eq!(tuple.id(), id);
        assert_eq!(tuple.location(), loc.clone());

        let unsafe_expr = Expression::Unsafe(Rc::new(crate::expression::Unsafe {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(unsafe_expr.id(), id);
        assert_eq!(unsafe_expr.location(), loc.clone());

        let while_expr = Expression::While(Rc::new(crate::expression::While {
            id,
            location: loc.clone(),
            label: Some("lbl".into()),
            condition: dummy_expr(),
            block: dummy_block(),
            is_ret: false,
        }));
        assert_eq!(while_expr.id(), id);
        assert_eq!(while_expr.location(), loc.clone());
    }

    #[test]
    fn test_function_call_function_name_from_syn_item() {
        let expr_call: ExprCall = parse_str("foo()").unwrap();
        let name = FunctionCall::function_name_from_syn_item(&expr_call);
        assert_eq!(name, "foo");
    }

    #[test]
    fn test_method_call_method_name_from_syn_item() {
        let method_call: ExprMethodCall = parse_str("x.bar()").unwrap();
        let name = MethodCall::method_name_from_syn_item(&method_call);
        assert_eq!(name, "bar");
    }

    #[test]
    fn test_member_access_member_name_from_syn_item() {
        let field: ExprField = parse_str("x.foo").unwrap();
        let name = MemberAccess::member_name_from_syn_item(&field);
        assert_eq!(name, "foo");

        let field: ExprField = parse_str("x.0").unwrap();
        let name = MemberAccess::member_name_from_syn_item(&field);
        assert_eq!(name, "0");
    }

    #[test]
    #[allow(irrefutable_let_patterns)]
    fn test_function_call_children() {
        let expr = dummy_expr();
        let func_call = FunctionCall {
            id: 1,
            location: dummy_location(),
            function_name: "foo".into(),
            expression: expr.clone(),
            parameters: vec![expr.clone()],
            is_ret: false,
        };
        let children: Vec<NodeKind> = func_call.children();
        assert_eq!(children.len(), 1);
        if let NodeKind::Statement(Statement::Expression(ref child_expr)) = children[0] {
            assert_eq!(child_expr.id(), 100);
        } else {
            panic!("Unexpected child type");
        }
    }

    #[test]
    #[allow(irrefutable_let_patterns)]
    fn test_method_call_children() {
        let base = dummy_expr();
        let method_call = MethodCall {
            id: 2,
            location: dummy_location(),
            method_name: "bar".into(),
            base: base.clone(),
            parameters: vec![base.clone()],
            is_ret: false,
        };
        let children: Vec<_> = method_call.children();
        assert_eq!(children.len(), 1);
        if let NodeKind::Statement(Statement::Expression(child_rc)) = &children[0] {
            assert_eq!(child_rc.id(), 100);
        } else {
            panic!("Unexpected child type");
        }
    }

    #[test]
    #[allow(irrefutable_let_patterns)]
    fn test_member_access_children() {
        let base = dummy_expr();
        let member_access = MemberAccess {
            id: 3,
            location: dummy_location(),
            base: base.clone(),
            member_name: "baz".into(),
            is_ret: true,
        };
        let children: Vec<_> = member_access.children();
        assert_eq!(children.len(), 1);
        if let NodeKind::Statement(Statement::Expression(child_rc)) = &children[0] {
            assert_eq!(child_rc.id(), 100);
        } else {
            panic!("Unexpected child type");
        }
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_binary_operator_field_all_ops() {
        fn check_op(bin: &Binary, expected: &BinOp) {
            assert_eq!(bin.operator, *expected);
            assert_eq!(bin.id, 123);
        }

        let rc_binex = dummy_binex();

        let bin = rc_binex.as_ref();
        check_op(bin, &BinOp::Add);

        let all_ops = [
            BinOp::Add,
            BinOp::Sub,
            BinOp::Mul,
            BinOp::Div,
            BinOp::Mod,
            BinOp::And,
            BinOp::Or,
            BinOp::BitXor,
            BinOp::BitAnd,
            BinOp::BitOr,
            BinOp::Shl,
            BinOp::Shr,
            BinOp::Eq,
            BinOp::Lt,
            BinOp::Le,
            BinOp::Ne,
            BinOp::Ge,
            BinOp::Gt,
            BinOp::AddAssign,
            BinOp::SubAssign,
            BinOp::MulAssign,
            BinOp::DivAssign,
            BinOp::ModAssign,
            BinOp::BitXorAssign,
            BinOp::BitAndAssign,
            BinOp::BitOrAssign,
            BinOp::ShlAssign,
            BinOp::ShrAssign,
        ];

        for op in &all_ops {
            let mut bin = rc_binex.as_ref().clone();
            bin.operator = op.clone();
            check_op(&bin, op);
        }
    }

    #[test]
    fn test_binary_id_and_location() {
        let rc_binex = dummy_binex();

        let mut add_bin = rc_binex.as_ref().clone();
        add_bin.operator = BinOp::Add;
        assert_eq!(add_bin.id, 123);
        assert_eq!(add_bin.location, Location::default());

        let mut sub_bin = rc_binex.as_ref().clone();
        sub_bin.operator = BinOp::Sub;
        assert_eq!(sub_bin.id, 123);
        assert_eq!(sub_bin.location, Location::default());

        let operators = vec![
            BinOp::Add,
            BinOp::Sub,
            BinOp::Mul,
            BinOp::Div,
            BinOp::Mod,
            BinOp::And,
            BinOp::Or,
            BinOp::BitXor,
            BinOp::BitAnd,
            BinOp::BitOr,
            BinOp::Shl,
            BinOp::Shr,
            BinOp::Eq,
            BinOp::Lt,
            BinOp::Le,
            BinOp::Ne,
            BinOp::Ge,
            BinOp::Gt,
            BinOp::AddAssign,
            BinOp::SubAssign,
            BinOp::MulAssign,
            BinOp::DivAssign,
            BinOp::ModAssign,
            BinOp::BitXorAssign,
            BinOp::BitAndAssign,
            BinOp::BitOrAssign,
            BinOp::ShlAssign,
            BinOp::ShrAssign,
        ];

        for variant in operators {
            let mut bin = rc_binex.as_ref().clone();
            bin.operator = variant;
            assert_eq!(bin.id, 123);
            assert_eq!(bin.location, Location::default());
        }
    }
}
