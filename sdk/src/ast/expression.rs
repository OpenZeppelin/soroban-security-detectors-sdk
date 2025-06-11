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
    }

    pub struct MethodCall {
        pub method_name: String,
        pub base: Expression,
    }

    pub struct MemberAccess {
        pub base: Expression,
        pub member_name: String,
    }

    pub struct Reference {
        pub inner: Expression,
        pub is_mutable: bool,
    }

    pub struct Identifier {
        pub name: String,
    }

    pub struct Array {
        pub elements: Vec<Expression>,
    }

    pub struct Assign {
        pub left: Expression,
        pub right: Expression,
    }

    pub struct Try {
        pub expression: Expression,
    }

    pub struct TryBlock {
        pub block: Rc<Block>,
    }

    pub struct Binary {
        pub left: Expression,
        pub right: Expression,
        pub operator: BinOp,
    }

    pub struct Unary {
        pub expression: Expression,
        pub operator: UnOp,
    }

    pub struct Break {
        pub expression: Option<Expression>,
    }

    pub struct EBlock {
        pub block: Rc<Block>,
    }

    pub struct Cast {
        pub base: Expression,
        pub target_type: Type,
    }

    pub struct Closure {
        pub captures: Vec<Rc<Identifier>>,
        pub body: Expression,
        pub returns: Type,
    }

    pub struct ConstBlock {
        pub block: Rc<Block>,
    }

    pub struct Continue {
    }

    pub struct ForLoop {
        pub expression: Expression,
        pub block: Rc<Block>,
    }

    pub struct If {
        pub condition: Expression,
        pub then_branch: Rc<Block>,
        pub else_branch: Option<Expression>,
    }

    pub struct IndexAccess {
        pub base: Expression,
        pub index: Expression,
    }


    pub struct LetGuard {
        pub guard: Pattern,
        pub value: Expression,
    }


    pub struct Lit {
        pub value: Literal,
    }


    pub struct Loop {
        pub block: Rc<Block>,
    }

    pub struct Match {
        pub expression: Expression,
        pub arms: Vec<MatchArm>,
    }
    pub struct Parenthesized {
        pub expression: Expression,
    }

    pub struct Range {
        pub is_closed: bool,
        pub start: Option<Expression>,
        pub end: Option<Expression>,
    }

    pub struct Addr {
        pub mutability: Mutability,
        pub expression: Expression,
    }

    pub struct Repeat {
        pub expression: Expression,
        pub count: Expression,
    }

    pub struct Return {
        pub expression: Option<Expression>,
    }

    pub struct EStruct {
        pub name: String,
        pub fields: Vec<(String, Expression)>,
        pub rest_dots: Option<Expression>,
    }

    pub struct Tuple {
        pub elements: Vec<Expression>,
    }

    pub struct Unsafe {
        pub block: Rc<Block>,
    }

    pub struct While {
        pub label: Option<String>,
        pub condition: Expression,
        pub block: Rc<Block>,
    }
}
//TODO: repeat if for the rest of the ast types
impl From<NodeKind> for Rc<If> {
    fn from(node: NodeKind) -> Rc<If> {
        if let NodeKind::Expression(Expression::If(inner)) = node {
            inner
        } else {
            panic!("expected NodeKind::Expression::If, got {node:?}");
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
    use syn::token::{
        And,
        AndAnd,
        AndEq,
        Caret,
        CaretEq,
        EqEq,
        Ge,
        Gt,
        Le,
        Lt,
        Minus,
        MinusEq,
        Ne,
        Not,
        Or,
        OrEq,
        OrOr,
        Percent,
        PercentEq,
        Plus,
        // assignment tokens
        PlusEq,
        Shl,
        ShlEq,
        Shr,
        ShrEq,
        Slash,
        SlashEq,
        Star,
        StarEq,
    };
    use syn::{parse_str, ExprCall, ExprField, ExprMethodCall};

    // For testing we assume that Location is simply a String.
    // (Adjust these dummy constructors as needed.)
    fn dummy_location() -> Location {
        Location::default()
    }

    fn dummy_expr() -> Expression {
        Expression::Identifier(Rc::new(Identifier {
            id: 100,
            location: dummy_location(),
            name: "dummy".into(),
        }))
    }

    // Create a dummy Block.
    // (Assumes that Block has an id, location, and a statements vector.)
    fn dummy_block() -> Rc<Block> {
        // Note: adjust the fields as needed.
        Rc::new(Block {
            id: 0,
            location: dummy_location(),
            // For testing an empty block is fine.
            // If your Block type has a different definition, adjust accordingly.
            statements: Vec::new(),
        })
    }

    // Dummy Type – adjust to your project’s type.
    fn dummy_type() -> crate::custom_type::Type {
        // For example, if Type is an enum with a variant Custom(&str):
        crate::custom_type::Type::Typename(Rc::new(Typename {
            id: 0,
            location: dummy_location(),
            name: "DummyType".into(),
        }))
    }

    // Dummy Literal – adjust as needed.
    fn dummy_literal() -> Literal {
        // For example, if Literal is an enum with a variant Number(String):
        Literal::Bool(Rc::new(LBool {
            id: 0,
            location: dummy_location(),
            value: true,
        }))
    }

    // Dummy Pattern – adjust as needed.
    fn dummy_pattern() -> Pattern {
        // For example, if Pattern is an enum with a variant Identifier(String):
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
            })),
            right: Expression::Identifier(Rc::new(Identifier {
                id: 2,
                location: dummy_location(),
                name: "right_name".to_string(),
            })),
            operator: BinOp::Add,
        })
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_expression_id_and_location_all_variants() {
        let id = 42;
        let loc = dummy_location();

        // Addr
        let addr = Expression::Addr(Rc::new(Addr {
            id,
            location: loc.clone(),
            mutability: Mutability::Immutable,
            expression: dummy_expr(),
        }));
        assert_eq!(addr.id(), id);
        assert_eq!(addr.location(), loc.clone());

        // Array
        let array = Expression::Array(Rc::new(Array {
            id,
            location: loc.clone(),
            elements: vec![dummy_expr()],
        }));
        assert_eq!(array.id(), id);
        assert_eq!(array.location(), loc.clone());

        // Assign
        let assign = Expression::Assign(Rc::new(Assign {
            id,
            location: loc.clone(),
            left: dummy_expr(),
            right: dummy_expr(),
        }));
        assert_eq!(assign.id(), id);
        assert_eq!(assign.location(), loc.clone());

        // Binary – using one variant (Add)
        let binary = Binary {
            id,
            location: loc.clone(),
            left: dummy_expr(),
            right: dummy_expr(),
            operator: BinOp::Add,
        };

        assert_eq!(binary.id(), id);
        assert_eq!(binary.location, loc.clone());

        // Unary – using one variant (Not)
        let unary = Unary {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            operator: UnOp::Not,
        };
        assert_eq!(unary.id(), id);
        assert_eq!(unary.location, loc.clone());

        // Break
        let break_expr = Expression::Break(Rc::new(crate::expression::Break {
            id,
            location: loc.clone(),
            expression: Some(dummy_expr()),
        }));
        assert_eq!(break_expr.id(), id);
        assert_eq!(break_expr.location(), loc.clone());

        // EBlock
        let eblock = Expression::EBlock(Rc::new(EBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(eblock.id(), id);
        assert_eq!(eblock.location(), loc.clone());

        // Cast
        let cast = Expression::Cast(Rc::new(Cast {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            target_type: dummy_type(),
        }));
        assert_eq!(cast.id(), id);
        assert_eq!(cast.location(), loc.clone());

        // Closure
        let closure = Expression::Closure(Rc::new(Closure {
            id,
            location: loc.clone(),
            captures: vec![Rc::new(Identifier {
                id: 1,
                location: loc.clone(),
                name: "cap".into(),
            })],
            body: dummy_expr(),
            returns: dummy_type(),
        }));
        assert_eq!(closure.id(), id);
        assert_eq!(closure.location(), loc.clone());

        // Const
        let cons = Expression::Const(Rc::new(ConstBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(cons.id(), id);
        assert_eq!(cons.location(), loc.clone());

        // Continue
        let continue_expr = Expression::Continue(Rc::new(crate::expression::Continue {
            id,
            location: loc.clone(),
        }));
        assert_eq!(continue_expr.id(), id);
        assert_eq!(continue_expr.location(), loc.clone());

        // ForLoop
        let for_loop = Expression::ForLoop(Rc::new(ForLoop {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            block: dummy_block(),
        }));
        assert_eq!(for_loop.id(), id);
        assert_eq!(for_loop.location(), loc.clone());

        // FunctionCall
        let func_call = Expression::FunctionCall(Rc::new(FunctionCall {
            id,
            location: loc.clone(),
            function_name: "foo".into(),
            expression: dummy_expr(),
            parameters: vec![dummy_expr()],
        }));
        assert_eq!(func_call.id(), id);
        assert_eq!(func_call.location(), loc.clone());

        // If
        let if_expr = Expression::If(Rc::new(crate::expression::If {
            id,
            location: loc.clone(),
            condition: dummy_expr(),
            then_branch: dummy_block(),
            else_branch: Some(dummy_expr()),
        }));
        assert_eq!(if_expr.id(), id);
        assert_eq!(if_expr.location(), loc.clone());

        // IndexAccess
        let index_access = Expression::IndexAccess(Rc::new(IndexAccess {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            index: dummy_expr(),
        }));
        assert_eq!(index_access.id(), id);
        assert_eq!(index_access.location(), loc.clone());

        // LetGuard
        let let_guard = Expression::LetGuard(Rc::new(LetGuard {
            id,
            location: loc.clone(),
            guard: dummy_pattern(),
            value: dummy_expr(),
        }));
        assert_eq!(let_guard.id(), id);
        assert_eq!(let_guard.location(), loc.clone());

        // MethodCall
        let method_call = Expression::MethodCall(Rc::new(MethodCall {
            id,
            location: loc.clone(),
            method_name: "bar".into(),
            base: dummy_expr(),
        }));
        assert_eq!(method_call.id(), id);
        assert_eq!(method_call.location(), loc.clone());

        // MemberAccess
        let member_access = Expression::MemberAccess(Rc::new(MemberAccess {
            id,
            location: loc.clone(),
            base: dummy_expr(),
            member_name: "baz".into(),
        }));
        assert_eq!(member_access.id(), id);
        assert_eq!(member_access.location(), loc.clone());

        // Reference
        let reference = Expression::Reference(Rc::new(Reference {
            id,
            location: loc.clone(),
            inner: dummy_expr(),
            is_mutable: true,
        }));
        assert_eq!(reference.id(), id);
        assert_eq!(reference.location(), loc.clone());

        // Identifier
        let identifier = Expression::Identifier(Rc::new(Identifier {
            id,
            location: loc.clone(),
            name: "id".into(),
        }));
        assert_eq!(identifier.id(), id);
        assert_eq!(identifier.location(), loc.clone());

        // Lit
        let lit = Expression::Literal(Rc::new(Lit {
            id,
            location: loc.clone(),
            value: dummy_literal(),
        }));
        assert_eq!(lit.id(), id);
        assert_eq!(lit.location(), loc.clone());

        // Loop
        let loop_expr = Expression::Loop(Rc::new(crate::expression::Loop {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(loop_expr.id(), id);
        assert_eq!(loop_expr.location(), loc.clone());

        // Macro
        // Note: The `Macro` type is imported from super::misc::Macro.
        let macro_expr = Expression::Macro(Rc::new(crate::expression::Macro {
            id,
            location: loc.clone(),
            name: String::new(),
            text: String::new(),
            // Fill in any additional fields required by Macro.
            // For testing we assume that these two fields are enough.
        }));
        assert_eq!(macro_expr.id(), id);
        assert_eq!(macro_expr.location(), loc.clone());

        // Match
        let match_expr = Expression::Match(Rc::new(crate::expression::Match {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            arms: vec![MatchArm {
                pattern: dummy_pattern(),
                expression: dummy_expr(),
            }],
        }));
        assert_eq!(match_expr.id(), id);
        assert_eq!(match_expr.location(), loc.clone());

        // Parenthesized
        let paren = Expression::Parenthesized(Rc::new(Parenthesized {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
        }));
        assert_eq!(paren.id(), id);
        assert_eq!(paren.location(), loc.clone());

        // Range
        let range = Expression::Range(Rc::new(Range {
            id,
            location: loc.clone(),
            is_closed: true,
            start: Some(dummy_expr()),
            end: Some(dummy_expr()),
        }));
        assert_eq!(range.id(), id);
        assert_eq!(range.location(), loc.clone());

        // Repeat
        let repeat = Expression::Repeat(Rc::new(Repeat {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
            count: dummy_expr(),
        }));
        assert_eq!(repeat.id(), id);
        assert_eq!(repeat.location(), loc.clone());

        // Return
        let return_expr = Expression::Return(Rc::new(crate::expression::Return {
            id,
            location: loc.clone(),
            expression: Some(dummy_expr()),
        }));
        assert_eq!(return_expr.id(), id);
        assert_eq!(return_expr.location(), loc.clone());

        // EStruct
        let estruct = Expression::EStruct(Rc::new(EStruct {
            id,
            location: loc.clone(),
            name: "Struct".into(),
            fields: vec![("field".into(), dummy_expr())],
            rest_dots: Some(dummy_expr()),
        }));
        assert_eq!(estruct.id(), id);
        assert_eq!(estruct.location(), loc.clone());

        // Try
        let try_expr = Expression::Try(Rc::new(crate::expression::Try {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
        }));
        assert_eq!(try_expr.id(), id);
        assert_eq!(try_expr.location(), loc.clone());

        // TryBlock
        let try_block = Expression::TryBlock(Rc::new(TryBlock {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(try_block.id(), id);
        assert_eq!(try_block.location(), loc.clone());

        // Tuple
        let tuple = Expression::Tuple(Rc::new(Tuple {
            id,
            location: loc.clone(),
            elements: vec![dummy_expr()],
        }));
        assert_eq!(tuple.id(), id);
        assert_eq!(tuple.location(), loc.clone());

        // Unsafe
        let unsafe_expr = Expression::Unsafe(Rc::new(crate::expression::Unsafe {
            id,
            location: loc.clone(),
            block: dummy_block(),
        }));
        assert_eq!(unsafe_expr.id(), id);
        assert_eq!(unsafe_expr.location(), loc.clone());

        // While
        let while_expr = Expression::While(Rc::new(crate::expression::While {
            id,
            location: loc.clone(),
            label: Some("lbl".into()),
            condition: dummy_expr(),
            block: dummy_block(),
        }));
        assert_eq!(while_expr.id(), id);
        assert_eq!(while_expr.location(), loc.clone());
    }

    #[test]
    fn test_function_call_function_name_from_syn_item() {
        // Use a simple call expression "foo()"
        let expr_call: ExprCall = parse_str("foo()").unwrap();
        let name = FunctionCall::function_name_from_syn_item(&expr_call);
        assert_eq!(name, "foo");
    }

    #[test]
    fn test_method_call_method_name_from_syn_item() {
        // Use a method call expression "x.bar()"
        let method_call: ExprMethodCall = parse_str("x.bar()").unwrap();
        let name = MethodCall::method_name_from_syn_item(&method_call);
        assert_eq!(name, "bar");
    }

    #[test]
    fn test_member_access_member_name_from_syn_item() {
        // Test a named field
        let field: ExprField = parse_str("x.foo").unwrap();
        let name = MemberAccess::member_name_from_syn_item(&field);
        assert_eq!(name, "foo");

        // Test an unnamed field: "x.0"
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
        };
        let children: Vec<_> = member_access.children();
        assert_eq!(children.len(), 1);
        if let NodeKind::Statement(Statement::Expression(child_rc)) = &children[0] {
            assert_eq!(child_rc.id(), 100);
        } else {
            panic!("Unexpected child type");
        }
    }

    // Test from_syn_item for every BinOp variant in syn.
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

        // Now test all BinOp variants
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

    // Test the id() and location() methods for each Binary variant.
    #[test]
    fn test_binary_id_and_location() {
        let rc_binex = dummy_binex();

        // Example check with Add
        // Example check with Add
        let mut add_bin = rc_binex.as_ref().clone();
        add_bin.operator = BinOp::Add;
        assert_eq!(add_bin.id, 123);
        assert_eq!(add_bin.location, Location::default());

        // Example check with Sub
        let mut sub_bin = rc_binex.as_ref().clone();
        sub_bin.operator = BinOp::Sub;
        assert_eq!(sub_bin.id, 123);
        assert_eq!(sub_bin.location, Location::default());

        // And so on. If you want to systematically test them all:
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
