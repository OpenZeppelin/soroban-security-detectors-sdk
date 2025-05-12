use crate::{ast_enum, ast_nodes};

use super::custom_type::Type;
use super::function::Function;
use super::literal::Literal;
use super::misc::Macro;
use super::node::{Location, Mutability, Node};
use super::node_type::{FunctionCallChildType, MemberAccessChildType, MethodCallChildType};
use super::pattern::Pattern;
use super::statement::Block;
use std::rc::Rc;
use syn::{Expr, ExprCall, ExprMethodCall};

ast_enum! {
    pub enum Expression {
        Addr(Rc<Addr>),
        Array(Rc<Array>),
        Assign(Rc<Assign>),
        @ty Binary(Binary),
        @ty Unary(Unary),
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
        Lit(Rc<Lit>),
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
        Async(Rc<Async>),
        Await(Rc<Await>),
        While(Rc<While>),
        Yield(Rc<Yield>),
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

    pub struct BinEx {
        pub left: Expression,
        pub right: Expression,
    }

    pub struct UnEx {
        pub expression: Expression,
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
    /// A yield expression `yield expr` in generator contexts
    pub struct Yield {
        pub expression: Option<Expression>,
    }
    /// An async block expression `async { ... }`
    pub struct Async {
        pub tokens: String,
    }
    /// An await expression `expr.await`
    pub struct Await {
        pub tokens: String,
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
impl Node for FunctionCall {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = FunctionCallChildType> {
        self.parameters
            .iter()
            .map(|param| FunctionCallChildType::Expression(Rc::new(param.clone())))
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

impl Node for MethodCall {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = MethodCallChildType> {
        vec![Rc::new(self.base.clone())]
            .into_iter()
            .map(MethodCallChildType::Expression)
    }
}

impl MethodCall {
    #[must_use]
    pub fn method_name_from_syn_item(method_call: &ExprMethodCall) -> String {
        method_call.method.to_string()
    }
}

impl Node for MemberAccess {
    #[allow(refining_impl_trait)]
    fn children(&self) -> impl Iterator<Item = MemberAccessChildType> {
        vec![Rc::new(self.base.clone())]
            .into_iter()
            .map(MemberAccessChildType::Expression)
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

type RcBinEx = Rc<BinEx>;

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Binary {
    Add(RcBinEx),
    Sub(RcBinEx),
    Mul(RcBinEx),
    Div(RcBinEx),
    Mod(RcBinEx),
    And(RcBinEx),
    Or(RcBinEx),
    BitXor(RcBinEx),
    BitAnd(RcBinEx),
    BitOr(RcBinEx),
    Shl(RcBinEx),
    Shr(RcBinEx),
    Eq(RcBinEx),
    Lt(RcBinEx),
    Le(RcBinEx),
    Ne(RcBinEx),
    Ge(RcBinEx),
    Gt(RcBinEx),
    AddAssign(RcBinEx),
    SubAssign(RcBinEx),
    MulAssign(RcBinEx),
    DivAssign(RcBinEx),
    ModAssign(RcBinEx),
    BitXorAssign(RcBinEx),
    BitAndAssign(RcBinEx),
    BitOrAssign(RcBinEx),
    ShlAssign(RcBinEx),
    ShrAssign(RcBinEx),
}

impl Binary {
    /// Converts a syn binary operation to a Binary enum.
    ///
    /// # Panics
    ///
    /// Panics if an unexpected binary operator is encountered.
    #[must_use]
    pub fn from_syn_item(binary: RcBinEx, syn_binop: &syn::BinOp) -> Binary {
        match syn_binop {
            syn::BinOp::Add(_plus) => Binary::Add(binary),
            syn::BinOp::Sub(_minus) => Binary::Sub(binary),
            syn::BinOp::Mul(_star) => Binary::Mul(binary),
            syn::BinOp::Div(_slash) => Binary::Div(binary),
            syn::BinOp::Rem(_percent) => Binary::Mod(binary),
            syn::BinOp::And(_and_and) => Binary::And(binary),
            syn::BinOp::Or(_or_or) => Binary::Or(binary),
            syn::BinOp::BitXor(_caret) => Binary::BitXor(binary),
            syn::BinOp::BitAnd(_and) => Binary::BitAnd(binary),
            syn::BinOp::BitOr(_or) => Binary::BitOr(binary),
            syn::BinOp::Shl(_shl) => Binary::Shl(binary),
            syn::BinOp::Shr(_shr) => Binary::Shr(binary),
            syn::BinOp::Eq(_eq_eq) => Binary::Eq(binary),
            syn::BinOp::Lt(_lt) => Binary::Lt(binary),
            syn::BinOp::Le(_le) => Binary::Le(binary),
            syn::BinOp::Ne(_ne) => Binary::Ne(binary),
            syn::BinOp::Ge(_ge) => Binary::Ge(binary),
            syn::BinOp::Gt(_gt) => Binary::Gt(binary),
            syn::BinOp::AddAssign(_plus_eq) => Binary::AddAssign(binary),
            syn::BinOp::SubAssign(_minus_eq) => Binary::SubAssign(binary),
            syn::BinOp::MulAssign(_star_eq) => Binary::MulAssign(binary),
            syn::BinOp::DivAssign(_slash_eq) => Binary::DivAssign(binary),
            syn::BinOp::RemAssign(_percent_eq) => Binary::ModAssign(binary),
            syn::BinOp::BitXorAssign(_caret_eq) => Binary::BitXorAssign(binary),
            syn::BinOp::BitAndAssign(_and_eq) => Binary::BitAndAssign(binary),
            syn::BinOp::BitOrAssign(_or_eq) => Binary::BitOrAssign(binary),
            syn::BinOp::ShlAssign(_shl_eq) => Binary::ShlAssign(binary),
            syn::BinOp::ShrAssign(_shr_eq) => Binary::ShrAssign(binary),
            _ => panic!("Unexpected binary operator"),
        }
    }

    #[must_use = "This method returns the id of the binary expression."]
    pub fn id(&self) -> u32 {
        match self {
            Binary::Add(binex)
            | Binary::Sub(binex)
            | Binary::Mul(binex)
            | Binary::Div(binex)
            | Binary::Mod(binex)
            | Binary::And(binex)
            | Binary::Or(binex)
            | Binary::BitXor(binex)
            | Binary::BitAnd(binex)
            | Binary::BitOr(binex)
            | Binary::Shl(binex)
            | Binary::Shr(binex)
            | Binary::Eq(binex)
            | Binary::Lt(binex)
            | Binary::Le(binex)
            | Binary::Ne(binex)
            | Binary::Ge(binex)
            | Binary::Gt(binex)
            | Binary::AddAssign(binex)
            | Binary::SubAssign(binex)
            | Binary::MulAssign(binex)
            | Binary::DivAssign(binex)
            | Binary::ModAssign(binex)
            | Binary::BitXorAssign(binex)
            | Binary::BitAndAssign(binex)
            | Binary::BitOrAssign(binex)
            | Binary::ShlAssign(binex)
            | Binary::ShrAssign(binex) => binex.id,
        }
    }

    #[must_use = "This method returns the location of the binary expression."]
    pub fn location(&self) -> Location {
        match self {
            Binary::Add(binex)
            | Binary::Sub(binex)
            | Binary::Mul(binex)
            | Binary::Div(binex)
            | Binary::Mod(binex)
            | Binary::And(binex)
            | Binary::Or(binex)
            | Binary::BitXor(binex)
            | Binary::BitAnd(binex)
            | Binary::BitOr(binex)
            | Binary::Shl(binex)
            | Binary::Shr(binex)
            | Binary::Eq(binex)
            | Binary::Lt(binex)
            | Binary::Le(binex)
            | Binary::Ne(binex)
            | Binary::Ge(binex)
            | Binary::Gt(binex)
            | Binary::AddAssign(binex)
            | Binary::SubAssign(binex)
            | Binary::MulAssign(binex)
            | Binary::DivAssign(binex)
            | Binary::ModAssign(binex)
            | Binary::BitXorAssign(binex)
            | Binary::BitAndAssign(binex)
            | Binary::BitOrAssign(binex)
            | Binary::ShlAssign(binex)
            | Binary::ShrAssign(binex) => binex.location.clone(),
        }
    }
}

type RcUnEx = Rc<UnEx>;

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Unary {
    Deref(RcUnEx),
    Not(RcUnEx),
    Neg(RcUnEx),
}

impl Unary {
    /// Converts a syn unary operation to a Unary enum.
    ///
    /// # Panics
    ///
    /// Panics if an unexpected unary operator is encountered.
    #[must_use]
    pub fn from_syn_item(unary: RcUnEx, syn_unop: &syn::UnOp) -> Unary {
        match syn_unop {
            syn::UnOp::Deref(_) => Unary::Deref(unary),
            syn::UnOp::Not(_) => Unary::Not(unary),
            syn::UnOp::Neg(_) => Unary::Neg(unary),
            _ => todo!(),
        }
    }

    #[must_use = "This method returns the id of the unary expression."]
    pub fn id(&self) -> u32 {
        match self {
            Unary::Deref(unex) | Unary::Not(unex) | Unary::Neg(unex) => unex.id,
        }
    }

    #[must_use = "This method returns the location of the unary expression."]
    pub fn location(&self) -> Location {
        match self {
            Unary::Deref(unex) | Unary::Not(unex) | Unary::Neg(unex) => unex.location.clone(),
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
    use crate::literal::LBool;

    use super::*;
    use std::rc::Rc;
    use syn::{parse_str, BinOp, ExprCall, ExprField, ExprMethodCall, UnOp};
    // import only needed token structs for operator tests
    use syn::token::{
        Plus, Minus, Star, Slash, Percent,
        AndAnd, OrOr, Caret, And, Or,
        Shl, Shr, EqEq, Lt, Le, Ne, Ge, Gt, Not,
        // assignment tokens
        PlusEq, MinusEq, StarEq, SlashEq, PercentEq,
        CaretEq, AndEq, OrEq, ShlEq, ShrEq,
    };

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
        crate::custom_type::Type::T("dummy_type".into())
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

    fn dummy_binex() -> Rc<BinEx> {
        Rc::new(BinEx {
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
        let bin_ex = BinEx {
            id,
            location: loc.clone(),
            left: dummy_expr(),
            right: dummy_expr(),
        };
        let rc_bin_ex = Rc::new(bin_ex);
        let binary = Expression::Binary(Binary::Add(rc_bin_ex.clone()));
        assert_eq!(binary.id(), id);
        assert_eq!(binary.location(), loc.clone());

        // Unary – using one variant (Not)
        let un_ex = UnEx {
            id,
            location: loc.clone(),
            expression: dummy_expr(),
        };
        let rc_un_ex = Rc::new(un_ex);
        let unary = Expression::Unary(Unary::Not(rc_un_ex.clone()));
        assert_eq!(unary.id(), id);
        assert_eq!(unary.location(), loc.clone());

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
        let lit = Expression::Lit(Rc::new(Lit {
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

        // Yield
        let yield_expr = Expression::Yield(Rc::new(Yield {
            id,
            location: loc.clone(),
            expression: Some(dummy_expr()),
        }));
        assert_eq!(yield_expr.id(), id);
        assert_eq!(yield_expr.location(), loc.clone());
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
    fn test_binary_from_syn_item() {
        let dummy_binex = BinEx {
            id: 55,
            location: dummy_location(),
            left: dummy_expr(),
            right: dummy_expr(),
        };
        let rc_binex = Rc::new(dummy_binex);
        let op = BinOp::Add(Plus::default());
        let binary = Binary::from_syn_item(rc_binex.clone(), &op);
        assert_eq!(binary.id(), 55);
        assert_eq!(binary.location(), dummy_location());
    }

    #[test]
    fn test_unary_from_syn_item() {
        let dummy_unex = UnEx {
            id: 66,
            location: dummy_location(),
            expression: dummy_expr(),
        };
        let rc_unex = Rc::new(dummy_unex);
        let op = UnOp::Not(Not::default());
        let unary = Unary::from_syn_item(rc_unex.clone(), &op);
        assert_eq!(unary.id(), 66);
        assert_eq!(unary.location(), dummy_location());
        let op = UnOp::Deref(Star::default());
        let unary = Unary::from_syn_item(rc_unex.clone(), &op);
        assert_eq!(unary.id(), 66);
        assert_eq!(unary.location(), dummy_location());
        let op = UnOp::Neg(Minus::default());
        let unary = Unary::from_syn_item(rc_unex.clone(), &op);
        assert_eq!(unary.id(), 66);
        assert_eq!(unary.location(), dummy_location());
    }

    #[test]
    #[allow(irrefutable_let_patterns)]
    fn test_function_call_children() {
        let expr = dummy_expr();
        let func_call = FunctionCall {
            id: 1,
            location: dummy_location(),
            function_name: "foo".into(),
            parameters: vec![expr.clone()],
        };
        let children: Vec<_> = func_call.children().collect();
        assert_eq!(children.len(), 1);
        if let FunctionCallChildType::Expression(child_rc) = &children[0] {
            assert_eq!(child_rc.id(), 100);
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
        let children: Vec<_> = method_call.children().collect();
        assert_eq!(children.len(), 1);
        if let MethodCallChildType::Expression(child_rc) = &children[0] {
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
        let children: Vec<_> = member_access.children().collect();
        assert_eq!(children.len(), 1);
        if let MemberAccessChildType::Expression(child_rc) = &children[0] {
            assert_eq!(child_rc.id(), 100);
        } else {
            panic!("Unexpected child type");
        }
    }

    // Test from_syn_item for every BinOp variant in syn.
    #[test]
    #[allow(clippy::too_many_lines)]
    fn test_binary_from_syn_item_all_ops() {
        let rc_binex = dummy_binex();

        // Add
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Add(Plus::default()));
        match bin {
            Binary::Add(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Add"),
        }

        // Sub
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Sub(Minus::default()));
        match bin {
            Binary::Sub(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Sub"),
        }

        // Mul
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Mul(Star::default()));
        match bin {
            Binary::Mul(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Mul"),
        }

        // Div
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Div(Slash::default()));
        match bin {
            Binary::Div(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Div"),
        }

        // Rem
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Rem(Percent::default()));
        match bin {
            Binary::Mod(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Mod"),
        }

        // And
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::And(AndAnd::default()));
        match bin {
            Binary::And(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected And"),
        }

        // Or
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Or(OrOr::default()));
        match bin {
            Binary::Or(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Or"),
        }

        // BitXor
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitXor(Caret::default()));
        match bin {
            Binary::BitXor(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitXor"),
        }

        // BitAnd
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitAnd(And::default()));
        match bin {
            Binary::BitAnd(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitAnd"),
        }

        // BitOr
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitOr(Or::default()));
        match bin {
            Binary::BitOr(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitOr"),
        }

        // Shl
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Shl(Shl::default()));
        match bin {
            Binary::Shl(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Shl"),
        }

        // Shr
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Shr(Shr::default()));
        match bin {
            Binary::Shr(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Shr"),
        }

        // Eq
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Eq(EqEq::default()));
        match bin {
            Binary::Eq(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Eq"),
        }

        // Lt
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Lt(Lt::default()));
        match bin {
            Binary::Lt(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Lt"),
        }

        // Le
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Le(Le::default()));
        match bin {
            Binary::Le(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Le"),
        }

        // Ne
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Ne(Ne::default()));
        match bin {
            Binary::Ne(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Ne"),
        }

        // Ge
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Ge(Ge::default()));
        match bin {
            Binary::Ge(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Ge"),
        }

        // Gt
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::Gt(Gt::default()));
        match bin {
            Binary::Gt(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected Gt"),
        }

        // AddAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::AddAssign(PlusEq::default()));
        match bin {
            Binary::AddAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected AddAssign"),
        }

        // SubAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::SubAssign(MinusEq::default()));
        match bin {
            Binary::SubAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected SubAssign"),
        }

        // MulAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::MulAssign(StarEq::default()));
        match bin {
            Binary::MulAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected MulAssign"),
        }

        // DivAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::DivAssign(SlashEq::default()));
        match bin {
            Binary::DivAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected DivAssign"),
        }

        // RemAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::RemAssign(PercentEq::default()));
        match bin {
            Binary::ModAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected ModAssign"),
        }

        // BitXorAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitXorAssign(CaretEq::default()));
        match bin {
            Binary::BitXorAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitXorAssign"),
        }

        // BitAndAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitAndAssign(AndEq::default()));
        match bin {
            Binary::BitAndAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitAndAssign"),
        }

        // BitOrAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::BitOrAssign(OrEq::default()));
        match bin {
            Binary::BitOrAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected BitOrAssign"),
        }

        // ShlAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::ShlAssign(ShlEq::default()));
        match bin {
            Binary::ShlAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected ShlAssign"),
        }

        // ShrAssign
        let bin = Binary::from_syn_item(rc_binex.clone(), &BinOp::ShrAssign(ShrEq::default()));
        match bin {
            Binary::ShrAssign(b) => assert_eq!(b.id, 123),
            _ => panic!("Expected ShrAssign"),
        }
    }

    // Test the id() and location() methods for each Binary variant.
    #[test]
    fn test_binary_id_and_location() {
        let rc_binex = dummy_binex();

        // Example check with Add
        let add_bin = Binary::Add(rc_binex.clone());
        assert_eq!(add_bin.id(), 123);
        assert_eq!(add_bin.location(), Location::default());

        // Example check with Sub
        let sub_bin = Binary::Sub(rc_binex.clone());
        assert_eq!(sub_bin.id(), 123);
        assert_eq!(sub_bin.location(), Location::default());

        // And so on. If you want to systematically test them all:
        let variants = vec![
            Binary::Add(rc_binex.clone()),
            Binary::Sub(rc_binex.clone()),
            Binary::Mul(rc_binex.clone()),
            Binary::Div(rc_binex.clone()),
            Binary::Mod(rc_binex.clone()),
            Binary::And(rc_binex.clone()),
            Binary::Or(rc_binex.clone()),
            Binary::BitXor(rc_binex.clone()),
            Binary::BitAnd(rc_binex.clone()),
            Binary::BitOr(rc_binex.clone()),
            Binary::Shl(rc_binex.clone()),
            Binary::Shr(rc_binex.clone()),
            Binary::Eq(rc_binex.clone()),
            Binary::Lt(rc_binex.clone()),
            Binary::Le(rc_binex.clone()),
            Binary::Ne(rc_binex.clone()),
            Binary::Ge(rc_binex.clone()),
            Binary::Gt(rc_binex.clone()),
            Binary::AddAssign(rc_binex.clone()),
            Binary::SubAssign(rc_binex.clone()),
            Binary::MulAssign(rc_binex.clone()),
            Binary::DivAssign(rc_binex.clone()),
            Binary::ModAssign(rc_binex.clone()),
            Binary::BitXorAssign(rc_binex.clone()),
            Binary::BitAndAssign(rc_binex.clone()),
            Binary::BitOrAssign(rc_binex.clone()),
            Binary::ShlAssign(rc_binex.clone()),
            Binary::ShrAssign(rc_binex.clone()),
        ];

        for variant in variants {
            assert_eq!(variant.id(), 123);
            assert_eq!(variant.location(), Location::default());
        }
    }
}
