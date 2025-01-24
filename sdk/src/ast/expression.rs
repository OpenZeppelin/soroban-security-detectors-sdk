#![warn(clippy::pedantic)]
use super::custom_type::Type;
use super::function::Function;
use super::literal::Literal;
use super::node::{Location, Mutability, Node, TLocation};
use super::node_type::{FunctionCallChildType, MemberAccessChildType, MethodCallChildType};
use super::pattern::Pattern;
use super::statement::Block;
use soroban_security_rules_macro_lib::node_location;
use std::rc::Rc;
use syn::{Expr, ExprCall, ExprMethodCall};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    Addr(Rc<Addr>),
    Array(Rc<Array>),
    Assign(Rc<Assign>),
    Binary(Binary),
    Unary(Unary),
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
    While(Rc<While>),
    Yield(Rc<Yeild>),
}

impl Expression {
    #[must_use = "This method returns the id of the expression."]
    pub fn id(&self) -> u128 {
        match self {
            Expression::Addr(a) => a.id,
            Expression::Array(a) => a.id,
            Expression::Assign(a) => a.id,
            Expression::Binary(b) => b.id(),
            Expression::Unary(u) => u.id(),
            Expression::Break(b) => b.id,
            Expression::EBlock(e) => e.id,
            Expression::Cast(c) => c.id,
            Expression::Closure(c) => c.id,
            Expression::Const(c) => c.id,
            Expression::Continue(c) => c.id,
            Expression::ForLoop(f) => f.id,
            Expression::FunctionCall(f) => f.id,
            Expression::If(i) => i.id,
            Expression::IndexAccess(i) => i.id,
            Expression::LetGuard(l) => l.id,
            Expression::MethodCall(m) => m.id,
            Expression::MemberAccess(m) => m.id,
            Expression::Reference(r) => r.id,
            Expression::Identifier(i) => i.id,
            Expression::Lit(l) => l.id,
            Expression::Loop(l) => l.id,
            Expression::Macro(m) => m.id,
            Expression::Match(m) => m.id,
            Expression::Parenthesized(p) => p.id,
            Expression::Range(r) => r.id,
            Expression::Repeat(r) => r.id,
            Expression::Return(r) => r.id,
            Expression::EStruct(r) => r.id,
            Expression::Try(t) => t.id,
            Expression::TryBlock(t) => t.id,
            Expression::Tuple(t) => t.id,
            Expression::Unsafe(u) => u.id,
            Expression::While(w) => w.id,
            Expression::Yield(y) => y.id,
        }
    }

    #[must_use = "This method returns the location of the expression."]
    pub fn location(&self) -> Location {
        match self {
            Expression::Addr(a) => a.location.clone(),
            Expression::Array(a) => a.location.clone(),
            Expression::Assign(a) => a.location.clone(),
            Expression::Binary(b) => b.location(),
            Expression::Unary(u) => u.location(),
            Expression::Break(b) => b.location.clone(),
            Expression::EBlock(e) => e.location.clone(),
            Expression::Cast(c) => c.location.clone(),
            Expression::Closure(c) => c.location.clone(),
            Expression::Const(c) => c.location.clone(),
            Expression::Continue(c) => c.location.clone(),
            Expression::ForLoop(f) => f.location.clone(),
            Expression::FunctionCall(f) => f.location.clone(),
            Expression::If(i) => i.location.clone(),
            Expression::IndexAccess(i) => i.location.clone(),
            Expression::LetGuard(l) => l.location.clone(),
            Expression::MethodCall(m) => m.location.clone(),
            Expression::MemberAccess(m) => m.location.clone(),
            Expression::Reference(r) => r.location.clone(),
            Expression::Identifier(i) => i.location.clone(),
            Expression::Lit(l) => l.location.clone(),
            Expression::Loop(l) => l.location.clone(),
            Expression::Macro(m) => m.location.clone(),
            Expression::Match(m) => m.location.clone(),
            Expression::Parenthesized(p) => p.location.clone(),
            Expression::Range(r) => r.location.clone(),
            Expression::Repeat(r) => r.location.clone(),
            Expression::Return(r) => r.location.clone(),
            Expression::EStruct(r) => r.location.clone(),
            Expression::Try(t) => t.location.clone(),
            Expression::TryBlock(t) => t.location.clone(),
            Expression::Tuple(t) => t.location.clone(),
            Expression::Unsafe(u) => u.location.clone(),
            Expression::While(w) => w.location.clone(),
            Expression::Yield(y) => y.location.clone(),
        }
    }
}

#[allow(clippy::module_name_repetitions)]
pub enum ExpressionParentType {
    Function(Rc<Function>),
    Expression(Rc<Expression>),
}

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionCall {
    pub id: u128,
    pub location: Location,
    pub function_name: String,
    pub parameters: Vec<Expression>,
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

#[node_location]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct MethodCall {
    pub id: u128,
    pub location: Location,
    pub method_name: String,
    pub base: Expression,
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

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct MemberAccess {
    pub id: u128,
    pub location: Location,
    pub base: Expression,
    pub member_name: String,
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

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Reference {
    pub id: u128,
    pub location: Location,
    pub inner: Expression,
    pub is_mutable: bool,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Identifier {
    pub id: u128,
    pub location: Location,
    pub name: String,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Array {
    pub id: u128,
    pub location: Location,
    pub elements: Vec<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Assign {
    pub id: u128,
    pub location: Location,
    pub left: Expression,
    pub right: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Try {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct TryBlock {
    pub id: u128,
    pub location: Location,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct BinEx {
    pub id: u128,
    pub location: Location,
    pub left: Expression,
    pub right: Expression,
}

type RcBinEx = Rc<BinEx>;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
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
    pub fn id(&self) -> u128 {
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

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct UnEx {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
}

type RcUnEx = Rc<UnEx>;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
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
    pub fn id(&self) -> u128 {
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

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Break {
    pub id: u128,
    pub location: Location,
    pub expression: Option<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct EBlock {
    pub id: u128,
    pub location: Location,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Cast {
    pub id: u128,
    pub location: Location,
    pub base: Expression,
    pub target_type: Type,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Closure {
    pub id: u128,
    pub location: Location,
    pub captures: Vec<Rc<Identifier>>,
    pub body: Expression,
    pub returns: Type,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct ConstBlock {
    pub id: u128,
    pub location: Location,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Continue {
    pub id: u128,
    pub location: Location,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct ForLoop {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct If {
    pub id: u128,
    pub location: Location,
    pub condition: Expression,
    pub then_branch: Rc<Block>,
    pub else_branch: Option<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct IndexAccess {
    pub id: u128,
    pub location: Location,
    pub base: Expression,
    pub index: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct LetGuard {
    pub id: u128,
    pub location: Location,
    pub guard: Pattern,
    pub value: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Lit {
    pub id: u128,
    pub location: Location,
    pub value: Literal,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Loop {
    pub id: u128,
    pub location: Location,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Macro {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub text: String,
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Match {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
    pub arms: Vec<MatchArm>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Parenthesized {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Range {
    pub id: u128,
    pub location: Location,
    pub is_closed: bool,
    pub start: Option<Expression>,
    pub end: Option<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Addr {
    pub id: u128,
    pub location: Location,
    pub mutability: Mutability,
    pub expression: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Repeat {
    pub id: u128,
    pub location: Location,
    pub expression: Expression,
    pub count: Expression,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Return {
    pub id: u128,
    pub location: Location,
    pub expression: Option<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct EStruct {
    pub id: u128,
    pub location: Location,
    pub name: String,
    pub fields: Vec<(String, Expression)>,
    pub rest_dots: Option<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Tuple {
    pub id: u128,
    pub location: Location,
    pub elements: Vec<Expression>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Unsafe {
    pub id: u128,
    pub location: Location,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct While {
    pub id: u128,
    pub location: Location,
    pub label: Option<String>,
    pub condition: Expression,
    pub block: Rc<Block>,
}

#[node_location]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Yeild {
    pub id: u128,
    pub location: Location,
    pub expression: Option<Expression>,
}

#[cfg(test)]
mod function_call_tests {
    use super::*;
    use crate::location;
    use syn::parse_quote;

    #[test]
    fn test_function_call_function_name() {
        let inner_struct: ExprCall = parse_quote! {
            execute("Hello, world!")
        };
        let function_call = FunctionCall {
            id: 0,
            location: location!(inner_struct),
            function_name: FunctionCall::function_name_from_syn_item(&inner_struct),
            parameters: vec![],
        };

        assert_eq!(function_call.function_name, "execute");
    }
}
