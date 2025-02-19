use std::ops::Deref;

use chumsky::span::SimpleSpan;
use rust_decimal::Decimal;

use super::{Atom, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedAtom {
    pub atom: Atom,
    pub span: Span,
}

impl SpannedAtom {
    pub fn new(atom: Atom, span: SimpleSpan) -> Self {
        SpannedAtom {
            atom,
            span: span.into_range(),
        }
    }
}

impl Deref for SpannedAtom {
    type Target = Atom;

    fn deref(&self) -> &Self::Target {
        &self.atom
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: SpannedAtom,
    pub params: Vec<SpannedAtom>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn into_kind(self) -> ExprKind {
        self.kind
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Ident(SpannedAtom),
    Array(Array),
    ArrayIndexExpr(ArrayIndexExpr),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
    BlockExpr(ExprBlock),
    IfExpr(IfExpr),
    LetExpr(LetExpr),
    Return(Box<Expr>),
    Loop(LoopExpr),
    FnCall(FnCallExpr),
}

impl ExprKind {
    pub fn into_expr(self, span: SimpleSpan) -> Expr {
        Expr {
            span: span.into_range(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayIndexExpr {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopExpr {
    pub body: ExprBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallExpr {
    pub name: SpannedAtom,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpr {
    pub name: SpannedAtom,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Float(Decimal),
    Integer(i64),
    String(SpannedAtom),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `==`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    ModAssign,
    /// `&=`
    BitAndAssign,
    /// `|=`
    BitOrAssign,
    /// `^=`
    BitXorAssign,
    /// `<<=`
    ShlAssign,
    /// `>>=`
    ShrAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    /// `-`
    Neg,
    /// `!`
    Not,
}