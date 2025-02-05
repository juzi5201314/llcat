use rust_decimal::Decimal;

use super::Atom;

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
    pub name: Atom,
    pub params: Vec<Atom>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(Atom),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
    BlockExpr(ExprBlock),
    IfExpr(IfExpr),
    LetExpr(LetExpr),
    Return(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpr {
    pub name: Atom,
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
pub enum Literal {
    Float(Decimal),
    Integer(i64),
    String(Atom),
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