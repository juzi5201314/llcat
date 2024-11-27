#[derive(Debug, Clone)]
pub struct Program {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
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
    /// `+=`
    AddEq,
    /// `-=`
    SubEq,
    /// `*=`
    MulEq,
    /// `/=`
    DivEq,
    /// `%=`
    ModEq,
    /// `&=`
    BitAndEq,
    /// `|=`
    BitOrEq,
    /// `^=`
    BitXorEq,
    /// `<<=`
    ShlEq,
    /// `>>=`
    ShrEq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    /// `-`
    Neg,
    /// `!`
    Not,
}