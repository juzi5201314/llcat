use super::{BinOp, Block, Literal, UnOp};
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(SmolStr),

    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),

    Block(Block),

    /// if cond { block } else { else_block }
    If(Box<Expr>, Block, Option<Block>),
    Loop(Block),
    Return(Box<Expr>),
    Break,

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        params: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },
}
