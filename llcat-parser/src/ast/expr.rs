use crate::small_vec::SmallVec3;

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

    /// ident(arg0, arg1, ..);
    Call(SmolStr, Box<SmallVec3<Expr>>),
}
