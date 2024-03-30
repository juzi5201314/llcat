use smol_str::SmolStr;

use crate::small_vec::SmallVec3;

use super::{Decl, Expr};


#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(SmolStr, Box<Expr>),
    Assign(SmolStr, Box<Expr>),
    Decl(Box<Decl>),
    /// `expr`
    Expr(Box<Expr>),
    /// `expr;`
    SemiExpr(Box<Expr>),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: SmallVec3<Stmt>,
}