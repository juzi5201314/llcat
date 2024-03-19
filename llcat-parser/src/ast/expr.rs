use smol_str::SmolStr;

use super::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(SmolStr),

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