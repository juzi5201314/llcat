/// todo...

use std::ops::Range;

use logos::Logos;
use peg::RuleResult;

use crate::parser::{ast::*, Delimiter};

use super::Token;

peg::parser! {
  grammar program_parser() for TokenStream {
/*
    pub rule integer() -> i64
        = hex_int() / bin_int() / dec_int()

    rule dec_int() -> i64 = n:$(['0'..='9']?['0'..='9' | '_']*) {? n.replace("_", "").parse().or(Err("int")) }
    rule hex_int() -> i64
        = "0x" n:$(['0'..='9' | 'a'..='f' | 'A'..='F']? ['0'..='9' | 'a'..='f' | 'A'..='F' | '_']*) {? i64::from_str_radix(&n.replace("_", ""), 16).or(Err("hex int")) }
    rule bin_int() -> i64
        = "0b" n:$(['0' | '1']? ['0' | '1' | '_']*) {? i64::from_str_radix(&n.replace("_", ""), 2).or(Err("bin int")) }
 */
    pub rule integer() -> i64 = [Token::Integer(n)] { n }
    pub rule float() -> rust_decimal::Decimal = [Token::Float(f)] { f }
    pub rule boolean() -> bool
        = [Token::Boolean(b)] { b }

    pub rule literal() -> Literal
        = i:integer() { Literal::Integer(i) }
        / f:float() { Literal::Float(f) }
        / b:boolean() { Literal::Boolean(b) }

    rule atom_expr() -> Expr
        = l:literal() { Expr::Literal(l) }

    pub rule arithmetic_expr() -> Expr = precedence!{
        l:(@) [Token::Plus] r:@ { Expr::BinaryExpr(BinaryExpr { left: Box::new(l), op: BinOp::Add, right: Box::new(r) }) }
        l:(@) [Token::Minus] r:@ { Expr::BinaryExpr(BinaryExpr { left: Box::new(l), op: BinOp::Sub, right: Box::new(r) }) }
        --
        l:(@) [Token::Star] r:@ { Expr::BinaryExpr(BinaryExpr { left: Box::new(l), op: BinOp::Mul, right: Box::new(r)}) }
        l:(@) [Token::Slash] r:@ { Expr::BinaryExpr(BinaryExpr { left: Box::new(l), op: BinOp::Div, right: Box::new(r)}) }
        --
        [Token::Not] e:(@) { Expr::UnaryExpr(UnaryExpr { op: UnOp::Not, expr: Box::new(e)}) }
        [Token::Minus] e:(@) { Expr::UnaryExpr(UnaryExpr { op: UnOp::Neg, expr: Box::new(e)}) }
        --
        e:atom_expr() { e }
        [Token::OpenDelimiter(Delimiter::Parenthesis)] e:arithmetic_expr() [Token::CloseDelimiter(Delimiter::Parenthesis)] { e }
    }

  }
}

pub fn lexer(src: &str) -> anyhow::Result<TokenStream> {
    let mut v = Vec::new();
    for (tok, span) in Token::lexer(src).spanned() {
        let token = tok?;
        v.push((token, span));
    }

    Ok(TokenStream(v))
}

#[cfg(test)]
macro_rules! expr {
    (@bin $l:pat, $op:pat, $r:pat) => {
        Expr::BinaryExpr(BinaryExpr {
            left: box $l,
            op: $op,
            right: box $r,
        })
    };
    (@un $op:pat, $r:pat) => {
        Expr::UnaryExpr(UnaryExpr {
            op: $op,
            expr: box $r,
        })
    };
    (@int $i:pat) => {
        Expr::Literal(Literal::Integer($i))
    };
    (@str $s:pat) => {
        Expr::Literal(Literal::String(s) if &s == $s)
    };
    (@bool $b:pat) => {
        Expr::Literal(Literal::Boolean($b))
    };
    (@ret $e:pat) => {
        Expr::Return(box $e)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::*;
    use crate::parser::peg::program_parser;

    use super::lexer;

    #[test]
    fn integer() {
        assert_eq!(program_parser::integer(&lexer("01").unwrap()), Ok(01));
        assert_eq!(
            program_parser::integer(&lexer("10_000").unwrap()),
            Ok(10_000)
        );
        assert_eq!(program_parser::integer(&lexer("0xff").unwrap()), Ok(255));
        assert_eq!(
            program_parser::integer(&lexer("0b110_011").unwrap()),
            Ok(0b110_011)
        );
    }

    #[test]
    pub fn arithmetic_expr() {
        let literal = program_parser::arithmetic_expr(&lexer("-(1+2)*3").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@bin
                expr!(@un UnOp::Neg, expr!(@bin expr!(@int n1), BinOp::Add, expr!(@int n2))),
                BinOp::Mul,
                expr!(@int n3)
            ) if n1 == 1 && n2 == 2 && n3 == 3
        ));
        let literal = program_parser::arithmetic_expr(&lexer("1 + 2 * 3").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@bin
                expr!(@int n1),
                BinOp::Add,
                expr!(@bin expr!(@int n2), BinOp::Mul, expr!(@int n3))
            ) if n1 == 1 && n2 == 2 && n3 == 3
        ));

        let literal = program_parser::arithmetic_expr(&lexer("1 * (2 + 3)").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@bin
                expr!(@int n1),
                BinOp::Mul,
                expr!(@bin expr!(@int n2), BinOp::Add, expr!(@int n3))
            ) if n1 == 1 && n2 == 2 && n3 == 3
        ));

        let literal = program_parser::arithmetic_expr(&lexer("1 - 2 / 3").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@bin
                expr!(@int n1),
                BinOp::Sub,
                expr!(@bin expr!(@int n2), BinOp::Div, expr!(@int n3))
            ) if n1 == 1 && n2 == 2 && n3 == 3
        ));

        let literal = program_parser::arithmetic_expr(&lexer("!1").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@un UnOp::Not, expr!(@int n1)) if n1 == 1
        ));

        let literal = program_parser::arithmetic_expr(&lexer("-(1)").unwrap()).unwrap();
        assert!(matches!(
            literal,
            expr!(@un UnOp::Neg, expr!(@int n1)) if n1 == 1
        ));
    }
}

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Sp(pub Span, pub usize);

impl ::std::fmt::Display for Sp {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        write!(fmt, "{:?} ({})", self.0, self.1)
    }
}

pub struct TokenStream(Vec<(Token, Span)>);

impl peg::Parse for TokenStream {
    type PositionRepr = Sp;

    fn start<'input>(&'input self) -> usize {
        0
    }

    fn is_eof<'input>(&'input self, p: usize) -> bool {
        p >= self.0.len()
    }

    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr {
        Sp(self.0.get(p).expect("overflow position_repr").1.clone(), p)
    }
}

impl<'i> peg::ParseElem<'i> for TokenStream {
    type Element = Token;

    fn parse_elem(&'i self, pos: usize) -> peg::RuleResult<Self::Element> {
        self.0
            .get(pos)
            .map(|(t, _)| RuleResult::Matched(pos + 1, *t))
            .unwrap_or(peg::RuleResult::Failed)
    }
}
