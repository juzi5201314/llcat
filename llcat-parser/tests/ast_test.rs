#![feature(assert_matches)]

use llcat_parser::ast::{Expr, Literal};
use llcat_parser::parser;

macro_rules! assert_matches {
    ($src:expr; err) => {
        std::assert_matches::assert_matches!(parser::parse_src($src).into_result(), Err(_))
    };
    ($src:expr, $ast:pat $(if $($if:tt)*)?) => {
        std::assert_matches::assert_matches!(parser::parse_src($src).into_result(), Ok($ast) $(if $($if)*)?)
    };
}

#[test]
fn ident_test() {
    assert_matches!("_true", Expr::Ident(id) if id == "_true");
    assert_matches!("a0", Expr::Ident(id) if id == "a0");
    assert_matches!("中"; err);
}

#[test]
fn bool_test() {
    assert_matches!("true", Expr::Literal(Literal::Boolean(true)));
    assert_matches!("false", Expr::Literal(Literal::Boolean(false)));
}

#[test]
fn int_test() {
    assert_matches!("-1", Expr::Literal(Literal::Interger(-1)));
    assert_matches!("0b11", Expr::Literal(Literal::Interger(3)));
    assert_matches!("0o10", Expr::Literal(Literal::Interger(8)));
    assert_matches!("-0xff", Expr::Literal(Literal::Interger(-255)));
    assert_matches!("10_000", Expr::Literal(Literal::Interger(10_000)));
    assert_matches!("0xf_fff", Expr::Literal(Literal::Interger(0xf_fff)));
}

#[test]
fn float_test() {
    assert_matches!("1.0", Expr::Literal(Literal::Float(1.0)));
    assert_matches!("0.1", Expr::Literal(Literal::Float(0.1)));
    assert_matches!("1e1", Expr::Literal(Literal::Float(1e1)));
    assert_matches!("1e-1", Expr::Literal(Literal::Float(1e-1)));
    assert_matches!("1e+1", Expr::Literal(Literal::Float(1e+1)));
    assert_matches!("-1e-1", Expr::Literal(Literal::Float(-1e-1)));
}

#[test]
fn str_test() {
    assert_matches!(r#" "'" "#, Expr::Literal(Literal::String(s)) if s == "'");
    assert_matches!(r#" "\"" "#, Expr::Literal(Literal::String(s)) if s == "\"");
    assert_matches!(r#" "\n" "#, Expr::Literal(Literal::String(s)) if s == "\n");
    assert_matches!(r#" "\q" "#; err);
    assert_matches!(r#" "\u{10FFFF}" "#, Expr::Literal(Literal::String(s)) if s == "\u{10FFFF}");
    assert_matches!("\"true\"", Expr::Literal(Literal::String(s)) if s == "true");
    assert_matches!("\"0\"", Expr::Literal(Literal::String(s)) if s == "0");
    assert_matches!("\"中\"", Expr::Literal(Literal::String(s)) if s == "中");
}