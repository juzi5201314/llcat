#![feature(assert_matches)]
#![feature(box_patterns)]

use llcat_parser::ast::BinOp;
use llcat_parser::ast::Expr::*;
use llcat_parser::ast::Literal::*;
use llcat_parser::parser;

macro_rules! assert_matches {
    ($src:expr; err) => {
        std::assert_matches::assert_matches!(parser::parse_src($src), Err(_))
    };
    ($src:expr, $ast:pat $(if $($if:tt)*)?) => {
        std::assert_matches::assert_matches!(parser::parse_src_and_print_error($src), Ok($ast) $(if $($if)*)?)
    };
}

#[test]
fn binary_expr_test() {
    assert_matches!(
        "1 * (2 + 3)",
        Binary(
            BinOp::Mul,
            box Literal(Interger(1)),
            box Binary(BinOp::Add, box Literal(Interger(2)), box Literal(Interger(3))),
        )
    );
    assert_matches!(
        "1 * 2 + 3",
        Binary(
            BinOp::Add,
            box Binary(BinOp::Mul, box Literal(Interger(1)), box Literal(Interger(2))),
            box Literal(Interger(3)),
        )
    );
    assert_matches!(
        "1.2 + arg0",
        Binary(
            BinOp::Add,
            box Literal(Float(1.2)),
            box Ident(s),
        ) if s == "arg0"
    );
    assert_matches!(
        "1 + 2 * 3 - 4 / 6",
        Binary(
            BinOp::Sub,
            box Binary(
                BinOp::Add,
                box Literal(Interger(1)),
                box Binary(BinOp::Mul, box Literal(Interger(2)), box Literal(Interger(3))),
            ),
            box Binary(BinOp::Div, box Literal(Interger(4)), box Literal(Interger(6))),
        )
    );
}

#[test]
fn ident_test() {
    assert_matches!("_true", Ident(id) if id == "_true");
    assert_matches!("_01", Ident(id) if id == "_01");
    assert_matches!("a0", Ident(id) if id == "a0");
    assert_matches!("中"; err);
}

#[test]
fn bool_test() {
    assert_matches!("true", Literal(Boolean(true)));
    assert_matches!("false", Literal(Boolean(false)));
}

#[test]
fn int_test() {
    assert_matches!("-1", Literal(Interger(-1)));
    assert_matches!("0b11", Literal(Interger(3)));
    assert_matches!("0o10", Literal(Interger(8)));
    assert_matches!("-0xff", Literal(Interger(-255)));
    assert_matches!("10_000", Literal(Interger(10_000)));
    assert_matches!("0xf_fff", Literal(Interger(0xf_fff)));
}

#[test]
fn float_test() {
    assert_matches!("1.0", Literal(Float(1.0)));
    assert_matches!("0.1", Literal(Float(0.1)));
    assert_matches!("1e1", Literal(Float(1e1)));
    assert_matches!("1e-1", Literal(Float(1e-1)));
    assert_matches!("1e+1", Literal(Float(1e+1)));
    assert_matches!("-1e-1", Literal(Float(-1e-1)));
}

#[test]
fn str_test() {
    assert_matches!(r#" "'" "#, Literal(String(s)) if s == "'");
    assert_matches!(r#" "\"" "#, Literal(String(s)) if s == "\"");
    assert_matches!(r#" "\n" "#, Literal(String(s)) if s == "\n");
    assert_matches!(r#" "\q" "#; err);
    assert_matches!(r#" "\u{10FFFF}" "#, Literal(String(s)) if s == "\u{10FFFF}");
    assert_matches!("\"true\"", Literal(String(s)) if s == "true");
    assert_matches!("\"0\"", Literal(String(s)) if s == "0");
    assert_matches!("\"中\"", Literal(String(s)) if s == "中");
}
