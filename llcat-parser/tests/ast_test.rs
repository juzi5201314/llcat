#![feature(assert_matches)]
#![feature(box_patterns)]

use llcat_parser::ast;
use llcat_parser::ast::BinOp;
use llcat_parser::ast::Expr::*;
use llcat_parser::ast::Item;
use llcat_parser::ast::Literal::*;
use llcat_parser::ast::Stmt::*;
use llcat_parser::ast::UnOp;
use llcat_parser::parser::Parser;

macro_rules! assert_matches_expr {
    ($src:expr; err) => {
        {
            let mut parser = Parser::new($src).without_print_error().no_check();
            std::assert_matches::assert_matches!(parser.parse_once_expr($src), Err(_))
        }
    };
    ($src:expr, $ast:pat $(if $($if:tt)*)?) => {
        {
            let mut parser = Parser::new($src).no_check();
            std::assert_matches::assert_matches!(parser.parse_once_expr($src), Ok($ast) $(if $($if)*)?)
        }
    };
}

macro_rules! assert_matches_stmt {
    ($src:expr; err) => {
        {
            let mut parser = Parser::new($src).without_print_error().no_check();
            std::assert_matches::assert_matches!(parser.parse_once_stmt($src), Err(_));
        }
    };
    ($src:expr, $ast:pat $(if $($if:tt)*)?) => {
        {
            let mut parser = Parser::new($src).no_check();
            std::assert_matches::assert_matches!(parser.parse_once_stmt($src), Ok($ast) $(if $($if)*)?);
        }
    };
}

macro_rules! assert_matches {
    ($src:expr; err) => {
        {
            let mut parser = Parser::new($src).without_print_error();
            std::assert_matches::assert_matches!(parser.parse(), Err(_));
        }
    };
    ($src:expr, $ast:pat $(if $($if:tt)*)?) => {
        {
            let mut parser = Parser::new($src);
            std::assert_matches::assert_matches!(parser.parse(), Ok(v) if matches!(&*v.items, $ast $(if $($if)*)?));
        }
    };
}

#[test]
fn fn_decl_test() {
    assert_matches!(
        "fn sum(a, b) { ret a + b; }",
        &[
            Item::Decl(ast::Decl::Fn { ref name, ref params, body: ast::Block { ref stmts }, ref retrun_ty })
        ] if name == "sum"
            && params.as_slice() == &["a", "b"]
            && retrun_ty == &None
            && matches!(stmts.as_slice(), [SemiExpr(box Return(box Binary(
                BinOp::Add,
                box Ident(a),
                box Ident(b),
            )))] if a == "a" && b == "b")
    );
}

#[test]
fn let_stmt_test() {
    assert_matches_stmt!(
        "let x = 10;",
        Let(
            id,
            box Literal(Interger(10)),
        ) if id == "x"
    );
}

#[test]
fn stmt_block_test() {
    assert_matches_stmt!(
        "{6 * 6} - ({1 + 2; 3 + 4} + 5)",
        Expr(box Binary(
            BinOp::Sub,
            box Block(ast::Block {
                stmts: block1,
            }),
            box Binary(
                BinOp::Add,
                box Block(ast::Block {
                    stmts: block2,
                }),
                box Literal(Interger(5)),
            ),
        )) if matches!(&*block1, &[Expr(box Binary(
            BinOp::Mul,
            box Literal(Interger(6)),
            box Literal(Interger(6)),
        ))])
            && matches!(&*block2, &[SemiExpr(box Binary(
                BinOp::Add,
                box Literal(Interger(1)),
                box Literal(Interger(2)),
            )), Expr(box Binary(
                BinOp::Add,
                box Literal(Interger(3)),
                box Literal(Interger(4)),
            ))])
    );
}

#[test]
fn call_expr_test() {
    assert_matches_expr!(
        "1 + func1(arg0, arg1) - 1",
        Binary(
            BinOp::Sub,
            box Binary(
                BinOp::Add,
                box Literal(Interger(1)),
                box Call(id, args),
            ),
            box Literal(Interger(1)),
        ) if id == "func1" && matches!(&args.as_slice(), &[Ident(a0), Ident(a1)] if a0 == "arg0" && a1 == "arg1")
    );
}

#[test]
fn loop_expr_test() {
    assert_matches_expr!("loop {}", Loop(_));
    assert_matches_expr!(
        "1 + loop {}",
        Binary(BinOp::Add, box Literal(Interger(1)), box Loop(_))
    );
}

#[test]
fn return_expr_test() {
    assert_matches_expr!(
        "ret 1 + 2",
        Return(box Binary(BinOp::Add, box Literal(Interger(1)), box Literal(Interger(2))))
    );
    assert_matches_stmt!(
        "ret 1 + 2;",
        SemiExpr(box Return(box Binary(
            BinOp::Add,
            box Literal(Interger(1)),
            box Literal(Interger(2)),
        )))
    );
    assert_matches_expr!(
        "1 + (ret 1 + 2) + 3",
        Binary(
            BinOp::Add,
            box Binary(
                BinOp::Add,
                box Literal(Interger(1)),
                box Return(box Binary(
                    BinOp::Add,
                    box Literal(Interger(1)),
                    box Literal(Interger(2)),
                )),
            ),
            box Literal(Interger(3)),
        )
    );
}

#[test]
fn if_expr_test() {
    assert_matches_expr!(
        "if 1 < 2 { true }",
        If(
            box Binary(BinOp::Lt, box Literal(Interger(1)), box Literal(Interger(2))),
            ast::Block { stmts: block1 },
            None,
        ) if matches!(&*block1, &[
            Expr(box Literal(Boolean(true)))
        ])
    );

    assert_matches_expr!(
        "if value { true } else { false }",
        If(
            box Ident(id),
            ast::Block { stmts: block1},
            Some(ast::Block { stmts: block2 },),
        ) if id == "value"
        && matches!(&*block1, &[
            Expr(box Literal(Boolean(true)))
        ])
        && matches!(&*block2, &[
            Expr(box Literal(Boolean(false)))
        ])
    );
}

#[test]
fn unary_expr_test() {
    assert_matches_expr!(
        "!1 + 2",
        Binary(
            BinOp::Add,
            box Unary(UnOp::Not, box Literal(Interger(1))),
            box Literal(Interger(2)),
        )
    );
    assert_matches_expr!(
        "-(1 + 2)",
        Unary(
            UnOp::Neg,
            box Binary(BinOp::Add, box Literal(Interger(1)), box Literal(Interger(2))),
        )
    );
}

#[test]
fn binary_expr_test() {
    assert_matches_expr!(
        "1 * (2 + 3)",
        Binary(
            BinOp::Mul,
            box Literal(Interger(1)),
            box Binary(BinOp::Add, box Literal(Interger(2)), box Literal(Interger(3))),
        )
    );
    assert_matches_expr!(
        "1 * 2 + 3",
        Binary(
            BinOp::Add,
            box Binary(BinOp::Mul, box Literal(Interger(1)), box Literal(Interger(2))),
            box Literal(Interger(3)),
        )
    );
    assert_matches_expr!(
        "1.2 + arg0",
        Binary(
            BinOp::Add,
            box Literal(Float(1.2)),
            box Ident(s),
        ) if s == "arg0"
    );
    assert_matches_expr!(
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
    assert_matches_expr!("_true", Ident(id) if id == "_true");
    assert_matches_expr!("_01", Ident(id) if id == "_01");
    assert_matches_expr!("a0", Ident(id) if id == "a0");
    assert_matches_expr!("中"; err);
}

#[test]
fn bool_test() {
    assert_matches_expr!("true", Literal(Boolean(true)));
    assert_matches_expr!("false", Literal(Boolean(false)));
}

#[test]
fn int_test() {
    assert_matches_expr!("-1", Literal(Interger(-1)));
    assert_matches_expr!("0b11", Literal(Interger(3)));
    assert_matches_expr!("0o10", Literal(Interger(8)));
    assert_matches_expr!("-0xff", Literal(Interger(-255)));
    assert_matches_expr!("10_000", Literal(Interger(10_000)));
    assert_matches_expr!("0xf_fff", Literal(Interger(0xf_fff)));
}

#[test]
fn float_test() {
    assert_matches_expr!("1.0", Literal(Float(1.0)));
    assert_matches_expr!("0.1", Literal(Float(0.1)));
    assert_matches_expr!("1e1", Literal(Float(1e1)));
    assert_matches_expr!("1e-1", Literal(Float(1e-1)));
    assert_matches_expr!("1e+1", Literal(Float(1e+1)));
    assert_matches_expr!("-1e-1", Literal(Float(-1e-1)));
}

#[test]
fn str_test() {
    assert_matches_expr!(r#" "'" "#, Literal(String(s)) if s == "'");
    assert_matches_expr!(r#" "\"" "#, Literal(String(s)) if s == "\"");
    assert_matches_expr!(r#" "\n" "#, Literal(String(s)) if s == "\n");
    assert_matches_expr!(r#" "\q" "#; err);
    assert_matches_expr!(r#" "\u{10FFFF}" "#, Literal(String(s)) if s == "\u{10FFFF}");
    assert_matches_expr!("\"true\"", Literal(String(s)) if s == "true");
    assert_matches_expr!("\"0\"", Literal(String(s)) if s == "0");
    assert_matches_expr!("\"中\"", Literal(String(s)) if s == "中");
}
