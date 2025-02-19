use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    error::Rich,
    input::{Input, MappedInput, Stream, ValueInput},
    pratt::{infix, left, postfix, prefix, right},
    prelude::{just, recursive},
    select,
    span::SimpleSpan,
    IterParser, Parser,
};

use crate::parser::{UnOp, UnaryExpr};

use super::{ast::*, Atom, Delimiter, Token};

pub type Span = Range<usize>;

pub trait TokenInput: ValueInput<'static, Token = Token, Span = SimpleSpan> {}

impl<T> TokenInput for T where T: ValueInput<'static, Token = Token, Span = SimpleSpan> {}

macro_rules! impl_parser {
    ($ret:ty) => {
        impl Parser<'static, I, $ret, chumsky::extra::Full<Rich<'static, Token>, (), ()>> + Clone + 'static
    };
}

pub enum ParseResult<O> {
    Ok(O),
    Err(Vec<String>),
}

impl<O> ParseResult<O> {
    pub fn unwrap(self) -> O {
        match self {
            ParseResult::Ok(o) => o,
            ParseResult::Err(err) => {
                for s in err {
                    eprintln!("{}\n", s);
                }
                panic!("parsing failed")
            }
        }
    }
}

pub trait ParserHelper<O> {
    fn f() -> fn((Token, SimpleSpan)) -> (Token, SimpleSpan);
    fn parse_from_str(&self, file: &str, src: &str) -> ParseResult<O>;
}

impl<'src, P, O> ParserHelper<O> for P
where
    P: Parser<
            'static,
            MappedInput<
                Token,
                SimpleSpan,
                Stream<std::vec::IntoIter<(Token, SimpleSpan)>>,
                fn((Token, SimpleSpan)) -> (Token, SimpleSpan),
            >,
            O,
            chumsky::extra::Full<Rich<'static, Token>, (), ()>,
        > + Clone,
{
    fn f() -> fn((Token, SimpleSpan)) -> (Token, SimpleSpan) {
        std::convert::identity
    }

    fn parse_from_str(&self, src_id: &str, src: &str) -> ParseResult<O> {
        use logos::Logos;

        let mut errors = Vec::new();
        let mut lexer_errors = Vec::new();

        let tokens = Token::lexer(src)
            .spanned()
            .map(|(result, span)| {
                (
                    match result {
                        Ok(t) => t,
                        Err(err) => {
                            lexer_errors.push(err.to_string());
                            Token::Error(lexer_errors.len() - 1)
                        }
                    },
                    SimpleSpan::from(span),
                )
            })
            .collect::<Vec<_>>();

        let stream = Stream::from_iter(tokens).map((0..src.len()).into(), Self::f());
        let res = self.parse(stream);

        match res.into_result() {
            Ok(o) => ParseResult::Ok(o),
            Err(errs) => {
                for err in errs {
                    let mut msg = Vec::new();
                    let tagged_span = (src_id, err.span().into_range());

                    let mut report = Report::build(ReportKind::Error, tagged_span.clone())
                        .with_message(err.to_string())
                        .with_label(
                            Label::new(tagged_span.clone())
                                .with_message(err.reason())
                                .with_color(Color::Red),
                        );

                    if let Some(Token::Error(idx)) = err.found() {
                        let lexer_err = &lexer_errors[*idx];
                        report = report.with_label(
                            Label::new(tagged_span)
                                .with_message(lexer_err.to_string())
                                .with_color(Color::Red)
                                .with_order(-5),
                        );
                    }

                    report
                        .finish()
                        .write((src_id, Source::from(src)), &mut msg)
                        .unwrap();
                    errors.push(String::from_utf8_lossy(&msg).to_string());
                }
                ParseResult::Err(errors)
            }
        }
    }
}

pub fn parser<I>() -> impl_parser!(Program)
where
    I: TokenInput,
{
    declaration_parser()
        .repeated()
        .collect()
        .map(|decls| Program { decls })
}

pub fn declaration_parser<I>() -> impl_parser!(Declaration)
where
    I: TokenInput,
{
    let func_decl = func_decl_parser().map(Declaration::Func);

    func_decl
}

pub fn func_decl_parser<I>() -> impl_parser!(FuncDecl)
where
    I: TokenInput,
{
    let name = ident_parser();
    let params = ident_parser()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<SpannedAtom>>()
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
        );

    let block = block_parser(expr_parser());
    let inline = expr_parser();
    let body = block
        .then_ignore(just(Token::Semi).or_not())
        .map_with(|block, e| ExprKind::BlockExpr(block).into_expr(e.span()))
        .or(inline.then_ignore(just(Token::Semi)));

    just(Token::Fn)
        .ignore_then(name)
        .then(params)
        .then_ignore(just(Token::Eq))
        .then(body)
        .map(|((name, params), body)| FuncDecl { name, params, body })
}

pub fn block_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(ExprBlock)
where
    I: TokenInput,
{
    expr_parser
        .clone()
        .then_ignore(just(Token::Semi))
        .repeated()
        .collect()
        .then(expr_parser.or_not())
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Brace)),
            just(Token::CloseDelimiter(Delimiter::Brace)),
        )
        .map(|(mut exprs, ret): (Vec<_>, Option<Expr>)| {
            if let Some(ret) = ret {
                exprs.push(ret);
            }
            ExprBlock { exprs }
        })
}

pub fn expr_parser<I>() -> impl_parser!(Expr)
where
    I: TokenInput,
{
    recursive(|expr| {
        //let inline_expr = recursive(|inline_expr| {
        // Token::Ident -> Expr::Ident
        let ident = ident_parser()
            .map(ExprKind::Ident)
            .map_with(|kind, e| kind.into_expr(e.span()))
            .labelled("ident");
        let literal = literal_parser()
            .map_with(|lit, e| ExprKind::Literal(lit).into_expr(e.span()))
            .labelled("literal");
        let bracket_expr = expr.clone().delimited_by(
            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
        );
        let if_expr = if_expr_parser(expr.clone())
            .map(ExprKind::IfExpr)
            .map_with(|kind, e| kind.into_expr(e.span()));
        let block_expr = block_parser(expr.clone())
            .map(ExprKind::BlockExpr)
            .map_with(|kind, e| kind.into_expr(e.span()));
        let let_expr = let_expr_parser(expr.clone())
            .map(ExprKind::LetExpr)
            .map_with(|kind, e| kind.into_expr(e.span()));
        let loop_expr = loop_expr_parser(expr.clone())
            .map(ExprKind::Loop)
            .map_with(|kind, e| kind.into_expr(e.span()));
        let array = array_parser(expr.clone())
            .map(ExprKind::Array)
            .map_with(|kind, e| kind.into_expr(e.span()));
        let array_index_expr = array_index_expr_parser(expr.clone())
            .map(ExprKind::ArrayIndexExpr)
            .map_with(|kind, e| kind.into_expr(e.span()));

        let atom = ident
            .or(literal)
            .or(bracket_expr)
            //.or(ret_expr)
            .or(let_expr)
            .or(if_expr)
            .or(block_expr)
            .or(loop_expr)
            .or(array);
        //.or(array_index_expr);

        let fn_call_expr = fn_call_expr_parser(expr.clone())
            .map(ExprKind::FnCall)
            .map_with(|kind, e| kind.into_expr(e.span()));

        let pratt = pratt_parser(fn_call_expr.or(atom), expr.clone());

        pratt
        //});
        //inline_expr
    })
}

pub fn let_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(LetExpr)
where
    I: TokenInput,
{
    just(Token::Let)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr_parser)
        .then_ignore(just(Token::Semi))
        .map(|(name, expr)| LetExpr {
            name,
            value: Box::new(expr),
        })
}

pub fn fn_call_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(FnCallExpr)
where
    I: TokenInput,
{
    ident_parser()
        .then(
            expr_parser
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(
                    just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                    just(Token::CloseDelimiter(Delimiter::Parenthesis)),
                ),
        )
        .map(|(name, args)| FnCallExpr { name, args })
}

pub fn loop_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(LoopExpr)
where
    I: TokenInput,
{
    just(Token::Loop)
        .ignore_then(block_parser(expr_parser))
        .map(|body| LoopExpr { body })
}

pub fn if_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(IfExpr)
where
    I: TokenInput,
{
    // if cond {...}
    // if cond {...} else {...}
    just(Token::If)
        .ignore_then(expr_parser.clone())
        .then(block_parser(expr_parser.clone()))
        .then(
            just(Token::Else)
                .ignore_then(block_parser(expr_parser.clone()))
                .or_not(),
        )
        .map_with(|((cond, then), el), e| IfExpr {
            cond: Box::new(cond),
            then_expr: Box::new(ExprKind::BlockExpr(then).into_expr(e.span())),
            else_expr: el.map(|b| Box::new(ExprKind::BlockExpr(b).into_expr(e.span()))),
        })
}

pub fn pratt_parser<I>(
    atom: impl_parser!(Expr),
    expr_parser: impl_parser!(Expr),
) -> impl_parser!(Expr)
where
    I: TokenInput,
{
    /* fn to_binary_expr<I>(l: Expr, op: BinOp, r: Expr, _: &mut MapExtra<'static, 'static, I, ParserExtra>) -> Expr where I: TokenInput {
        Expr::BinaryExpr(BinaryExpr {
            left: Box::new(l),
            op,
            right: Box::new(r),
        })
    }
    fn to_unary_expr<I>(op: UnOp, r: Expr, _: &mut MapExtra<'static, 'static, I, ParserExtra>) -> Expr where I: TokenInput {
        Expr::UnaryExpr(UnaryExpr {
            op,
            expr: Box::new(r),
        })
    } */

    macro_rules! to_binary_expr {
        () => {
            |l, op, r, e| {
                ExprKind::BinaryExpr(BinaryExpr {
                    left: Box::new(l),
                    op,
                    right: Box::new(r),
                })
                .into_expr(e.span())
            }
        };
    }

    macro_rules! to_unary_expr {
        () => {
            |op, r, e| {
                ExprKind::UnaryExpr(UnaryExpr {
                    op,
                    expr: Box::new(r),
                })
                .into_expr(e.span())
            }
        };
    }

    atom.clone().pratt((
        // @unary -, !
        prefix(
            13,
            select! {
                Token::Minus => UnOp::Neg,
                Token::Not => UnOp::Not,
            },
            to_unary_expr!(),
        ),
        // `*, /, %`
        infix(
            left(11),
            select! {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
            },
            to_binary_expr!(),
        ),
        // `+, -`
        infix(
            left(10),
            select! {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
            },
            to_binary_expr!(),
        ),
        // `== != < > <= >=`
        infix(
            left(5),
            select! {
                Token::EqEq => BinOp::Eq,
                Token::Ne => BinOp::Ne,
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::Le => BinOp::Le,
                Token::Ge => BinOp::Ge,
            },
            to_binary_expr!(),
        ),
        // <expr> && <expr>
        infix(
            left(4),
            select! { Token::AndAnd => BinOp::And },
            to_binary_expr!(),
        ),
        // <expr> || <expr>
        infix(
            left(3),
            select! { Token::OrOr => BinOp::Or },
            to_binary_expr!(),
        ),
        // ternary expr
        // cond ? then : else
        infix(
            right(2),
            just(Token::Question)
                .ignore_then(expr_parser.clone())
                .then_ignore(just(Token::Colon)),
            |cond, then, el, e| {
                ExprKind::IfExpr(IfExpr {
                    cond: Box::new(cond),
                    then_expr: Box::new(then),
                    else_expr: Some(Box::new(el)),
                })
                .into_expr(e.span())
            },
        ),
        // `=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=, **=`
        infix(
            right(1),
            select! {
                Token::Eq => BinOp::Assign,
                Token::PlusEq => BinOp::AddAssign,
                Token::MinusEq => BinOp::SubAssign,
                Token::StarEq => BinOp::MulAssign,
                Token::SlashEq => BinOp::DivAssign,
                Token::PercentEq => BinOp::ModAssign,
            },
            to_binary_expr!(),
        ),
        // return expr
        prefix(0, just(Token::Return).labelled("return"), |_, expr, e| {
            ExprKind::Return(Box::new(expr)).into_expr(e.span())
        }),
        /* postfix(
            0,
            expr_parser
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(
                    just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                    just(Token::CloseDelimiter(Delimiter::Parenthesis)),
                ),
            |name: Expr, args, e| {
                if let ExprKind::Ident(atom) = name.into_kind() {
                    ExprKind::FnCall(FnCallExpr { name: atom, args }).into_expr(e.span())
                } else {
                    todo!()
                }
            },
        ), */
        // array index expr
        postfix(
            0,
            expr_parser.delimited_by(
                just(Token::OpenDelimiter(Delimiter::Bracket)),
                just(Token::CloseDelimiter(Delimiter::Bracket)),
            ),
            |array, index, e| {
                ExprKind::ArrayIndexExpr(ArrayIndexExpr {
                    array: Box::new(array),
                    index: Box::new(index),
                })
                .into_expr(e.span())
            },
        ),
    ))
    //.pratt((a,))
}

pub fn array_index_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(ArrayIndexExpr)
where
    I: TokenInput,
{
    expr_parser
        .clone()
        .then(expr_parser.delimited_by(
            just(Token::OpenDelimiter(Delimiter::Bracket)),
            just(Token::CloseDelimiter(Delimiter::Bracket)),
        ))
        .map(|(expr, index)| ArrayIndexExpr {
            array: Box::new(expr),
            index: Box::new(index),
        })
}

pub fn array_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(Array)
where
    I: TokenInput,
{
    expr_parser
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Bracket)),
            just(Token::CloseDelimiter(Delimiter::Bracket)),
        )
        .map(|exprs| Array { elements: exprs })
}

pub fn literal_parser<I>() -> impl_parser!(Literal)
where
    I: TokenInput,
{
    select! {
        Token::Float(n) => Literal::Float(n),
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) = e => Literal::String(SpannedAtom::new(s, e.span())),
        Token::Boolean(b) => Literal::Boolean(b),
        Token::Nil => Literal::Nil,
    }
}

pub fn binop_parser<I>() -> impl_parser!(BinOp)
where
    I: TokenInput,
{
    select! {
        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Or => BinOp::Or,
    }
}

pub fn ident_parser<I>() -> impl_parser!(SpannedAtom)
where
    I: TokenInput,
{
    select! {
        Token::Ident(atom) = e => SpannedAtom::new(atom, e.span()),
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use crate::parser::ast::*;

    use super::*;

    macro_rules! expr {
        (@bin $l:pat, $op:pat, $r:pat) => {
            Expr {
                kind: ExprKind::BinaryExpr(BinaryExpr {
                    left: box $l,
                    op: $op,
                    right: box $r,
                }),
                span: _
            }
        };
        (@un $op:pat, $r:pat) => {
            Expr {
                kind: ExprKind::UnaryExpr(UnaryExpr {
                    op: $op,
                    expr: box $r,
                }),
                span: _
            }
        };
        (@float $n:pat) => {
            Expr { kind: ExprKind::Literal(Literal::Float($n)), span: _ }
        };
        (@int $i:pat) => {
            Expr { kind: ExprKind::Literal(Literal::Integer($i)), span: _ }
        };
        (@str $s:pat) => {
            Expr { kind: ExprKind::Literal(Literal::String(s) if &s == $s), span: _ }
        };
        (@bool $b:pat) => {
            Expr { kind: ExprKind::Literal(Literal::Boolean($b)), span: _ }
        };
        (@ret $e:pat) => {
            Expr { kind: ExprKind::Return(box $e), span: _ }
        };
        (@id $s:pat) => {
            Expr { kind: ExprKind::Ident($s), span: _ }
        };
        (@block $b:pat) => {
            Expr { kind: ExprKind::BlockExpr($b), span: _ }
        };
    }

    #[test]
    pub fn exprs() {
        let parser = expr_parser();

        let ident = parser.parse_from_str("[test]", "foo").unwrap().into_kind();
        assert!(matches!(ident, ExprKind::Ident(s) if &*s == "foo"));
    }

    #[test]
    pub fn pratt_exprs() {
        let parser = expr_parser();

        let literal = parser.parse_from_str("[test]", "-(1+2)*3.2").unwrap();
        assert!(matches!(
            literal,
            expr!(@bin
                expr!(@un UnOp::Neg, expr!(@bin expr!(@int n1), BinOp::Add, expr!(@int n2))),
                BinOp::Mul,
                expr!(@float n3)
            ) if n1 == 1 && n2 == 2 && n3 == dec!(3.2)
        ));
    }

    #[test]
    pub fn func_decl() {
        let parser = func_decl_parser();

        let func_decl = parser
            .parse_from_str("[test]", "fn add(a, b) = { a + b }")
            .unwrap();
        /* assert!(matches!(func_decl, FuncDecl {
            name,
            params,
            body: expr!(@block ExprBlock { exprs })
        } if &*name == "add" && params == vec!["a", "b"] && matches!(&*exprs, [
            expr!(@bin expr!(@id a), BinOp::Add, expr!(@id b))
        ] if &**a == "a" && &**b == "b")));

        let func_decl_inline = parser
            .parse_from_str("[test]", "fn add(a, b) = a + b;")
            .unwrap();
        assert!(matches!(func_decl_inline, FuncDecl {
            name,
            params,
            body: expr!(@bin Expr::Ident(a), BinOp::Add, Expr::Ident(b))
        } if &name == "add" && params == vec!["a", "b"] && a == "a" && b == "b")); */
    }

    #[test]
    pub fn if_expr() {
        let parser = expr_parser();

        let if_expr = parser
            .parse_from_str("[test]", "if a { b } else { c }")
            .unwrap();
        /* assert!(matches!(if_expr, Expr::IfExpr(IfExpr {
                   cond: box expr!(@id a),
                   then_expr: box Expr::BlockExpr(ExprBlock { exprs: then }),
                   else_expr: Some(box Expr::BlockExpr(ExprBlock { exprs: _else }))
               }) if a == "a" && matches!(&*then, &[expr!(@id b)] if b == "b") && matches!(&*_else, &[expr!(@id c)] if c == "c")));
        */
        let short_if_expr = parser.parse_from_str("[test]", "a + 1 ? b : c").unwrap();
        /*  assert!(matches!(short_if_expr, Expr::IfExpr(IfExpr {
            cond: box expr!(@bin expr!(@id a), BinOp::Add, expr!(@int 1)),
            then_expr: box expr!(@id b),
            else_expr: Some(box expr!(@id c))
        }) if a == "a" && b == "b" && c == "c")); */

        let short_if_expr = parser
            .parse_from_str("[test]", "{ a } ? { b } : c")
            .unwrap();
        /* assert!(matches!(short_if_expr, Expr::IfExpr(IfExpr {
                      cond: box Expr::BlockExpr(ExprBlock { exprs: cond }),
                      then_expr: box Expr::BlockExpr(ExprBlock { exprs: then }),
                      else_expr: Some(box expr!(@id c))
                  }) if c == "c" && matches!(&*then, &[expr!(@id b)] if b == "b") && matches!(&*cond, &[expr!(@id a)] if a == "a")));
        */
    }

    #[test]
    pub fn return_expr() {
        let parser = expr_parser();
        let ret = parser.parse_from_str("[test]", "ret 1 + 2").unwrap();
        assert!(matches!(
            ret,
            expr!(@ret expr!(@bin expr!(@int 1), BinOp::Add, expr!(@int 2)))
        ));
    }

    #[test]
    pub fn priority() {
        let parser = expr_parser();

        let if_expr = parser
            .parse_from_str("[test]", "if a { b } else { c } + 1")
            .unwrap();
        let let_expr = parser.parse_from_str("[test]", "let x = 1 + 1;").unwrap();
        println!("{:?}", let_expr);
    }
}
