use std::vec::IntoIter;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    error::Rich,
    input::{Input, SpannedInput, Stream, ValueInput},
    pratt::{infix, left, prefix, right},
    prelude::{just, recursive},
    select,
    span::SimpleSpan,
    IterParser, Parser,
};

use crate::parser::{UnOp, UnaryExpr};

use super::{ast::*, Atom, Delimiter, Token};

pub trait TokenInput: ValueInput<'static, Token = Token, Span = SimpleSpan> {}

impl<T> TokenInput for T where T: ValueInput<'static, Token = Token, Span = SimpleSpan> {}

macro_rules! impl_parser {
    ($ret:ty) => {
        impl Parser<'static, I, $ret, chumsky::extra::Full<Rich<'static, Token>, (), ()>> + Clone
    };
}

pub enum ParseResult<O> {
    Ok(O),
    Err(Vec<String>),
}

pub trait ParseFromSource<O> {
    fn parse_src(&self, input: &str) -> ParseResult<O>;
}

impl<P, O> ParseFromSource<O> for P
where
    P: Parser<
            'static,
            SpannedInput<Token, SimpleSpan, Stream<IntoIter<(Token, SimpleSpan)>>>,
            O,
            chumsky::extra::Full<Rich<'static, Token>, (), ()>,
        > + Clone,
{
    fn parse_src(&self, input: &str) -> ParseResult<O> {
        use logos::Logos;

        let mut errors = Vec::new();

        let tokens = Token::lexer(input)
            .spanned()
            .map(|(result, span)| {
                (
                    match result {
                        Ok(t) => t,
                        Err(err) => {
                            let mut msg = Vec::new();
                            Report::build(ReportKind::Error, span.clone())
                                .with_message("Lexical Error")
                                .with_label(
                                    Label::new(span.clone())
                                        .with_message(err.to_string())
                                        .with_color(Color::Red),
                                )
                                .finish()
                                .write(Source::from(input), &mut msg)
                                .unwrap();
                            errors.push(String::from_utf8_lossy(&msg).to_string());
                            Token::Error
                        }
                    },
                    SimpleSpan::from(span),
                )
            })
            .collect::<Vec<_>>();

        let stream = Stream::from_iter(tokens)
            .spanned::<Token, SimpleSpan>(SimpleSpan::from(input.len()..input.len()));
        let res = self.parse(stream);

        if res.has_errors() {
            for err in res.errors() {
                let mut msg = Vec::new();
                Report::build(ReportKind::Error, err.span().into_range())
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(input), &mut msg)
                    .unwrap();
                errors.push(String::from_utf8_lossy(&msg).to_string());
            }
        }
        if let Some(output) = res.into_output() {
            ParseResult::Ok(output)
        } else {
            ParseResult::Err(errors)
        }
    }
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
        .collect::<Vec<Atom>>()
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
        );
    let body = block_parser(expr_parser());

    just(Token::Fn)
        .ignore_then(name)
        .then(params)
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
        let ident = ident_parser().map(Expr::Ident).labelled("ident");
        let literal = literal_parser()
            .map(|lit| Expr::Literal(lit))
            .labelled("literal");
        let bracket_expr = expr.clone().delimited_by(
            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
        );
        let ret_expr = just(Token::Return)
            .ignore_then(expr.clone())
            .map(Box::new)
            .map(Expr::Return)
            .labelled("return expr");
        let if_expr = if_expr_parser(expr.clone()).map(Expr::IfExpr);
        let block_expr = block_parser(expr.clone()).map(Expr::BlockExpr);

        let atom = ident
            .or(literal)
            .or(bracket_expr)
            .or(ret_expr)
            .or(if_expr)
            .or(block_expr);

        pratt_parser(atom, expr)
        //});
        //inline_expr
    })
}

pub fn ternary_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(IfExpr)
where
    I: TokenInput,
{
    let short_if = expr_parser
        .clone()
        .then_ignore(just(Token::Question))
        .then(expr_parser.clone())
        .then_ignore(just(Token::Colon))
        .then(expr_parser.clone())
        .map(|((cond, then), el)| IfExpr {
            cond: Box::new(cond),
            then_expr: Box::new(then),
            else_expr: Some(Box::new(el)),
        });
    short_if
}

pub fn if_expr_parser<I>(expr_parser: impl_parser!(Expr)) -> impl_parser!(IfExpr)
where
    I: TokenInput,
{
    // if cond {...}
    // if cond {...} else {...}
    let full = just(Token::If)
        .ignore_then(expr_parser.clone())
        .then(block_parser(expr_parser.clone()))
        .then(
            just(Token::Else)
                .ignore_then(block_parser(expr_parser.clone()))
                .or_not(),
        )
        .map(|((cond, then), el)| IfExpr {
            cond: Box::new(cond),
            then_expr: Box::new(Expr::BlockExpr(then)),
            else_expr: el.map(|e| Box::new(Expr::BlockExpr(e))),
        });

    let short_if = expr_parser
        .clone()
        .then_ignore(just(Token::Question))
        .then(expr_parser.clone())
        .then_ignore(just(Token::Colon))
        .then(expr_parser.clone())
        .map(|((cond, then), el)| IfExpr {
            cond: Box::new(cond),
            then_expr: Box::new(then),
            else_expr: Some(Box::new(el)),
        });

    full //.or(short_if)
}

pub fn pratt_parser<I>(
    atom: impl_parser!(Expr),
    expr_parser: impl_parser!(Expr),
) -> impl_parser!(Expr)
where
    I: TokenInput,
{
    fn to_binary_expr(l: Expr, op: BinOp, r: Expr) -> Expr {
        Expr::BinaryExpr(BinaryExpr {
            left: Box::new(l),
            op,
            right: Box::new(r),
        })
    }
    fn to_unary_expr(op: UnOp, r: Expr) -> Expr {
        Expr::UnaryExpr(UnaryExpr {
            op,
            expr: Box::new(r),
        })
    }

    macro_rules! just_map {
        ($just:expr => $map_to:expr) => {
            just($just).map(|_| $map_to).boxed()
        };
    }

    atom.pratt((
        // @unary -, !
        prefix(12, just_map!(Token::Minus => UnOp::Neg), to_unary_expr),
        prefix(12, just_map!(Token::Not => UnOp::Not), to_unary_expr),
        // *, /, %
        infix(
            left(11),
            just_map!(Token::Star => BinOp::Mul),
            to_binary_expr,
        ),
        infix(
            left(11),
            just_map!(Token::Slash => BinOp::Div),
            to_binary_expr,
        ),
        infix(
            left(11),
            just_map!(Token::Percent => BinOp::Mod),
            to_binary_expr,
        ),
        // +, -
        infix(
            left(10),
            just_map!(Token::Plus => BinOp::Add),
            to_binary_expr,
        ),
        infix(
            left(10),
            just_map!(Token::Minus => BinOp::Sub),
            to_binary_expr,
        ),
        infix(
            right(0),
            just(Token::Question)
                .ignore_then(expr_parser)
                .then_ignore(just(Token::Colon)),
            |(cond, then, el)| {
                Expr::IfExpr(IfExpr {
                    cond: Box::new(cond),
                    then_expr: Box::new(then),
                    else_expr: Some(Box::new(el)),
                })
            },
        ),
    ))
}

pub fn literal_parser<I>() -> impl_parser!(Literal)
where
    I: TokenInput,
{
    select! {
        Token::Float(n) => Literal::Float(n),
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(s),
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

pub fn ident_parser<I>() -> impl_parser!(Atom)
where
    I: TokenInput,
{
    select! {
        Token::Ident(atom) => atom,
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use crate::parser::ast::*;

    use super::*;

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
        (@float $n:pat) => {
            Expr::Literal(Literal::Float($n))
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
        };
        (@id $s:pat) => {
            Expr::Ident($s)
        };
    }

    #[test]
    pub fn exprs() {
        let parser = expr_parser();

        let ident = parser.parse_src("foo").unwrap();
        assert!(matches!(ident, Expr::Ident(s) if &s == "foo"));
    }

    #[test]
    pub fn pratt_exprs() {
        let parser = expr_parser();

        let literal = parser.parse_src("-(1+2)*3.2").unwrap();
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

        let func_decl = parser.parse_src("fn add(a, b) { a + b }").unwrap();
        assert!(matches!(func_decl, FuncDecl {
            name,
            params,
            body: ExprBlock { exprs }
        } if &name == "add" && params == vec!["a", "b"] && matches!(&*exprs, [
            expr!(@bin Expr::Ident(a), BinOp::Add, Expr::Ident(b))
        ] if a == "a" && b == "b")));
    }

    #[test]
    pub fn if_expr() {
        let parser = expr_parser();

        let if_expr = parser.parse_src("if a { b } else { c }").unwrap();
        assert!(matches!(if_expr, Expr::IfExpr(IfExpr {
            cond: box expr!(@id a),
            then_expr: box Expr::BlockExpr(ExprBlock { exprs: then }),
            else_expr: Some(box Expr::BlockExpr(ExprBlock { exprs: _else }))
        }) if a == "a" && matches!(&*then, &[expr!(@id b)] if b == "b") && matches!(&*_else, &[expr!(@id c)] if c == "c")));
        let short_if_expr = parser.parse_src("a ? b : c").unwrap();
        assert!(matches!(short_if_expr, Expr::IfExpr(IfExpr {
                   cond: box expr!(@id a),
                   then_expr: box expr!(@id b),
                   else_expr: Some(box expr!(@id c))
               }) if a == "a" && b == "b" && c == "c"));

        let short_if_expr = parser.parse_src("a ? { b } : c").unwrap();
        assert!(matches!(short_if_expr, Expr::IfExpr(IfExpr {
                   cond: box Expr::BlockExpr(ExprBlock { exprs: cond }),
                   then_expr: box Expr::BlockExpr(ExprBlock { exprs: then }),
                   else_expr: Some(box expr!(@id c))
               }) if c == "c" && matches!(&*then, &[expr!(@id b)] if b == "b") && matches!(&*cond, &[expr!(@id a)] if a == "a")));
    }
}
