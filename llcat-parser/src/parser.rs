use ariadne::Color;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;
use chumsky::error::Rich;

use chumsky::input::ValueInput;
use chumsky::primitive::just;
use chumsky::recursive::recursive;
use chumsky::select;
use chumsky::span::SimpleSpan;
use chumsky::util::MaybeSync;
use chumsky::IterParser;
use chumsky::ParseResult;
use chumsky::Parser as _;

use crate::ast::BinOp;
use crate::ast::Block;
use crate::ast::Stmt;
use crate::ast::{Expr, Literal};
use crate::small_vec::SmallVec1;
use crate::token::lexer;
use crate::token::token_stream;
use crate::token::Delimiter;
use crate::token::Token;
use crate::token::TokenIter;

macro_rules! P {
    ($l:lifetime, $i:ty, $o:ty) => {
        impl chumsky::Parser<$l, $i, $o, chumsky::extra::Full<Rich<$l, Token>, (), ParserContext>> + Clone + MaybeSync
    };
}

pub trait I<'a>: ValueInput<'a, Token = Token, Span = SimpleSpan> {}
impl<'a, T> I<'a> for T where T: ValueInput<'a, Token = Token, Span = SimpleSpan> {}

pub struct ParserContext {}

impl Default for ParserContext {
    fn default() -> Self {
        Self {}
    }
}

pub fn parse_src(src: &str, print_err: bool) -> Result<Stmt, Vec<Rich<Token>>> {
    let tokens = lexer(src);
    let result = parse_token(tokens, src.len()).into_result();
    if print_err {
        print_error(src, result).map_err(|_| Vec::new())
    } else {
        result
    }
}

pub fn parse_token<'s: 'a, 'a>(
    tokens: TokenIter<'s>,
    eoi: usize,
) -> ParseResult<Stmt, Rich<'a, Token>> {
    parser().parse(token_stream(tokens, eoi))
}

pub fn parse_expr(src: &str, print_err: bool) -> Result<Expr, Vec<Rich<Token>>> {
    let tokens = lexer(src);
    let result = expr_parser()
        .parse(token_stream(tokens, src.len()))
        .into_result();
    if print_err {
        print_error(src, result).map_err(|_| Vec::new())
    } else {
        result
    }
}

pub fn print_error<T>(src: &str, result: Result<T, Vec<Rich<Token>>>) -> Result<T, ()> {
    match result {
        Ok(t) => Ok(t),
        Err(errs) => {
            for err in errs {
                let err = err.map_token(|c| c.to_string());
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(src))
                    .unwrap();
            }
            Err(())
        }
    }
}

fn parser<'a, Input>() -> P!('a, Input, Stmt)
where
    Input: I<'a>,
{
    /* recursive(|_| {
        let stmt = stmt_parser(expr_parser()).boxed();

        stmt
    }) */
    let stmt = stmt_parser(expr_parser()).boxed();

    stmt
}

fn stmt_parser<'a, Input>(e: P!('a, Input, Expr)) -> P!('a, Input, Stmt)
where
    Input: I<'a>,
{
    let expr = e;
    let _let = just(Token::KeywordLet)
        .ignore_then(select! {
            Token::Ident(id) => id
        })
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .then_ignore(just(Token::Semi))
        .map(|(id, expr)| Stmt::Let(id, Box::new(expr)));

    expr.clone()
        .then_ignore(just(Token::Semi))
        .map(|expr| Stmt::SemiExpr(Box::new(expr)))
        .or(expr.map(|expr| Stmt::Expr(Box::new(expr))))
    //let expr = expr_parser();
}

fn expr_parser<'a, Input>() -> P!('a, Input, Expr)
where
    Input: I<'a>,
{
    recursive(|_expr| {
        let expr = recursive(|expr| {
            let nested_expr = expr.clone().delimited_by(
                just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                just(Token::CloseDelimiter(Delimiter::Parenthesis)),
            );

            let stmt = stmt_parser::<Input>(expr.clone());
            let block = stmt
                .clone()
                .filter(|stmt| !matches!(stmt, Stmt::Expr(_)))
                .repeated()
                .collect::<Vec<_>>()
                .then(stmt.filter(|stmt| matches!(stmt, Stmt::Expr(_))).or_not())
                .delimited_by(
                    just(Token::OpenDelimiter(Delimiter::Brace)),
                    just(Token::CloseDelimiter(Delimiter::Brace)),
                )
                .map(|(mut stmts, ret)| {
                    if let Some(ret) = ret {
                        stmts.push(ret);
                    }
                    Expr::Block(Block {
                        stmts: stmts.into(),
                    })
                });

            let atom = atom_parser();

            let ops = op_parser();
            let mut last_parser = atom.or(block).or(nested_expr).boxed();
            for op in ops {
                last_parser = last_parser
                    .clone()
                    .foldl(op.then(last_parser.clone()).repeated(), |l, (op, r)| {
                        Expr::Binary(op, Box::new(l), Box::new(r))
                    })
                    .boxed();
            }

            last_parser
        });

        expr
    })
}

fn op_parser<'a, Input>() -> [P!('a, Input, BinOp); 10]
where
    Input: I<'a>,
{
    [
        // as
        select! {
            Token::Star => BinOp::Mul,
            Token::Slash => BinOp::Div,
            Token::Percent => BinOp::Mod,
        }
        .boxed(),
        select! {
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub,
        }
        .boxed(),
        select! {
            Token::Shl => BinOp::Shl,
            Token::Shr => BinOp::Shr,
        }
        .boxed(),
        select! {
            Token::And => BinOp::BitAnd,
        }
        .boxed(),
        select! {
            Token::Caret => BinOp::BitXor,
        }
        .boxed(),
        select! {
            Token::Or => BinOp::BitOr,
        }
        .boxed(),
        select! {
            Token::Lt => BinOp::Lt,
            Token::Gt => BinOp::Gt,
            Token::Le => BinOp::Le,
            Token::Lt => BinOp::Lt,
            Token::Eq => BinOp::Eq,
            Token::Ne => BinOp::Ne,
        }
        .boxed(),
        select! {
            Token::AndAnd => BinOp::And,
        }
        .boxed(),
        select! {
            Token::OrOr => BinOp::Or,
        }
        .boxed(),
        // .. | ..=
        select! {
            Token::EqEq => BinOp::Eq,
            Token::PlusEq => BinOp::AddEq,
            Token::MinusEq => BinOp::SubEq,
            Token::StarEq => BinOp::MulEq,
            Token::SlashEq => BinOp::DivEq,
            Token::PercentEq => BinOp::ModEq,
            Token::AndEq => BinOp::BitAndEq,
            Token::CaretEq => BinOp::BitXorEq,
            Token::OrEq => BinOp::BitOrEq,
            Token::ShlEq => BinOp::ShlEq,
            Token::ShrEq => BinOp::ShrEq,
        }
        .boxed(),
    ]
}

fn atom_parser<'a, Input>() -> P!('a, Input, Expr)
where
    Input: I<'a>,
{
    select! {
        Token::Interger(i) => Expr::Literal(Literal::Interger(i)),
        Token::Float(f) => Expr::Literal(Literal::Float(f)),
        Token::Boolean(b) => Expr::Literal(Literal::Boolean(b)),
        Token::String(s) => Expr::Literal(Literal::String(s)),

        Token::Ident(id) => Expr::Ident(id),
    }
}
