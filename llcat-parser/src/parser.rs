use chumsky::error::EmptyErr;
use chumsky::input::Input;
use chumsky::input::ValueInput;
use chumsky::recursive::recursive;
use chumsky::select;
use chumsky::span::SimpleSpan;
use chumsky::ParseResult;
use chumsky::Parser as _;

use crate::ast::{Expr, Literal};
use crate::token::lexer;
use crate::token::token_stream;
use crate::token::Token;
use crate::token::TokenIter;

pub type Parser<'s, I: Input<'s>> = impl chumsky::Parser<'s, I, Expr>;

pub fn parse_src(src: &str) -> ParseResult<Expr, EmptyErr> {
    let tokens = lexer(src);
    dbg!(tokens.clone().collect::<Vec<_>>());
    parse_token(tokens, src.len())
}

pub fn parse_token<'s>(tokens: TokenIter<'s>, eoi: usize) -> ParseResult<Expr, EmptyErr> {
    parser().parse(token_stream(tokens, eoi))
}

fn parser<'s, I>() -> Parser<'s, I>
where
    I: ValueInput<'s, Token = Token, Span = SimpleSpan>,
{
    recursive(|_expr| {
        let atom = select! {
            Token::Interger(i) => Expr::Literal(Literal::Interger(i)),
            Token::Float(f) => Expr::Literal(Literal::Float(f)),
            Token::Boolean(b) => Expr::Literal(Literal::Boolean(b)),
            Token::String(s) => Expr::Literal(Literal::String(s)),
        

            Token::Ident(id) => Expr::Ident(id),
        };

        atom
    })
}

/* fn interger_parser<'s>() -> Parser<'s> {
    let int = text::int(10).map(|s: &str| Expr::Literal(Literal::Interger(s.parse().unwrap())));
    int.or(int)
} */
