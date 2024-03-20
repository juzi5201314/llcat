use chumsky::error::Rich;

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

pub type P<'a, Input: I<'a>> =
    impl chumsky::Parser<'a, Input, Expr, chumsky::extra::Err<Rich<'a, Token>>>;

pub trait I<'a>: ValueInput<'a, Token = Token, Span = SimpleSpan> {}
impl<'a, T> I<'a> for T where T: ValueInput<'a, Token = Token, Span = SimpleSpan> {}

pub fn parse_src(src: &str) -> ParseResult<Expr, Rich<Token>> {
    let tokens = lexer(src);
    //dbg!(tokens.clone().collect::<Vec<_>>());
    parse_token(tokens, src.len())
}

pub fn parse_token<'s: 'a, 'a>(
    tokens: TokenIter<'s>,
    eoi: usize,
) -> ParseResult<Expr, Rich<'a, Token>> {
    parser().parse(token_stream(tokens, eoi))
}

fn parser<'a, Input>() -> P<'a, Input>
where
    Input: I<'a>,
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
