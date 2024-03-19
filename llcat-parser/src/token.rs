use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};
use smol_str::SmolStr;

#[derive(Logos, Clone, PartialEq, Debug)]
pub enum Token {
    Error,

    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().unwrap(), priority = 10)]
    #[regex(r"-?0b[0-1]+", |lex| lex.slice().parse::<i64>().unwrap())]
    #[regex(r"-?0o[0-7]+", |lex| lex.slice().parse::<i64>().unwrap())]
    #[regex(r"-?0x[0-9a-f]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Interger(i64),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex|  SmolStr::from(lex.slice()))]
    String(SmolStr),
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| SmolStr::from(lex.slice()))]
    Ident(SmolStr),

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
}

fn _kilo(lex: &mut Lexer<Token>) -> Option<u64> {
    let slice = lex.slice();

    let n: u64 = slice[..slice.len() - 1].parse().ok()?; // skip 'k'
    Some(n * 1_000)
}

pub type TokenIter<'s> = impl Iterator<Item = (Token, SimpleSpan)> + Clone;
//pub type TokenInput = SpannedInput<Token, SimpleSpan, Stream<impl Iterator<Item = (Token, SimpleSpan)> + Clone>>;

pub fn lexer<'s>(source: &'s str) -> TokenIter<'s> {
    Token::lexer(source).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    })
}

pub fn token_stream(tokens: impl Iterator<Item = (Token, SimpleSpan)> + Clone, eoi: usize) -> SpannedInput<Token, SimpleSpan, Stream<impl Iterator<Item = (Token, SimpleSpan)> + Clone>> {
    let stream = Stream::from_iter(tokens.clone()).spanned(SimpleSpan::from(eoi..eoi));

    stream
}
