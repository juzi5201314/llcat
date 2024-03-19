use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};
use smol_str::SmolStr;

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    Error,

    // literal
    #[regex(r"-?[0-9]+", to_i64, priority = 10)]
    #[regex(r"-?0b[0-1]+", to_i64)]
    #[regex(r"-?0o[0-7]+", to_i64)]
    #[regex(r"-?0x[0-9a-f]+", to_i64)]
    Interger(i64),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| SmolStr::from(lex.slice().trim_matches('"')))]
    String(SmolStr),
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Boolean(bool),

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

fn to_i64(lex: &mut Lexer<Token>) -> Option<i64> {
    let s = lex.slice();
    if s.len() < 3 {
        return s.parse::<i64>().ok()
    }
    match &s[..2] {
        "0b" => i64::from_str_radix(&s[2..], 2),
        "0o" => i64::from_str_radix(&s[2..], 8),
        "0x" => i64::from_str_radix(&s[2..], 16),
        s => s.parse::<i64>()
    }.ok()
}

pub type TokenIter<'s> = impl Iterator<Item = (Token, SimpleSpan)> + Clone;
//pub type TokenInput = SpannedInput<Token, SimpleSpan, Stream<impl Iterator<Item = (Token, SimpleSpan)> + Clone>>;

pub fn lexer<'s>(source: &'s str) -> TokenIter<'s> {
    Token::lexer(source).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    })
}

pub fn token_stream(
    tokens: impl Iterator<Item = (Token, SimpleSpan)> + Clone,
    eoi: usize,
) -> SpannedInput<Token, SimpleSpan, Stream<impl Iterator<Item = (Token, SimpleSpan)> + Clone>> {
    let stream = Stream::from_iter(tokens.clone()).spanned(SimpleSpan::from(eoi..eoi));

    stream
}
