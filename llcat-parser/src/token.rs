use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};
use smallvec::SmallVec;
use smol_str::SmolStr;

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    Error,

    // literal
    #[regex(r"-?[_0-9]+", to_i64, priority = 10)]
    #[regex(r"-?0b[_0-1]+", to_i64)]
    #[regex(r"-?0o[_0-7]+", to_i64)]
    #[regex(r"-?0x[_0-9a-f]+", to_i64)]
    Interger(i64),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    #[regex(r#""([^"\\]|\\["\\0abnfrt]|\\u\{[a-fA-F0-9]{6}\})*""#, lex_string)]
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
    let s = lex.slice().replace("_", "");
    let s = &s;
    let neg = &s[..1] == "-";
    let no_dec_int_str = || if neg { &s[3..] } else { &s[2..] };
    if s.len() < 3 {
        return s.parse::<i64>().ok();
    }
    match if neg { &s[1..3] } else { &s[..2] } {
        "0b" => i64::from_str_radix(no_dec_int_str(), 2),
        "0o" => i64::from_str_radix(no_dec_int_str(), 8),
        "0x" => i64::from_str_radix(no_dec_int_str(), 16),
        _ => s.parse::<i64>(),
    }
    .ok()
    .map(|i| if neg { -i } else { i })
}

fn lex_string(lex: &mut Lexer<Token>) -> SmolStr {
    let chars = lex.slice()[1..lex.slice().len() - 1].chars();
    let mut s = Vec::with_capacity(lex.slice().len());
    let mut escape = false;
    let mut unicode_byte = None;

    for ch in chars {
        if escape {
            match ch {
                'n' => s.push('\n'),
                't' => s.push('\t'),
                'r' => s.push('\r'),
                '0' => s.push('\x00'),
                'a' => s.push('\x07'),
                'b' => s.push('\x08'),
                'f' => s.push('\x0c'),
                '\\' => s.push('\\'),
                '\"' => s.push('\"'),
                'u' => {
                    unicode_byte = Some(SmallVec::<[u8; 6]>::new());
                }
                ch => panic!("unknown escape char: {}", ch),
            }
            escape = false;
        } else if let Some(unicode) = &mut unicode_byte {
            if ch == '}' {
                let unicode = u32::from_str_radix(std::str::from_utf8(unicode).unwrap(), 16).unwrap();
                s.push(char::from_u32(unicode).unwrap());
                unicode_byte = None;
            } else if ch != '{' {
                let mut dst = [0; 1];
                ch.encode_utf8(&mut dst);
                unicode.extend_from_slice(&dst[..ch.len_utf8()]);
            }
        } else if ch == '\\' {
            escape = true;
        } else {
            s.push(ch);
        }
    }

    SmolStr::from_iter(s)
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
