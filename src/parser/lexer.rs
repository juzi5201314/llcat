use std::cell::RefCell;

use caches::Cache;
use logos::Logos;
use rust_decimal::Decimal;
use smallvec::SmallVec;

use super::Atom;

pub type Lexer<'a> = logos::Lexer<'a, Token>;

pub struct Caches<'s> {
    s: caches::AdaptiveCache<
        &'s str,
        Atom,
        hashbrown::DefaultHashBuilder,
        hashbrown::DefaultHashBuilder,
        hashbrown::DefaultHashBuilder,
        hashbrown::DefaultHashBuilder,
    >,
}

impl<'s> Default for Caches<'s> {
    fn default() -> Self {
        let hasher = hashbrown::DefaultHashBuilder::default();
        Caches {
            s: caches::AdaptiveCache::<&'s str, Atom>::builder(1024)
                .set_recent_hasher(hasher.clone())
                .set_frequent_hasher(hasher.clone())
                .set_frequent_evict_hasher(hasher.clone())
                .set_recent_evict_hasher(hasher.clone())
                .finalize()
                .unwrap(),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone, Copy, amplify_derive::Display)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = LexError)]
#[logos(extras = (Caches<'s>,))]
#[display(lowercase)]
pub enum Token {
    // literal
    #[regex(
        r"-?(?:0|[1-9]\d*)(?:\.\d+)(?:[eE][+-]?\d+)?",
        lex_float_num,
        priority = 80
    )]
    #[display("{0}")]
    Float(Decimal),
    #[regex(r"-?[0-9][_0-9]*", lex_num)]
    #[regex(r"-?0b[_0-1]+", lex_num)]
    #[regex(r"-?0o[_0-7]+", lex_num)]
    #[regex(r"-?0x[_0-9a-f]+", lex_num)]
    Integer(i64),
    //#[regex(r#""([^"\\]|\\["\\0abnfrt]|\\u\{[a-fA-F0-9]{6}\})*""#, lex_string)]
    #[regex(r#""([^"\\]|\\\S)*""#, lex_string)]
    #[display("{0}")]
    String(Atom),
    #[token("false", |_| false, priority = 100)]
    #[token("true", |_| true, priority = 100)]
    #[display("{0}")]
    Boolean(bool),

    #[token("nil")]
    Nil,

    // #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| Atom::from(lex.slice()), priority = 4)]
    #[regex(r"[^[:punct:]0-9\s]([^[:punct:]\s]|_)*", |lex| Atom::from(lex.slice()), priority = 3)]
    #[display("{0}")]
    Ident(Atom),

    // keyword
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("ret")]
    Return,
    #[token("super")]
    Super,
    #[token("this")]
    This,
    #[token("let")]
    Let,
    #[token("loop")]
    Loop,

    // punctuation symbol
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,
    #[token("!")]
    Not,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("^=")]
    CaretEq,
    #[token("&=")]
    AndEq,
    #[token("|=")]
    OrEq,
    #[token("<<=")]
    ShlEq,
    #[token(">>=")]
    ShrEq,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    Ne,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token(">=")]
    Ge,
    #[token("<=")]
    Le,
    #[token("@")]
    At,
    #[token("_")]
    Underscore,
    #[token(".")]
    Dot,
    #[display(";")]
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token("::")]
    PathSep,
    #[token("->")]
    RArrow,
    #[token("=>")]
    FatArrow,
    #[token("#")]
    Pound,
    #[token("$")]
    Dollar,
    #[display("?")]
    #[token("?")]
    Question,
    #[token("~")]
    Tilde,
    #[token("'")]
    SingleQuotationMark,
    #[token(",")]
    Comma,

    // delimiter
    #[regex(r"\(|\{|\[", lex_delimiter)]
    #[display("open_delimiter")]
    OpenDelimiter(Delimiter),
    #[regex(r"\)|\}|\]", lex_delimiter)]
    #[display("close_delimiter")]
    CloseDelimiter(Delimiter),

    #[display("[error token]")]
    Error(usize),
    //Error(LexError),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Delimiter {
    /// ()
    Parenthesis,
    /// {}
    Brace,
    /// []
    Bracket,
}

fn lex_delimiter(lex: &mut Lexer) -> Option<Delimiter> {
    Some(match lex.slice() {
        "(" | ")" => Delimiter::Parenthesis,
        "{" | "}" => Delimiter::Brace,
        "[" | "]" => Delimiter::Bracket,
        _ => return None,
    })
}

fn lex_string(lex: &mut Lexer) -> Result<Atom, LexError> {
    thread_local! {
        // cache buffer, it can avoid a lot of heap allocation performance consumption
        static STRING_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(24));
    }

    if let Some(atom) = lex.extras.0.s.get(&lex.slice()) {
        return Ok(*atom);
    };

    let chars = lex.slice()[1..lex.slice().len() - 1].chars();
    let mut escape = false;
    let mut unicode = false;
    let mut unicode_byte = SmallVec::<_, 6>::new();

    STRING_BUFFER.with_borrow_mut(|s| {
        s.clear();
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
                        unicode = true;
                    }
                    ch => return Err(LexError::InvalidEscapeChar(ch)),
                }
                escape = false;
            } else if unicode {
                if ch == '}' {
                    let code =
                        u32::from_str_radix(std::str::from_utf8(&unicode_byte).unwrap(), 16)?;
                    s.push(char::from_u32(code).unwrap());
                    unicode = false;
                } else if ch != '{' {
                    if !['a'..='f', 'A'..='F', '0'..='9']
                        .iter()
                        .any(|r| r.contains(&ch))
                    {
                        return Err(LexError::InvalidUnicodeChar(ch));
                    }

                    if unicode_byte.len() >= 6 {
                        return Err(LexError::OverlongUnicodeEscape);
                    }

                    let mut dst = [0; 1];
                    ch.encode_utf8(&mut dst);
                    unicode_byte.extend_from_slice(&dst[..ch.len_utf8()]);
                }
            } else if ch == '\\' {
                escape = true;
            } else {
                s.push(ch);
            }
        }

        let atom = Atom::new(s.as_str());

        lex.extras.0.s.put(lex.slice(), atom);
        Ok(atom)
    })
}

fn lex_float_num(lex: &mut Lexer) -> Result<Decimal, LexError> {
    (if lex.slice().contains('e') || lex.slice().contains('E') {
        Decimal::from_scientific
    } else {
        Decimal::from_str_exact
    })(lex.slice())
    .map_err(Into::<LexError>::into)
}

fn lex_num(lex: &mut Lexer) -> Result<i64, LexError> {
    thread_local! {
        static BUFFER: RefCell<SmallVec::<u8, 8>> = RefCell::new(SmallVec::<_, 8>::new());
    }

    let s = lex.slice();

    BUFFER.with_borrow_mut(|buffer| {
        buffer.clear();
        let bytes = {
            // copy from String::replace
            // delete `_`
            let mut last_end = 0;
            for (start, part) in s.match_indices('_') {
                buffer.extend_from_slice(unsafe { s.get_unchecked(last_end..start) }.as_bytes());
                last_end = start + part.len();
            }
            buffer.extend_from_slice(unsafe { s.get_unchecked(last_end..s.len()) }.as_bytes());
            buffer
        };
        let s = unsafe { std::str::from_utf8_unchecked(&bytes) };

        let (neg, s) = if &s[..1] == "-" {
            (true, &s[1..])
        } else {
            (false, s)
        };

        let (base, num) = if neg {
            (s.get(1..3), s.get(3..))
        } else {
            (s.get(..2), s.get(2..))
        };

        let radix = match base {
            Some("0b") => 2,
            Some("0o") => 8,
            Some("0x") => 16,
            _ => 10,
        };

        let src = if radix == 10 {
            s
        } else {
            num.expect("the lexer regex ensures that there must be something here")
        };

        i64::from_str_radix(src, radix)
            .map(|i| if neg { -i } else { i })
            .map_err(Into::into)
    })
}

#[derive(thiserror::Error, Default, Debug, Clone, PartialEq)]
pub enum LexError {
    #[default]
    #[error("Invalid token")]
    InvalidToken,

    #[error("Invalid escape char: {0}")]
    InvalidEscapeChar(char),

    #[error("invalid character in unicode escape: {0}")]
    InvalidUnicodeChar(char),
    #[error("overlong unicode escape")]
    OverlongUnicodeEscape,

    #[error("parse int error: {0}")]
    ParseIntegerError(#[from] std::num::ParseIntError),

    #[error("parse float error: {0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),

    #[error("parse decimal error: {0}")]
    ParseDecimalError(#[from] rust_decimal::Error),
}

#[allow(unused_variables)]
#[cfg(test)]
mod tests {
    use logos::Logos;
    use rust_decimal_macros::dec;

    use crate::parser::{Atom, LexError, Lexer, Token};

    macro_rules! assert_lex_eq {
        ($src:expr, |$id:ident| [$($tok:expr),+ $(,)?]) => {
            let mut lex = Token::lexer($src);
            $(
                assert_eq!(lex.next(), (|$id: &mut Lexer| Some($tok))(&mut lex));
            )+
            assert_eq!(lex.next(), None);
        };
        ($src:expr, [$($tok:expr),+ $(,)?]) => {
            assert_lex_eq!($src, |_lex| [$($tok),+]);
        };
        /* ($src:expr, [$($tok:expr),+ $(,)?]) => {
            let mut lex = Token::lexer($src);
            $(
                assert_eq!(lex.next(), Some($tok));
            )+
            assert_eq!(lex.next(), None);
        }; */

    }

    #[test]
    fn lex_ident() {
        assert_lex_eq!("a", [Ok(Token::Ident(Atom::from("a")))]);
        assert_lex_eq!("a_bc", [Ok(Token::Ident(Atom::from("a_bc")))]);
        assert_lex_eq!("abc123", [Ok(Token::Ident(Atom::from("abc123")))]);
        assert_lex_eq!(
            "123abc",
            [
                Ok(Token::Integer(123.into())),
                Ok(Token::Ident(Atom::from("abc")))
            ]
        );
        assert_lex_eq!("中文", [Ok(Token::Ident(Atom::from("中文")))]);
    }

    #[test]
    fn lex_float() {
        assert_lex_eq!("123.45", [Ok(Token::Float(dec!(123.45)))]);
        assert_lex_eq!("-67.89", [Ok(Token::Float(dec!(-67.89)))]);
        assert_lex_eq!("123.45e+6", [Ok(Token::Float(dec!(123.45e+6)))]);
        assert_lex_eq!("-67.89e-3", [Ok(Token::Float(dec!(-67.89e-3)))]);
        assert_lex_eq!("123.45e+6", [Ok(Token::Float(dec!(123.45e+6)))]);
    }

    #[test]
    fn lex_integer() {
        assert_lex_eq!("1_230", [Ok(Token::Integer(1230))]);
        assert_lex_eq!("-456", [Ok(Token::Integer(-456))]);
        assert_lex_eq!("0b1010", [Ok(Token::Integer(0b1010))]);
        assert_lex_eq!("0o777", [Ok(Token::Integer(0o777))]);
        assert_lex_eq!("0x12345678", [Ok(Token::Integer(0x12345678))]);
    }

    #[test]
    fn lex_escape_str() {
        assert_lex_eq!(r#" """ "#, |lex| [
            Ok(Token::String(Atom::new(""))),
            Err(LexError::InvalidToken)
        ]);
        assert_lex_eq!(r#" "\"" "\n" "\q" "#, |lex| [
            Ok(Token::String(Atom::new("\""))),
            Ok(Token::String(Atom::new("\n"))),
            Err(LexError::InvalidEscapeChar('q'))
        ]);
    }

    #[test]
    fn lex_unicode_str() {
        assert_lex_eq!(r#" "\u{10FFFF}" "\u{10FFFX}" "\u{10FFFFF}""#, |lex| [
            Ok(Token::String(Atom::new("\u{10FFFF}"))),
            Err(LexError::InvalidUnicodeChar('X')),
            Err(LexError::OverlongUnicodeEscape)
        ]);
    }

    #[test]
    fn lex_str() {
        assert_lex_eq!(r#" "foo" "0" "中文" "#, |lex| [
            Ok(Token::String(Atom::new("foo"))),
            Ok(Token::String(Atom::new("0"))),
            Ok(Token::String(Atom::new("中文")))
        ]);
    }
}
