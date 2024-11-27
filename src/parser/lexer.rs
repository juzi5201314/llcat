use std::cell::RefCell;

use logos::Logos;
use smallvec::SmallVec;
use smol_str::SmolStr;

use super::{Atom, Intern};

pub type Lexer<'a> = logos::Lexer<'a, Token>;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = LexError)]
#[logos(extras = Intern)]
pub enum Token {
    // literal
    #[regex(r"-?[_0-9]+", lex_i64, priority = 3)]
    #[regex(r"-?0b[_0-1]+", lex_i64)]
    #[regex(r"-?0o[_0-7]+", lex_i64)]
    #[regex(r"-?0x[_0-9a-f]+", lex_i64)]
    Integer(i64),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    //#[regex(r#""([^"\\]|\\["\\0abnfrt]|\\u\{[a-fA-F0-9]{6}\})*""#, lex_string)]
    #[regex(r#""([^"\\]|\\\S)*""#, lex_string)]
    String(Atom),
    #[token("false", |_| false, priority = 100)]
    #[token("true", |_| true, priority = 100)]
    Boolean(bool),

    #[token("nil")]
    Nil,

    // #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| SmolStr::from(lex.slice()), priority = 4)]
    #[regex(r"[^[:punct:]0-9\s]([^[:punct:]\s]|_)*", |lex| SmolStr::from(lex.slice()), priority = 3)]
    Ident(SmolStr),

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
    #[token("while")]
    While,

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
    OpenDelimiter(Delimiter),
    #[regex(r"\)|\}|\]", lex_delimiter)]
    CloseDelimiter(Delimiter),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Delimiter {
    // ()
    Parenthesis,
    // {}
    Brace,
    // []
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
        static STRING_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(24));
    }

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

        Ok(lex.extras.intern(&s))
    })
}

fn lex_i64(lex: &mut Lexer) -> Result<i64, LexError> {
    let s = lex.slice();
    let bytes = {
        // copy from String::replace
        let mut result = SmallVec::<_, 8>::with_capacity(s.len());
        let mut last_end = 0;
        for (start, part) in s.match_indices('_') {
            result.extend_from_slice(unsafe { s.get_unchecked(last_end..start) }.as_bytes());
            last_end = start + part.len();
        }
        result.extend_from_slice(unsafe { s.get_unchecked(last_end..s.len()) }.as_bytes());
        result
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
}

#[allow(unused_variables)]
#[cfg(test)]
mod tests {
    use logos::Logos;
    use smol_str::SmolStr;

    use crate::parser::{LexError, Token, Lexer};

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
        assert_lex_eq!("a", [Ok(Token::Ident(SmolStr::from("a")))]);
        assert_lex_eq!("a_bc", [Ok(Token::Ident(SmolStr::from("a_bc")))]);
        assert_lex_eq!("abc123", [Ok(Token::Ident(SmolStr::from("abc123")))]);
        assert_lex_eq!(
            "123abc",
            [
                Ok(Token::Integer(123)),
                Ok(Token::Ident(SmolStr::from("abc")))
            ]
        );
        assert_lex_eq!("中文", [Ok(Token::Ident(SmolStr::from("中文")))]);
    }

    #[test]
    fn lex_float() {
        assert_lex_eq!("123.45", [Ok(Token::Float(123.45))]);
        assert_lex_eq!("-67.89", [Ok(Token::Float(-67.89))]);
        assert_lex_eq!("123.45e6", [Ok(Token::Float(123.45e6))]);
        assert_lex_eq!("-67.89e-3", [Ok(Token::Float(-67.89e-3))]);
        assert_lex_eq!("123.45e+6", [Ok(Token::Float(123.45e+6))]);
        assert_lex_eq!("-67.89e-3", [Ok(Token::Float(-67.89e-3))]);
    }

    #[test]
    fn lex_integer() {
        assert_lex_eq!("1_230", [Ok(Token::Integer(1230))]);
        assert_lex_eq!("-456", [Ok(Token::Integer(-456))]);
        assert_lex_eq!("0b1010", [Ok(Token::Integer(10))]);
        assert_lex_eq!("0o777", [Ok(Token::Integer(511))]);
        assert_lex_eq!("0x12345678", [Ok(Token::Integer(0x12345678))]);
    }

    #[test]
    fn lex_escape_str() {
        assert_lex_eq!(
            r#" """ "#,
            |lex| [
                Ok(Token::String(lex.extras.intern(""))),
                Err(LexError::InvalidToken)
            ]
        );
        assert_lex_eq!(
            r#" "\"" "\n" "\q" "#,
            |lex| [
                Ok(Token::String(lex.extras.intern("\""))),
                Ok(Token::String(lex.extras.intern("\n"))),
                Err(LexError::InvalidEscapeChar('q'))
            ]
        );
    }

    #[test]
    fn lex_unicode_str() {
        assert_lex_eq!(
            r#" "\u{10FFFF}" "\u{10FFFX}" "\u{10FFFFF}""#,
            |lex| [
                Ok(Token::String(lex.extras.intern("\u{10FFFF}"))),
                Err(LexError::InvalidUnicodeChar('X')),
                Err(LexError::OverlongUnicodeEscape)
            ]
        );
    }

    #[test]
    fn lex_str() {
        assert_lex_eq!(
            r#" "foo" "0" "中文" "#,
            |lex| [
                Ok(Token::String(lex.extras.intern("foo"))),
                Ok(Token::String(lex.extras.intern("0"))),
                Ok(Token::String(lex.extras.intern("中文")))
            ]
        );
    }
}
