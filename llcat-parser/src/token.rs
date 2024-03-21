use std::fmt::Display;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};
use smallvec::SmallVec;
use smol_str::SmolStr;

use crate::small_vec::{SmallVec32, SmallVec8};

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    Error,

    // literal
    #[regex(r"-?[_0-9]+", lex_i64, priority = 10)]
    #[regex(r"-?0b[_0-1]+", lex_i64)]
    #[regex(r"-?0o[_0-7]+", lex_i64)]
    #[regex(r"-?0x[_0-9a-f]+", lex_i64)]
    Interger(i64),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    #[regex(r#""([^"\\]|\\["\\0abnfrt]|\\u\{[a-fA-F0-9]{6}\})*""#, lex_string)]
    String(SmolStr),
    #[token("false", |_| false, priority = 100)]
    #[token("true", |_| true, priority = 100)]
    Boolean(bool),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| SmolStr::from(lex.slice()), priority = 20)]
    Ident(SmolStr),

    // keyword
    #[token("fn")]
    KeywordFn,
    #[token("let")]
    KeywordLet,

    // control flow
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("loop")]
    KeywordLoop,
    #[token("br")]
    KeywordBreak,
    #[token("ret")]
    KeywordRet,

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

fn lex_delimiter(lex: &mut Lexer<Token>) -> Option<Delimiter> {
    Some(match lex.slice() {
        "(" | ")" => Delimiter::Parenthesis,
        "{" | "}" => Delimiter::Brace,
        "[" | "]" => Delimiter::Bracket,
        _ => return None,
    })
}

fn lex_i64(lex: &mut Lexer<Token>) -> Option<i64> {
    let s = lex.slice();
    let bytes = {
        // copy from String::replace
        //let mut result = String::new_in(lex.extras);
        let mut result = SmallVec8::new();
        let mut last_end = 0;
        for (start, part) in s.match_indices('_') {
            result.extend_from_slice(unsafe { s.get_unchecked(last_end..start) }.as_bytes());
            last_end = start + part.len();
        }
        result.extend_from_slice(unsafe { s.get_unchecked(last_end..s.len()) }.as_bytes());
        result
    };
    let s = unsafe { std::str::from_utf8_unchecked(&bytes) };
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
    let mut s = SmallVec32::with_capacity(lex.slice().len());
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
                let unicode =
                    u32::from_str_radix(std::str::from_utf8(unicode).unwrap(), 16).unwrap();
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

pub fn token_stream<'s>(
    tokens: impl Iterator<Item = (Token, SimpleSpan)> + Clone,
    eoi: usize,
) -> SpannedInput<Token, SimpleSpan, Stream<impl Iterator<Item = (Token, SimpleSpan)> + Clone>> {
    let stream = Stream::from_iter(tokens.clone()).spanned(SimpleSpan::from(eoi..eoi));

    stream
}

macro_rules! write_token_display {
    ($f:expr, $tok:expr => { $($pat_arm:pat => $output:expr,)* }) => {
        match $tok {
            $($pat_arm => write!($f, "{}", $output)),*
        }
    };
}

impl Display for Token {
    #[allow(unreachable_code)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_token_display!(f, self => {
            Token::Error => "[Error Token]",
            Token::Interger(i) => i,
            Token::Float(float) => *float,
            Token::String(s) => format!("\"{}\"", s),
            Token::Boolean(b) => b,
            Token::Ident(id) => id,
            Token::KeywordFn => "fn",
            Token::KeywordRet => "ret",
            Token::KeywordLet => "let",
            Token::KeywordIf => "if",
            Token::KeywordElse => "else",
            Token::KeywordLoop => "loop",
            Token::KeywordBreak => "br",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Percent => "%",
            Token::Caret => "^",
            Token::Not => "!",
            Token::And => "&",
            Token::Or => "or",
            Token::AndAnd => "&&",
            Token::OrOr => "||",
            Token::Shl => "<<",
            Token::Shr => ">>",
            Token::PlusEq => "+=",
            Token::MinusEq => "-=",
            Token::StarEq => "*=",
            Token::SlashEq => "/=",
            Token::PercentEq => "%=",
            Token::CaretEq => "^=",
            Token::AndEq => "&=",
            Token::OrEq => "|=",
            Token::ShlEq => "<<=",
            Token::ShrEq => ">>=",
            Token::Eq => "=",
            Token::EqEq => "==",
            Token::Ne => "!=",
            Token::Gt => ">",
            Token::Lt => "<",
            Token::Ge => ">=",
            Token::Le => "<=",
            Token::At => "@",
            Token::Underscore => "_",
            Token::Dot => ".",
            Token::Semi => ";",
            Token::Colon => ":",
            Token::PathSep => "::",
            Token::RArrow => "->",
            Token::FatArrow => "=>",
            Token::Pound => "#",
            Token::Dollar => "$",
            Token::Question => "?",
            Token::Tilde => "~",
            Token::SingleQuotationMark => "'",
            Token::Comma => ",",
            Token::OpenDelimiter(delimiter) => match delimiter {
                Delimiter::Parenthesis => "(",
                Delimiter::Brace => "{",
                Delimiter::Bracket => "[",
            },
            Token::CloseDelimiter(delimiter) => match delimiter {
                Delimiter::Parenthesis => ")",
                Delimiter::Brace => "}",
                Delimiter::Bracket => "]",
            },
        })
    }
}
