#![feature(test)]

use chumsky::span::SimpleSpan;
use llcat_parser::token::{lexer, Token};
use smallvec::SmallVec;
use test::{black_box, Bencher};

extern crate test;

type Consume = SmallVec<[(Token, SimpleSpan); 8]>;

#[bench]
fn bench_lex_str(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(lexer("ident").collect::<Consume>());
    });
}

#[bench]
fn bench_lex_str_on_heap(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(lexer("ident127335cb323292b8127335cb323292b8").collect::<Consume>());
    });
}

#[bench]
fn bench_lex_int(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(lexer("12345678").collect::<Consume>());
    });
}

#[bench]
fn bench_lex_int_on_heap(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(lexer("123456789").collect::<Consume>());
    });
}
