#![feature(test)]

use test::{black_box, Bencher};

extern crate test;

#[bench]
fn bench_lex_str(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(llcat_parser::parser::parse_src("ident127335cb323292b8127335cb323292b8"));
    });
}

#[bench]
fn bench_lex_int(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(llcat_parser::parser::parse_src("12345678"));
    });
}

#[bench]
fn bench_lex_int_on_heap(b: &mut Bencher) {
    b.iter(|| {
        let _ = black_box(llcat_parser::parser::parse_src("123456789"));
    });
}