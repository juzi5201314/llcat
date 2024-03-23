#![feature(test)]

extern crate test;

use llcat_parser::parser::Parser;
use test::{black_box, Bencher};

#[bench]
fn bench_parse_block(b: &mut Bencher) {
    let mut parser = Parser::new("{6 * 6} - ({1 + 2; 3 + 4} + 5)").without_print_error();
    b.iter(|| {
        let _ = black_box(parser.parse());
    });
}
