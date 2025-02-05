use std::hint::black_box;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use llcat::parser::Token;
use logos::Logos;

fn bench_lexer(c: &mut Criterion) {
    c.benchmark_group("lex: literal")
        .bench_function("string", |b| {
            let src = r#" "foo\nbar" "#.repeat(100);
            b.iter_batched_ref(
                || {
                    let lexer = Token::lexer(&src);
                    lexer
                },
                |lexer| {
                    for token in lexer {
                        let _ = black_box(token);
                    }
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("int", |b| {
            let src = r#" 123_456 "#.repeat(100);
            b.iter_batched_ref(
                || {
                    let lexer = Token::lexer(&src);
                    lexer
                },
                |lexer| {
                    for token in lexer {
                        let _ = black_box(token);
                    }
                },
                BatchSize::SmallInput,
            );
        });
}

criterion_group!(benches, bench_lexer);
criterion_main!(benches);
