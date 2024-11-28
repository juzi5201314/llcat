use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use llcat::parser::intern2;
use rand::distr::DistString;
use std::{hint::black_box, num::NonZero};

fn new_lasso() -> lasso::Rodeo {
    lasso::Rodeo::with_capacity(lasso::Capacity::new(0, NonZero::new(1).unwrap()))
}

fn new_intern2() -> intern2::InternPool {
    intern2::InternPool::with_capacity(1, 0)
}

fn bench_intern(c: &mut Criterion) {
    let (mut lasso, intern2) = (new_lasso(), new_intern2());
    c.benchmark_group("intern: [rand new 12]")
        .bench_function("lasso", |b| {
            b.iter_batched(
                || random_str(12),
                |input| {
                    let _ = black_box(lasso.get_or_intern(&input));
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern2", |b| {
            b.iter_batched(
                || random_str(12),
                |input| {
                    let _ = black_box(intern2.alloc(&input));
                },
                BatchSize::SmallInput,
            );
        });

    let (mut lasso, intern2) = (new_lasso(), new_intern2());
    c.benchmark_group("intern: [rand new 32]")
        .bench_function("lasso", |b| {
            b.iter_batched(
                || random_str(32),
                |input| {
                    let _ = black_box(lasso.get_or_intern(&input));
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern2", |b| {
            b.iter_batched(
                || random_str(32),
                |input| {
                    let _ = black_box(intern2.alloc(&input));
                },
                BatchSize::SmallInput,
            );
        });

    c.benchmark_group("intern: [access]")
        .bench_function("lasso", |b| {
            b.iter_batched(
                || {
                    let mut pool = new_lasso();
                    let input = random_str(24);
                    let atom = pool.get_or_intern(&input);
                    (input, atom, pool)
                },
                |(input, atom, pool)| {
                    let atom = black_box(pool.resolve(&atom));
                    assert!(atom == &input);
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern2", |b| {
            b.iter_batched(
                || {
                    let pool = new_intern2();
                    let input = random_str(24);
                    let atom = pool.alloc(&input);
                    (input, atom)
                },
                |(input, atom)| {
                    let atom = black_box(atom.s);
                    assert!(atom == &input);
                },
                BatchSize::SmallInput,
            );
        });

    c.benchmark_group("intern: [clone]")
        .bench_function("lasso", |b| {
            b.iter_batched(
                || {
                    let mut pool = new_lasso();
                    let input = random_str(24);
                    let atom = pool.get_or_intern(&input);
                    atom
                },
                |atom| {
                    let _ = black_box(atom.clone());
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern2", |b| {
            b.iter_batched(
                || {
                    let pool = new_intern2();
                    let input = random_str(24);
                    let atom = pool.alloc(&input);
                    atom
                },
                |atom| {
                    let _ = black_box(atom.clone());
                },
                BatchSize::SmallInput,
            );
        });
}

#[inline(always)]
fn random_str(len: usize) -> String {
    rand::distr::StandardUniform.sample_string(&mut rand::rng(), len)
}

criterion_group!(benches, bench_intern);
criterion_main!(benches);
