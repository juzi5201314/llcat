use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use llcat::parser::{intern, intern_old, Atom};
use rand::distr::DistString;
use std::{hint::black_box, num::NonZero};

fn new_lasso() -> lasso::Rodeo {
    lasso::Rodeo::with_capacity(lasso::Capacity::new(0, NonZero::new(1).unwrap()))
}

fn new_intern2() -> intern_old::InternPool {
    intern_old::InternPool::with_capacity(1, 0)
}

fn new_intern() -> intern::ThreadLocalPool {
    intern::ThreadLocalPool::with_capacity(1, 0)
}

fn bench_intern(c: &mut Criterion) {
    let (mut lasso, intern2, mut intern) = (new_lasso(), new_intern2(), new_intern());
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
        .bench_function("intern", |b| {
            b.iter_batched(
                || random_str(12),
                |input| {
                    let _ = black_box(unsafe { Atom::new_in(&input, &mut intern) });
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

    let (mut lasso, intern2, mut intern) = (new_lasso(), new_intern2(), new_intern());
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
        .bench_function("intern", |b| {
            b.iter_batched(
                || random_str(32),
                |input| {
                    let _ = black_box(unsafe { Atom::new_in(&input, &mut intern) });
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern(owned)", |b| {
            b.iter_batched(
                || random_str(32),
                |input| {
                    let _ = black_box(unsafe { Atom::new_in(input, &mut intern) });
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

    let (mut lasso, intern2, mut intern) = (new_lasso(), new_intern2(), new_intern());
    let sample = random_str(32);
    c.benchmark_group("intern: [same new 32]")
        .bench_function("lasso", |b| {
            b.iter_batched(
                || sample.clone(),
                |input| {
                    let _ = black_box(lasso.get_or_intern(&input));
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern", |b| {
            b.iter_batched(
                || sample.clone(),
                |input| {
                    let _ = black_box(unsafe { Atom::new_in(&input, &mut intern) });
                },
                BatchSize::SmallInput,
            );
        })
        .bench_function("intern2", |b| {
            b.iter_batched(
                || sample.clone(),
                |input| {
                    let _ = black_box(intern2.alloc(&input));
                },
                BatchSize::SmallInput,
            );
        });

    let (mut lasso, intern2, mut intern) = (new_lasso(), new_intern2(), new_intern());
    let input = random_str(24);
    let (lasso_atom, intern_atom, intern2_atom) = (
        lasso.get_or_intern(&input),
        unsafe { Atom::new_in(&input, &mut intern) },
        intern2.alloc(&input),
    );
    c.benchmark_group("intern: [access]")
        .bench_function("lasso", |b| {
            b.iter(|| {
                let atom = black_box(lasso.resolve(&lasso_atom));
                assert!(atom == &input);
            });
        })
        .bench_function("intern", |b| {
            b.iter(|| {
                let atom = black_box(intern_atom.as_ref());
                assert!(atom == &input);
            });
        })
        .bench_function("intern2", |b| {
            b.iter(|| {
                let atom = black_box(intern2_atom.as_ref());
                assert!(atom == &input);
            });
        });

    let (mut lasso, intern2, mut intern) = (new_lasso(), new_intern2(), new_intern());
    let input = random_str(24);
    let (lasso_atom, intern_atom, intern2_atom) = (
        lasso.get_or_intern(&input),
        unsafe { Atom::new_in(&input, &mut intern) },
        intern2.alloc(&input),
    );
    c.benchmark_group("intern: [clone]")
        .bench_function("lasso", |b| {
            b.iter(|| {
                let _ = black_box(lasso_atom.clone());
            });
        })
        .bench_function("intern", |b| {
            b.iter(|| {
                let _ = black_box(intern_atom.clone());
            });
        })
        .bench_function("intern2", |b| {
            b.iter(|| {
                let _ = black_box(intern2_atom.clone());
            });
        });
}

#[inline(always)]
fn random_str(len: usize) -> String {
    rand::distr::StandardUniform.sample_string(&mut rand::rng(), len)
}

criterion_group!(benches, bench_intern);
criterion_main!(benches);
