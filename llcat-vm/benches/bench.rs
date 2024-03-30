#![feature(test)]

use llcat_vm::runtime::Runtime;
use llcat_vm::value::Value;
use test::{black_box, Bencher};

extern crate test;

#[bench]
fn recursion_bench(b: &mut Bencher) {
    let mut rt = Runtime::from_module(llcat_bytecode::from_source(include_str!(
        "../../examples/fib_recursion.llc"
    )));
    b.iter(|| {
        let _ = black_box(rt.call_func(0, [Value::Consti64(16)]).unwrap());
    });
}

#[bench]
fn fib_bench(b: &mut Bencher) {
    let mut rt = Runtime::from_module(llcat_bytecode::from_source(include_str!(
        "../../examples/fib.llc"
    )));
    b.iter(|| {
        let _ = black_box(rt.call_func(0, [Value::Consti64(16)]).unwrap());
    });
}
