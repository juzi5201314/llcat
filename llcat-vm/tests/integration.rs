use llcat_vm::runtime::Runtime;
use llcat_vm::value::Value;

/* #[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc; */
#[global_allocator]
static ALLOC: mimalloc_rust::GlobalMiMalloc = mimalloc_rust::GlobalMiMalloc;

#[test]
fn sum_test() {
    let mut rt = Runtime::from_module(llcat_bytecode::from_source(include_str!(
        "../../examples/sum.llc"
    )));
    let ret = rt
        .call_func(0, vec![Value::Consti64(1), Value::Consti64(2)])
        .unwrap();
    assert_eq!(ret, Value::Consti64(3))
}

#[test]
fn fib_recursion_test() {
    let mut rt = Runtime::from_module(llcat_bytecode::from_source(include_str!(
        "../../examples/fib_recursion.llc"
    )));
    // 47: 369.20s(pool), 288.14s(offset)
    let ret = rt.call_func(0, vec![Value::Consti64(32)]).unwrap();
    
    assert_eq!(ret, Value::Consti64(2178309))
}

#[test]
fn fib_test() {
    let mut rt = Runtime::from_module(llcat_bytecode::from_source(include_str!(
        "../../examples/fib.llc"
    )));
    let ret = rt.call_func(0, vec![Value::Consti64(47)]).unwrap();
    assert_eq!(ret, Value::Consti64(2971215073))
}
