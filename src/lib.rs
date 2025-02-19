#![feature(allocator_api)]
#![feature(box_patterns)]

pub mod parser;
pub mod bytecode;
pub mod vm;
pub mod arena;

#[macro_export]
macro_rules! pub_use_mod {
    ($($m:ident),+) => {
        $(pub mod $m; #[allow(unused_imports)] pub use $m::*;)+
    };
}

#[macro_export]
macro_rules! args {
    ($($val:expr),* $(,)?) => {
        [$(crate::vm::Value::from($val)),*]
    };
}
