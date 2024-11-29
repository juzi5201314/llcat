#![feature(str_from_raw_parts)]
#![feature(allocator_api)]
#![feature(min_specialization)]

pub mod parser;
pub mod arena;

#[macro_export]
macro_rules! pub_use_mod {
    ($($m:ident),+) => {
        $(pub mod $m; #[allow(unused_imports)] pub use $m::*;)+
    };
}