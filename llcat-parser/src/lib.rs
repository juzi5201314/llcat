#![feature(type_alias_impl_trait)]
#![allow(dead_code)]

pub mod ast;
pub mod parser;
pub mod token;

#[test]
fn expr_test() {
    let src = "\"true\"";
    println!("{:?}", parser::parse_src(src));
}
