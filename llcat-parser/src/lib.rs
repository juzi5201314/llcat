#![feature(type_alias_impl_trait)]
#![allow(dead_code)]

mod ast;
mod parser;
mod token;

#[test]
fn expr_test() {
    let src = "111";
    println!("{:?}", parser::parse_src(src));
}
