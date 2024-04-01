#![feature(box_patterns)]

use bytecode::generator::ToByteCode;
use bytecode::{CodeGenerator, Module};
use llcat_parser::ast::Item;

pub mod bytecode;

pub fn from_source(source: &str) -> Module {
    let mut parser = llcat_parser::parser::Parser::new(source);
    let ast = parser.parse().unwrap();

    let mut generator = CodeGenerator::new();

    for decl in ast.items {
        if let Item::Decl(decl) = decl {
            decl.generate(&mut generator);
        }
    }

    generator.module
}