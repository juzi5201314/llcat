use std::fs::File;
use std::io::{copy, Write};
use std::process::{Command, Stdio};

use llcat_bytecode::bytecode::generator::ToByteCode;
use llcat_bytecode::bytecode::CodeGenerator;
use llcat_parser::ast::Item;
use petgraph::dot::Dot;

#[test]
fn a_test() {
    let mut parser = llcat_parser::parser::Parser::new(include_str!("../../examples/fib.llc"));
    let ast = parser.parse().unwrap();

    let mut generator = CodeGenerator::new();

    for decl in ast.items {
        if let Item::Decl(decl) = decl {
            decl.generate(&mut generator);
        }
    }
    for func in &generator.module.func_blocks {
        let mut output = File::create("func_flow.svg").unwrap();
        let dot = Dot::with_config(&func.graph, &[petgraph::dot::Config::EdgeNoLabel]);
        let mut cmd = Command::new("dot")
            .arg("-Tsvg")
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        let stdin = cmd.stdin.as_mut().unwrap();
        stdin.write_all(format!("{:?}", dot).as_bytes()).unwrap();
        stdin.flush().unwrap();
        dbg!(cmd.wait().unwrap());
        copy(cmd.stdout.as_mut().unwrap(), &mut output).unwrap();
        output.flush().unwrap();
    }

    dbg!(&generator.module);
}
