use hashbrown::{HashMap, HashSet};

use crate::parser::{ast::*, parser, walk_bin_op, walk_expr, walk_program, ParseFromSource};
use crate::parser::{Atom, Visitor};
use crate::vm::bytecode;

use super::Instruction;

#[derive(Default)]
pub struct Compiler {
    funcs: HashMap<Atom, bytecode::Function>,

    symbols: HashMap<Atom, Vec<Symbol>>,
    locals: LocalScope,
    current_func: Option<Atom>,
}

impl Compiler {
    fn enter_func(&mut self, name: Atom) {
        self.funcs.insert(name, bytecode::Function::new());
        self.add_symbol(name, Symbol::Func);
        self.current_func = Some(name);
    }

    fn add_symbol(&mut self, name: Atom, ty: Symbol) {
        self.symbols.entry(name).or_insert_with(Vec::new).push(ty);
    }

    fn define_local(&mut self, name: Atom) {
        self.locals.define(name);
        self.add_symbol(name, Symbol::Local);
    }

    fn enter_scope(&mut self) {
        self.locals.enter();
    }

    fn exit_scope(&mut self) {
        let exited = self.locals.exit();
        for name in exited {
            let v = self
                .symbols
                .entry(name)
                .and_modify(|v| {
                    v.pop();
                })
                .or_default();
            if v.is_empty() {
                self.symbols.remove(&name);
            }
        }
    }

    fn instruction(&mut self, instruction: Instruction) {
        let f = self.current_func.expect("not in function context");
        self.funcs
            .get_mut(&f)
            .expect("can't find current function")
            .add_inst(instruction);
    }

    fn compile_src(&mut self, src: &str) -> bytecode::Module {
        let parser = parser();
        let program = parser.parse_src(src).unwrap();
        self.visit_program(&program);
        bytecode::Module {
            functions: self.funcs.clone(),
        }
    }
}

impl Visitor for Compiler {
    fn visit_program(&mut self, program: &Program) {
        self.enter_scope();
        walk_program(self, program);
        self.exit_scope();
    }

    fn visit_func_decl(&mut self, decl: &FuncDecl) {
        self.enter_func(decl.name);

        self.enter_scope();

        for param in &decl.params {
            self.define_local(*param);
        }

        for expr in &decl.body.exprs {
            self.visit_expr(expr);
        }

        self.exit_scope();
    }

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) {
        // 默认情况下, walk顺序是left -> op -> right
        // 但这是一个基于栈的虚拟机, 所以顺序应该是left -> right -> op
        self.visit_expr(&binary_expr.left);
        self.visit_expr(&binary_expr.right);
        self.visit_bin_op(&binary_expr.op);
    }

    fn visit_bin_op(&mut self, bin_op: &BinOp) {
        match bin_op {
            BinOp::Add => self.instruction(Instruction::Add),
            BinOp::Sub => self.instruction(Instruction::Sub),
            BinOp::Mul => self.instruction(Instruction::Mul),
            BinOp::Div => self.instruction(Instruction::Div),
            BinOp::Mod => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::BitAnd => todo!(),
            BinOp::BitOr => todo!(),
            BinOp::BitXor => todo!(),
            BinOp::Shl => todo!(),
            BinOp::Shr => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Ge => todo!(),
            BinOp::Gt => todo!(),
            BinOp::AddEq => todo!(),
            BinOp::SubEq => todo!(),
            BinOp::MulEq => todo!(),
            BinOp::DivEq => todo!(),
            BinOp::ModEq => todo!(),
            BinOp::BitAndEq => todo!(),
            BinOp::BitOrEq => todo!(),
            BinOp::BitXorEq => todo!(),
            BinOp::ShlEq => todo!(),
            BinOp::ShrEq => todo!(),
        }
    }

    fn visit_literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Float(n) => self.instruction(Instruction::ConstNumber(*n)),
            Literal::String(atom) => todo!(),
            Literal::Integer(i) => todo!(),
            Literal::Boolean(b) => self.instruction(Instruction::ConstByte(*b as u8)),
            Literal::Nil => self.instruction(Instruction::Nil),
        }
    }

    fn visit_ident_expr(&mut self, ident: &Atom) {
        // TODO: values ​​other than local
        let idx = self.locals.find(ident).expect("todo: undefined variable");
        self.instruction(Instruction::GetLocal(idx as u64));
    }
}

struct LocalScope {
    locals: HashMap<Atom, Vec<usize>>,
    current_scope: Vec<HashSet<Atom>>,
    inc_id: usize,
}

impl LocalScope {
    pub fn new() -> Self {
        LocalScope {
            locals: HashMap::new(),
            current_scope: vec![HashSet::new()],
            inc_id: 0,
        }
    }

    pub fn define(&mut self, name: Atom) {
        let new = self.current_scope.last_mut().unwrap().insert(name);

        if !new {
            return;
        }

        self.locals
            .entry(name)
            .or_insert_with(Vec::new)
            .push(self.inc_id);

        self.inc_id += 1;
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        self.locals.get(name).map(|v| v.last().copied()).flatten()
    }

    pub fn enter(&mut self) {
        self.current_scope.push(HashSet::new());
    }

    pub fn exit(&mut self) -> HashSet<Atom> {
        let locals = self.current_scope.pop().unwrap();
        for name in &locals {
            self.locals.entry(*name).and_modify(|v| {
                v.pop();
            });
        }
        locals
    }
}

impl Default for LocalScope {
    fn default() -> Self {
        LocalScope::new()
    }
}

pub enum Symbol {
    Local,
    Global,
    Func,
}

#[cfg(test)]
mod tests {
    use crate::parser::ToPrettyDoc;

    use super::Compiler;

    #[test]
    fn compile_func() {
        let src = r#"fn sum(a, b) { ret a + b }"#;
        let mut compiler = Compiler::default();
        let module = compiler.compile_src(src);
        println!("{}", module.to_pretty(50));
        dbg!(&module.functions["sum"].instructions);
    }
}
