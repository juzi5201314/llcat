use std::rc::Rc;

// compiler.rs
use crate::bytecode::{self, Instruction};
use crate::parser::{
    Atom, BinOp, BinaryExpr, Declaration, Expr, ExprBlock, ExprKind, FnCallExpr, FuncDecl, Literal,
    ParserHelper, Program, Span, SpannedAtom, UnOp, Visitor,
};
use hashbrown::HashMap;

use super::{Constant, Function};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol {
    Func,
    Local,
}

#[derive(Debug, Clone)]
struct Locals {
    scopes: Vec<HashMap<Atom, usize>>, // Atom -> offset
    depth: usize,
    count: usize, // Total number of locals
}

impl Locals {
    fn new() -> Self {
        Locals {
            scopes: vec![HashMap::new()],
            depth: 0,
            count: 0,
        }
    }

    fn enter(&mut self) {
        self.depth += 1;
        self.scopes.push(HashMap::new());
    }

    fn exit(&mut self) -> Vec<Atom> {
        self.depth -= 1;
        let exited = self.scopes.pop().unwrap();
        exited.keys().cloned().collect()
    }

    fn define(&mut self, name: Atom) -> usize {
        self.scopes.last_mut().unwrap().insert(name, self.count);
        self.count += 1;
        self.count - 1
    }

    fn resolve(&self, name: &Atom) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(offset) = scope.get(name) {
                return Some(*offset);
            }
        }
        None
    }
}

pub struct Compiler {
    funcs: HashMap<String, Function>,
    current_func: Option<Atom>,
    symbols: HashMap<Atom, Vec<Symbol>>,
    locals: Locals,
    constants: Vec<Constant>,
    constant_indices: HashMap<Constant, usize>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            funcs: HashMap::new(),
            current_func: None,
            symbols: HashMap::new(),
            locals: Locals::new(),
            constants: Vec::new(),
            constant_indices: HashMap::new(),
        }
    }

    fn enter_func(&mut self, name: Atom, arity: usize) {
        self.funcs
            .insert(name.as_ref().to_owned(), Function::new(arity));
        self.add_symbol(name, Symbol::Func);
        self.current_func = Some(name);
        self.locals = Locals::new(); // reset locals for each function
    }

    fn current_func(&mut self) -> &mut Function {
        let name = self.current_func.expect("not in function context");
        self.funcs.get_mut(&name).unwrap()
    }

    fn add_symbol(&mut self, name: Atom, ty: Symbol) {
        self.symbols.entry(name).or_insert_with(Vec::new).push(ty);
    }

    fn define_local(&mut self, name: Atom) -> usize {
        let idx = self.locals.define(name);
        self.add_symbol(name, Symbol::Local);
        idx
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

    fn emit(&mut self, instruction: Instruction, span: Span) {
        let name = self.current_func.expect("not in function context");
        let func = self
            .funcs
            .get_mut(&name)
            .expect("can't find current function");
        func.add_inst(instruction);
        // todo: u32::MAX size source file
        func.spans.push(span.start as u32..span.end as u32);
    }

    fn constant(&mut self, constant: Constant) -> usize {
        *self.constant_indices.entry(constant).or_insert_with(|| {
            let idx = self.constants.len();
            self.constants.push(constant);
            idx
        })
    }

    pub fn compile_module(&mut self, src: &str) -> bytecode::Module {
        let parser = crate::parser::parser();
        let program = parser.parse_from_str("[test]", src).unwrap();
        self.visit_program(&program);
        bytecode::Module {
            funcs: self
                .funcs
                .iter()
                .map(|(name, func)| (name.clone(), Rc::new(func.clone())))
                .collect(),
            constants: self.constants.clone(),
        }
    }
}

impl Visitor for Compiler {
    fn visit_program(&mut self, program: &Program) {
        for decl in &program.decls {
            self.visit_declaration(decl);
        }
    }

    fn visit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Func(func_decl) => self.visit_func_decl(func_decl),
        }
    }

    fn visit_func_decl(&mut self, decl: &FuncDecl) {
        self.enter_func(decl.name.atom.clone(), decl.params.len());

        // Define parameters as local variables
        self.enter_scope();
        for param in decl.params.iter() {
            self.define_local(param.atom.clone());
            //self.emit(Instruction::SetLocal(param.to_string()), param.span.clone());
        }

        self.visit_expr(&decl.body);
        self.emit(Instruction::Return, decl.body.span.clone());

        self.exit_scope();

        self.current_func = None;
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(literal) => self.visit_literal(literal),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            ExprKind::BinaryExpr(binary_expr) => self.visit_binary_expr(binary_expr),
            ExprKind::BlockExpr(block_expr) => self.visit_expr_block(block_expr),
            ExprKind::IfExpr(if_expr) => self.visit_if_expr(if_expr),
            ExprKind::LetExpr(let_expr) => self.visit_let_expr(let_expr),
            ExprKind::Return(ret_expr) => {
                self.visit_expr(ret_expr);
                self.emit(Instruction::Return, expr.span.clone());
            }
            ExprKind::Loop(_) => todo!(),
            ExprKind::FnCall(fn_call_expr) => self.visit_fn_call_expr(fn_call_expr),
            ExprKind::UnaryExpr(unary_expr) => self.visit_unary_expr(unary_expr),
            ExprKind::Array(_) => todo!(),
            ExprKind::ArrayIndexExpr(_) => todo!(),
        }
    }

    fn visit_expr_block(&mut self, block_expr: &ExprBlock) {
        self.enter_scope();
        for expr in &block_expr.exprs {
            self.visit_expr(expr);
        }
        self.exit_scope();
    }

    /// 'cond
    /// jump.if.false 'else
    /// 'then
    /// jump 'end
    /// 'else
    /// 'end
    fn visit_if_expr(&mut self, if_expr: &crate::parser::IfExpr) {
        self.visit_expr(&if_expr.cond);

        let jump_if_false_inst = self.current_func().instructions.len();
        self.emit(Instruction::JumpIfFalse(0), if_expr.cond.span.clone());

        self.visit_expr(&if_expr.then_expr);

        let jump_inst = self.current_func().instructions.len();
        if if_expr.else_expr.is_some() {
            self.emit(Instruction::Jump(0), if_expr.cond.span.clone());
        }

        self.current_func().instructions[jump_if_false_inst] =
            Instruction::JumpIfFalse(self.current_func().instructions.len());

        if let Some(else_expr) = &if_expr.else_expr {
            self.visit_expr(else_expr);

            self.current_func().instructions[jump_inst] =
                Instruction::Jump(self.current_func().instructions.len());
        }
    }

    fn visit_let_expr(&mut self, let_expr: &crate::parser::LetExpr) {
        self.visit_expr(&let_expr.value);
        self.emit(
            Instruction::SetLocal(let_expr.name.as_ref().to_owned()),
            let_expr.name.span.clone(),
        );
        self.define_local(let_expr.name.atom.clone());
    }

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) {
        self.visit_expr(&binary_expr.left);
        self.visit_expr(&binary_expr.right);
        self.visit_bin_op(&binary_expr.op);
    }

    fn visit_unary_expr(&mut self, unary_expr: &crate::parser::UnaryExpr) {
        let span = unary_expr.expr.span.clone();
        self.visit_expr(&unary_expr.expr);
        match unary_expr.op {
            UnOp::Neg => {
                self.emit(Instruction::Neg, span);
            }
            UnOp::Not => self.emit(Instruction::Not, span),
        }
    }

    fn visit_bin_op(&mut self, bin_op: &BinOp) {
        // todo
        let span = 0..0;
        match bin_op {
            BinOp::Add => self.emit(Instruction::Add, span),
            BinOp::Sub => self.emit(Instruction::Sub, span),
            BinOp::Mul => self.emit(Instruction::Mul, span),
            BinOp::Div => self.emit(Instruction::Div, span),
            BinOp::Mod => self.emit(Instruction::Mod, span),
            BinOp::And => self.emit(Instruction::And, span),
            BinOp::Or => self.emit(Instruction::Or, span),
            BinOp::BitAnd => todo!(),
            BinOp::BitOr => todo!(),
            BinOp::BitXor => todo!(),
            BinOp::Shl => todo!(),
            BinOp::Shr => todo!(),
            BinOp::Eq => self.emit(Instruction::Eq, span),
            BinOp::Lt => self.emit(Instruction::Lt, span),
            BinOp::Le => self.emit(Instruction::Le, span),
            BinOp::Ne => self.emit(Instruction::NotEq, span),
            BinOp::Ge => self.emit(Instruction::Ge, span),
            BinOp::Gt => self.emit(Instruction::Gt, span),
            BinOp::AddAssign => todo!(),
            BinOp::SubAssign => todo!(),
            BinOp::MulAssign => todo!(),
            BinOp::DivAssign => todo!(),
            BinOp::ModAssign => todo!(),
            BinOp::BitAndAssign => todo!(),
            BinOp::BitOrAssign => todo!(),
            BinOp::BitXorAssign => todo!(),
            BinOp::ShlAssign => todo!(),
            BinOp::ShrAssign => todo!(),
            BinOp::Assign => todo!(),
        }
    }

    fn visit_literal(&mut self, literal: &Literal) {
        let constant = match literal {
            Literal::Integer(i) => Constant::Integer(*i),
            Literal::Float(f) => Constant::Float(*f),
            Literal::String(s) => Constant::String(s.atom),
            Literal::Boolean(b) => Constant::Boolean(*b),
            Literal::Nil => Constant::Nil,
        };
        let idx = self.constant(constant);
        self.emit(Instruction::Const(idx), 0..0);
    }

    fn visit_ident_expr(&mut self, ident: &SpannedAtom) {
        if let Some(offset) = self.locals.resolve(ident) {
            self.emit(Instruction::GetLocal(offset), ident.span.clone());
        } else {
            self.emit(
                Instruction::GetVariable(ident.as_ref().to_owned()),
                ident.span.clone(),
            );
        }
    }

    fn visit_fn_call_expr(&mut self, fn_call_expr: &FnCallExpr) {
        for arg in &fn_call_expr.args {
            self.visit_expr(arg);
        }

        self.emit(
            Instruction::Call(
                Rc::new(fn_call_expr.name.as_ref().to_owned()),
                fn_call_expr.args.len(),
            ),
            0..0,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::Compiler;

    #[test]
    fn _expr() {
        let mut compiler = Compiler::new();
        //let module = compiler.compile_module("fn if_expr() = { if true { 1 } else { 2 } }");
        let module = compiler.compile_module("fn sum(a, b) = { a + b }");
        println!("{:#?}", module);
    }
}
