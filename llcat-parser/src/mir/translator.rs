use petgraph::Graph;
use smol_str::SmolStr;

use crate::ast::{Decl, Expr, Literal, Stmt};

use super::basic_block::BasicBlock;
use super::module::{FuncBlock, Module};

pub struct Translator {
    pub module: Module,
    graph: Graph<BasicBlock, ()>,
    ctx: TranslatorContext,
}

struct TranslatorContext {
    current_func: Option<FuncBlock>,
}

pub trait Translatable {
    fn translate(&self, translator: &mut Translator);
}

impl Default for TranslatorContext {
    fn default() -> Self {
        Self {
            current_func: None,
        }
    }
}

impl Translator {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            ctx: TranslatorContext::default(),
            module: Module::new(),
        }
    }

    pub fn create_func(&mut self, id: SmolStr, block: FuncBlock) {
        self.module.blocks.insert(id, block);
    }
}

impl Translatable for Decl {
    fn translate(&self, translator: &mut Translator) {
        match self {
            Decl::Fn {
                name,
                params,
                body,
                retrun_ty,
            } => {
                let mut func_block = FuncBlock::new();
                func_block.locals.push(SmolStr::new_inline("r0"));
                func_block.locals.extend(params.clone());
                func_block.blocks.push(BasicBlock::new());

                translator.ctx.current_func = Some(func_block);
                for stmt in &body.stmts {
                    stmt.translate(translator)
                }

                let func_block = translator.ctx.current_func.take().unwrap();
                translator.create_func(name.clone(), func_block)
            }
        }
    }
}

impl Translatable for Stmt {
    fn translate(&self, translator: &mut Translator) {
        let mut func_block = translator.ctx.current_func.as_mut().unwrap();
        match self {
            Stmt::Let(id, expr) => {
                func_block.locals.push(id.clone());
            },
            Stmt::Decl(_) => todo!(),
            Stmt::Expr(expr) => {
                expr.translate(translator);
            },
            Stmt::SemiExpr(expr) => expr.translate(translator),
            Stmt::Empty => todo!(),
        }
    }
}

impl Translatable for Expr {
    fn translate(&self, translator: &mut Translator) {
        match self {
            Expr::Literal(lit) => match lit {
                Literal::Interger(int) => todo!(),
                Literal::Float(_) => todo!(),
                Literal::Boolean(_) => todo!(),
                Literal::String(_) => todo!(),
            },
            Expr::Ident(_) => todo!(),
            Expr::Binary(_, _, _) => todo!(),
            Expr::Unary(_, _) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If(_, _, _) => todo!(),
            Expr::Loop(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Break => todo!(),
            Expr::Call(_, _) => todo!(),
        }
    }
}