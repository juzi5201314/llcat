use ahash::AHashMap;
use petgraph::graph::NodeIndex;
use smol_str::SmolStr;

use llcat_parser::ast::{BinOp, Block, Decl, Expr, Literal, Stmt};

use super::block::{BasicBlock, FuncBlock};
use super::instruction::Instruction;
use super::module::Module;

pub trait ToByteCode {
    fn generate(&self, generator: &mut CodeGenerator);
}

#[derive(Default)]
struct GeneratorContext {
    curbb: u32,
    curfn: Option<FuncBlock>,

    if_num: usize,
    last_loop: u32,
}

pub struct CodeGenerator {
    pub module: Module,
    func_map: AHashMap<SmolStr, u32>,
    scope: Vec<Vec<SmolStr>>,

    ctx: GeneratorContext,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {
            module: Module::new(),
            func_map: AHashMap::new(),
            scope: Vec::new(),
            ctx: GeneratorContext::default(),
        }
    }

    fn enter_scope(&mut self) -> usize {
        self.scope.push(Vec::new());
        self.scope.len()
    }

    fn exit_scope(&mut self, id: usize) {
        assert_eq!(self.scope.len(), id, "inconsistent scope");
        self.scope.pop();
    }

    fn scope(&self) -> &Vec<SmolStr> {
        self.scope.last().expect("not in a scope")
    }

    fn scope_mut(&mut self) -> &mut Vec<SmolStr> {
        self.scope.last_mut().expect("not in a scope")
    }

    fn local(&self, id: &str) -> u32 {
        self.scope()
            .iter()
            .position(|x| id == x)
            .map(|x| x as u32)
            .expect(&format!("local variable {} not found", id))
    }

    fn add_bb(&mut self, bb: BasicBlock) -> u32 {
        let idx = self.ctx.curfn.as_mut().unwrap().add_block(bb);
        let name = self.bb_mut(idx).name();
        let node_idx = self.ctx.curfn.as_mut().unwrap().graph.add_node(name);
        assert_eq!(idx, node_idx.index() as u32);
        idx
    }

    fn set_bb(&mut self, idx: u32) {
        self.ctx.curbb = idx;
    }

    fn link_bb(&mut self, src: u32, dst: u32) {
        let g = &mut self.ctx.curfn.as_mut().unwrap().graph;
        g.add_edge(
            NodeIndex::new(src as usize),
            NodeIndex::new(dst as usize),
            (),
        );
    }

    fn set_func(&mut self, func: FuncBlock) {
        self.ctx.curfn = Some(func);
    }

    fn finsh_fn(&mut self) -> FuncBlock {
        self.ctx.curbb = 0;
        self.ctx.if_num = 0;
        self.ctx.curfn.take().expect("no func block")
    }

    fn bb_mut(&mut self, id: u32) -> &mut BasicBlock {
        self.ctx
            .curfn
            .as_mut()
            .unwrap()
            .blocks
            .get_mut(id as usize)
            .unwrap()
    }

    fn instruction(&mut self, instruction: Instruction) {
        self.bb_mut(self.ctx.curbb).instruction(instruction)
    }
}

impl ToByteCode for Decl {
    fn generate(&self, generator: &mut CodeGenerator) {
        match self {
            Decl::Fn {
                name,
                params,
                body,
                retrun_ty,
            } => {
                generator
                    .func_map
                    .insert(name.clone(), generator.func_map.len() as u32);
                let _ = retrun_ty;
                let mut func_block = FuncBlock::new();
                let entry = BasicBlock::with_label("entry");
                let scope = generator.enter_scope();

                func_block.params = params.len();
                generator.set_func(func_block);
                generator.add_bb(entry);
                for param in params {
                    generator.scope_mut().push(param.clone());
                }

                body.generate(generator);

                generator.exit_scope(scope);

                let func_block = generator.finsh_fn();
                generator.module.add_func(func_block);
            }
        }
    }
}

impl ToByteCode for Stmt {
    fn generate(&self, generator: &mut CodeGenerator) {
        match self {
            Stmt::Let(id, expr) => {
                expr.generate(generator);
                generator.instruction(Instruction::StoreLocal(generator.scope().len() as u32));
                generator.scope_mut().push(id.clone());
            }
            Stmt::Decl(_) => todo!(),
            Stmt::Expr(expr) => expr.generate(generator),
            Stmt::SemiExpr(expr) => expr.generate(generator),
            Stmt::Empty => todo!(),
            Stmt::Assign(id, expr) => {
                expr.generate(generator);
                generator.instruction(Instruction::StoreLocal(generator.local(id)));
            }
        }
    }
}

impl ToByteCode for Expr {
    fn generate(&self, generator: &mut CodeGenerator) {
        match self {
            Expr::Literal(lit) => lit.generate(generator),
            Expr::Ident(id) => {
                let id = generator.local(id);
                generator.instruction(Instruction::LoadLocal(id));
            }
            Expr::Binary(op, l, r) => {
                l.generate(generator);
                r.generate(generator);
                op.generate(generator);
                match op {
                    BinOp::AddEq
                    | BinOp::SubEq
                    | BinOp::DivEq
                    | BinOp::ModEq
                    | BinOp::MulEq
                    | BinOp::ShlEq
                    | BinOp::ShrEq
                    | BinOp::BitOrEq
                    | BinOp::BitAndEq
                    | BinOp::BitXorEq => {
                        if let Expr::Ident(id) = &**l {
                            generator.instruction(Instruction::StoreLocal(generator.local(&id)))
                        } else {
                            panic!("xxeq but left place is not ident")
                        }
                    }
                    _ => {}
                }
            }
            Expr::Unary(_, _) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If(cond, body, el) => {
                cond.generate(generator);
                let cur_id = generator.ctx.curbb;

                let body_id = generator.add_bb(BasicBlock::with_label("if"));
                let el_id = if el.is_some() {
                    generator.add_bb(BasicBlock::with_label("else"))
                } else {
                    // 0 is entry block, so it cannot be 0
                    0
                };
                let bb3 = generator.add_bb(BasicBlock::new());

                generator.link_bb(cur_id, body_id);
                generator.link_bb(body_id, bb3);

                // body
                generator.set_bb(body_id);
                body.generate(generator);
                generator.instruction(Instruction::Br(bb3));

                if let Some(block) = el {
                    generator.link_bb(cur_id, el_id);
                    generator.link_bb(el_id, bb3);
                    generator.set_bb(el_id);
                    block.generate(generator);
                    generator.instruction(Instruction::Br(bb3));
                }

                generator.set_bb(cur_id);
                generator.instruction(Instruction::BrIf(
                    body_id,
                    (el_id != 0).then(|| el_id).unwrap_or(bb3),
                ));

                generator.set_bb(bb3);
                generator.ctx.if_num += 1;
            }
            Expr::Loop(block) => {
                let loop_bb = generator.add_bb(BasicBlock::with_label("loop"));
                let next_bb = generator.add_bb(BasicBlock::new());
                generator.ctx.last_loop = loop_bb;
                generator.link_bb(generator.ctx.curbb, loop_bb);
                generator.link_bb(loop_bb, next_bb);

                generator.instruction(Instruction::Br(loop_bb));
                generator.set_bb(loop_bb);
                block.generate(generator);
                generator.instruction(Instruction::Br(loop_bb));
                generator.set_bb(next_bb);

            }
            Expr::Return(expr) => {
                expr.generate(generator);
                generator.instruction(Instruction::Ret);
            }
            Expr::Break => {
                let target_bb = generator.ctx.last_loop + 1;
                generator.instruction(Instruction::Br(target_bb));
                generator.link_bb(generator.ctx.curbb, target_bb);
            }
            Expr::Call(name, args) => {
                let id = *generator.func_map.get(name).unwrap();
                for arg in args.as_ref() {
                    arg.generate(generator)
                }
                generator.instruction(Instruction::FuncCall(id))
            }
        }
    }
}

impl ToByteCode for Literal {
    fn generate(&self, generator: &mut CodeGenerator) {
        match self {
            Literal::Interger(int) => generator.instruction(Instruction::Consti64(*int)),
            Literal::Float(_) => todo!(),
            Literal::Boolean(_) => todo!(),
            Literal::String(_) => todo!(),
        }
    }
}

impl ToByteCode for BinOp {
    fn generate(&self, generator: &mut CodeGenerator) {
        match self {
            BinOp::Add | BinOp::AddEq => generator.instruction(Instruction::Add),
            BinOp::Sub | BinOp::SubEq => generator.instruction(Instruction::Sub),
            BinOp::Mul | BinOp::MulEq => todo!(),
            BinOp::Div | BinOp::DivEq => todo!(),
            BinOp::Mod | BinOp::ModEq => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::BitAnd | BinOp::BitAndEq => todo!(),
            BinOp::BitOr | BinOp::BitOrEq => todo!(),
            BinOp::BitXor | BinOp::BitXorEq => todo!(),
            BinOp::Shl | BinOp::ShlEq => todo!(),
            BinOp::Shr | BinOp::ShrEq => todo!(),
            BinOp::Eq => generator.instruction(Instruction::Eq),
            BinOp::Lt => generator.instruction(Instruction::Lt),
            BinOp::Le => generator.instruction(Instruction::Le),
            BinOp::Ne => generator.instruction(Instruction::Ne),
            BinOp::Ge => generator.instruction(Instruction::Ge),
            BinOp::Gt => generator.instruction(Instruction::Gt),
        }
    }
}

impl ToByteCode for Block {
    fn generate(&self, generator: &mut CodeGenerator) {
        for stmt in &self.stmts {
            stmt.generate(generator);
        }
    }
}
