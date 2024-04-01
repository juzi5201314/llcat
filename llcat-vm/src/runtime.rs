use std::mem::size_of;
use std::rc::Rc;

use llcat_bytecode::bytecode::{Instruction, Module};

use crate::context::RuntimeContext;
use crate::error::Error;
use crate::value::Value;

pub struct Runtime {
    module: Rc<Module>,
    ctx: RuntimeContext,
    //ctx_pool: Pool<RuntimeContext>,
}

impl Runtime {
    pub fn from_module(module: Module) -> Self {
        Runtime {
            module: Rc::new(module),
            ctx: RuntimeContext::new(),
            //ctx_pool: Pool::with_capacity(16, RuntimeContext::new),
        }
    }

    pub fn call_func<A>(&mut self, func_id: u32, args: A) -> Result<Value, Error>
    where
        A: IntoIterator<Item = Value>,
    {
        self.ctx.stack.raw.extend(args);
        self.__call_func(func_id)
    }

    // 展开递归，但性能更差
    /* fn __call_func(&mut self, mut func_id: u32) -> Result<Value, Error> {
        #[derive(Default, Clone, Copy)]
        struct FnCtx {
            func_id: u32,
            bb_idx: u32,
            inst_offset: usize,
            local_offset: usize,
            stack_offset: usize,
            old_local_offset: usize,
        }

        let module = self.module.clone();
        let mut fnctxs = [FnCtx::default(); 128];
        let mut ctx_idx = 0;
        let mut back = false;

        'start: loop {
            let mut bb_idx = 0;
            let mut inst_offset = 0;
            let mut stack_offset = self.ctx.stack.raw.len();
            let mut local_offset = self.ctx.local_table.len();
            let mut old_local_offset = self.ctx.local_offset;

            if back {
                if let Some(ctx) = fnctxs.get(ctx_idx - 1) {
                    ctx_idx = ctx_idx - 1;
                    //dbg!(ctx_idx);
                    func_id = ctx.func_id;
                    bb_idx = ctx.bb_idx;
                    inst_offset = ctx.inst_offset;
                    stack_offset = ctx.stack_offset;
                    local_offset = ctx.local_offset;
                    old_local_offset = ctx.old_local_offset;
                }
            }

            let func = module
                .func_blocks
                .get(func_id as usize)
                .ok_or_else(|| Error::FuncNotExist(func_id as usize))?;
            //let mut ctx = self.ctx_pool.alloc();

            if self.ctx.stack.raw.len() < func.params {
                return Err(Error::FuncParamsNotMatch(
                    func.params,
                    self.ctx.stack.raw.len(),
                ));
            }

            self.ctx.local_offset = local_offset;

            if !back {
                self.ctx.local_table.extend(
                    self.ctx
                        .stack
                        .raw
                        .drain(self.ctx.stack.raw.len() - func.params..),
                );
                stack_offset -= func.params;
            }

            // enter function context
            //std::mem::swap(&mut self.ctx, &mut ctx);

            let mut current_bb = func.blocks.get(bb_idx as usize).unwrap().insts.0.iter();

            if inst_offset > 0 {
                current_bb.nth(inst_offset - 1);
            }

            loop {
                let Some(inst) = current_bb.next() else {
                    break;
                };
                inst_offset += 1;
                match self.instruction(*inst)? {
                    Control::Continue => continue,
                    Control::Br(idx) => {
                        current_bb = func.blocks.get(idx as usize).unwrap().insts.0.iter();
                        bb_idx = idx;
                        inst_offset = 0;
                        continue;
                    }
                    Control::Return => {
                        break;
                    }
                    Control::Call(idx) => {
                        fnctxs[ctx_idx] = FnCtx {
                            func_id,
                            bb_idx,
                            inst_offset,
                            local_offset,
                            stack_offset,
                            old_local_offset,
                        };

                        ctx_idx += 1;
                        back = false;
                        func_id = idx;
                        continue 'start;
                    }
                }
            }

            //std::mem::swap(&mut self.ctx, &mut ctx);
            let ret = self.ctx.stack.pop().unwrap_or_else(|| Value::Unit);

            assert!(self.ctx.stack.raw.len() >= stack_offset);
            assert!(self.ctx.local_table.len() >= local_offset);
            self.ctx.stack.raw.truncate(stack_offset);
            self.ctx.local_table.truncate(local_offset);
            self.ctx.local_offset = old_local_offset;
            //ctx.stack.raw.clear();
            //ctx.local_table.clear();
            //self.ctx_pool.free(ctx);

            if ctx_idx > 0 {
                self.ctx.stack.push(ret);
                back = true;

                continue 'start;
            } else {
                break 'start Ok(ret);
            }
        }
    } */

    fn __call_func(&mut self, func_id: u32) -> Result<Value, Error> {
        let module = self.module.clone();
        let func = module
            .func_blocks
            .get(func_id as usize)
            .ok_or_else(|| Error::FuncNotExist(func_id as usize))?;
        //let mut ctx = self.ctx_pool.alloc();

        if self.ctx.stack.raw.len() < func.params {
            return Err(Error::FuncParamsNotMatch(
                func.params,
                self.ctx.stack.raw.len(),
            ));
        }

        let local_offset = self.ctx.local_table.len();
        let old_local_offset = self.ctx.local_offset;
        self.ctx.local_offset = local_offset;

        self.ctx.local_table.extend(
            self.ctx
                .stack
                .raw
                .drain(self.ctx.stack.raw.len() - func.params..),
        );

        // enter function context
        //std::mem::swap(&mut self.ctx, &mut ctx);
        let stack_offset = self.ctx.stack.raw.len();

        let entry = func.blocks.first().unwrap();
        let mut current_bb = entry.insts.0.iter();

        loop {
            let Some(inst) = current_bb.next() else {
                break;
            };
            match self.instruction(*inst)? {
                Control::Continue => continue,
                Control::Br(idx) => {
                    current_bb = func.blocks.get(idx as usize).unwrap().insts.0.iter();
                    continue;
                }
                Control::Return => {
                    break;
                }
            }
        }

        //std::mem::swap(&mut self.ctx, &mut ctx);
        let ret = self.ctx.stack.pop().unwrap_or_else(|| Value::Unit);

        assert!(self.ctx.stack.raw.len() >= stack_offset);
        assert!(self.ctx.local_table.len() >= local_offset);
        self.ctx.stack.raw.truncate(stack_offset);
        self.ctx.local_table.truncate(local_offset);
        self.ctx.local_offset = old_local_offset;
        //ctx.stack.raw.clear();
        //ctx.local_table.clear();
        //self.ctx_pool.free(ctx);

        dbg!(self.ctx.stack.raw.len() * size_of::<Value>());

        Ok(ret)
    }

    fn instruction(&mut self, instruction: Instruction) -> Result<Control, Error> {
        match instruction {
            Instruction::Consti64(int) => {
                self.ctx.stack.push(Value::Consti64(int));
            }
            Instruction::LoadLocal(id) => {
                self.ctx.load_lcoal(id as usize)?;
            }
            Instruction::StoreLocal(id) => {
                let value = self.ctx.stack.assert_pop();
                if let Some(v) = self.ctx.local_table.get_mut(id as usize) {
                    *v = value
                } else if self.ctx.local_table.len() == id as usize {
                    self.ctx.local_table.push(value);
                } else {
                    return Err(Error::LocalNotExist(id as usize));
                }
            }
            Instruction::FuncCall(id) => {
                let ret = self.__call_func(id)?;
                self.ctx.stack.push(ret);
                //return Ok(Control::Call(id));
            }
            Instruction::Ret => {
                return Ok(Control::Return);
            }
            Instruction::Add => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(v[0] + v[1]);
            }
            Instruction::Sub => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(v[0] - v[1]);
            }
            Instruction::Eq => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(Value::from_bool(v[0] == v[1]));
            }
            Instruction::Gt => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(Value::from_bool(v[0] > v[1]));
            }
            Instruction::Ge => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(Value::from_bool(v[0] >= v[1]));
            }
            Instruction::Lt => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(Value::from_bool(v[0] < v[1]));
            }
            Instruction::Le => {
                let v = self.ctx.stack.pop_n::<2>();
                self.ctx.stack.push(Value::from_bool(v[0] <= v[1]));
            }
            Instruction::Mul => todo!(),
            Instruction::Div => todo!(),
            Instruction::Ne => todo!(),
            Instruction::Br(idx) => return Ok(Control::Br(idx)),
            Instruction::BrIf(idx, el) => {
                return Ok(if self.ctx.stack.pop().expect("cond not found").to_bool() {
                    Control::Br(idx)
                } else {
                    Control::Br(el)
                })
            }
        }

        Ok(Control::Continue)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Control {
    Continue,
    Return,
    Br(u32),
    //Call(u32),
}

impl Control {
    pub fn is_continue(&self) -> bool {
        matches!(self, Control::Continue)
    }

    pub fn is_return(&self) -> bool {
        matches!(self, Control::Return)
    }
}
