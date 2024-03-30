use std::rc::Rc;

use llcat_bytecode::bytecode::{Instruction, Module};

use crate::context::RuntimeContext;
use crate::error::Error;
use crate::typed_pool::Pool;
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

        self.ctx.stack.raw.truncate(stack_offset);
        self.ctx.local_table.truncate(local_offset);
        self.ctx.local_offset = old_local_offset;
        //ctx.stack.raw.clear();
        //ctx.local_table.clear();
        //self.ctx_pool.free(ctx);

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
}

impl Control {
    pub fn is_continue(&self) -> bool {
        matches!(self, Control::Continue)
    }

    pub fn is_return(&self) -> bool {
        matches!(self, Control::Return)
    }
}
