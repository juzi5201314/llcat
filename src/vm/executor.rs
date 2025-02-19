use std::rc::Rc;

use hashbrown::HashMap;

use crate::bytecode::{Constant, Instruction, Module};

use crate::vm::{Function, Value};

use super::{IntoValues, RuntimeError, Stack};

pub struct Vm {
    module: Module,

    //pub stack: Vec<Value>,
    pub stack: Stack,
    globals: HashMap<String, Value>,
    ip: usize, // Instruction pointer
    call_frames: Vec<CallFrame>,
}

static mut COUNTER1: usize = 0;
static mut COUNTER2: usize = 0;

pub struct CallFrame {
    function: Function,
    ret_ip: usize,     // Instruction pointer for when we return.
    stack_base: usize, // The index in the stack that is the base of the call frame.
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Vm {
            module,
            stack: Stack::new(),
            globals: HashMap::new(),
            ip: 0,
            call_frames: Vec::with_capacity(32),
        }
    }

    pub fn call_func(&mut self, name: &str, args: impl IntoValues) -> Result<Value, RuntimeError> {
        let func = self.module.funcs.get(name).unwrap().clone();
        let arity = func.arity;
        let stack_len = self.stack.len();

        for arg in args.into_iter() {
            self.stack.push(arg.into());
        }
        debug_assert!(
            arity == self.stack.len() - stack_len,
            "parameters count mismatch"
        );

        self.call_frames.push(CallFrame {
            function: Function {
                name: Rc::new(name.to_owned()),
                bytecode: func,
                dyn_locals: HashMap::new(),
            },
            ret_ip: self.ip,
            stack_base: self.stack.len() - arity,
        });
        self.run()?;
        unsafe {
            dbg!(COUNTER1);
            dbg!(COUNTER2);
        }
        Ok(self.stack.pop().unwrap_or_else(|| Value::Nil))
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        let mut rt_err: Option<super::RuntimeError> = None;
        'run: loop {
            'frame: loop {
                //let _span = tracing::debug_span!("call frame loop").entered();
                let call_frame = self.call_frames.last_mut().expect("empty call stack");

                // handle last runtime error
                if let Some(err) = &mut rt_err {
                    err.stack_trace.push(call_frame.function.name.to_string());
                    // 跳出当前栈帧, 交由上一层栈帧处理
                    break 'frame;
                }

                if self.ip >= call_frame.function.bytecode.instructions.len() {
                    break 'frame;
                }
                let instruction = &call_frame.function.bytecode.instructions[self.ip];

                // execute instruction
                let rt_res = match instruction {
                    Instruction::Const(idx) => {
                        let val = match self.module.constants[*idx] {
                            Constant::Integer(i) => Value::Integer(i),
                            Constant::Float(decimal) => todo!(),
                            Constant::String(atom) => todo!(),
                            Constant::Boolean(_) => todo!(),
                            Constant::Nil => todo!(),
                        };
                        self.stack.push(val);
                        Ok(())
                    }
                    Instruction::Pop => todo!(),
                    Instruction::Dup => todo!(),
                    Instruction::GetGlobal(_) => todo!(),
                    Instruction::SetGlobal(_) => todo!(),
                    Instruction::GetLocal(offset) => {
                        let offset = call_frame.stack_base + *offset;
                        self.stack.push(self.stack[offset].clone());
                        Ok(())
                    }
                    Instruction::SetLocal(var) => {
                        /* call_frame.function.locals.insert(
                            var.clone(),
                            self.stack.pop().expect("expect 1 value, but got none"),
                        ); */
                        todo!();
                        Ok(())
                    }
                    Instruction::GetVariable(var) => {
                        if let Some(val) = call_frame
                            .function
                            .dyn_locals
                            .get(var)
                            .or_else(|| self.globals.get(var))
                        {
                            self.stack.push(val.clone());
                        } else {
                            panic!("runtime error: variable not found")
                        }
                        Ok(())
                    }
                    Instruction::Add => {
                        //let _span = tracing::debug_span!("add inst").entered();
                        let rhs = self.stack_pop();
                        let lhs = self.stack_pop();
                        lhs.__add(&rhs, self)
                    }
                    Instruction::Sub => {
                        let rhs = self.stack_pop();
                        let lhs = self.stack_pop();
                        lhs.__sub(&rhs, self)
                    }
                    Instruction::Mul => todo!(),
                    Instruction::Div => todo!(),
                    Instruction::Mod => todo!(),
                    Instruction::Eq => todo!(),
                    Instruction::NotEq => todo!(),
                    Instruction::Gt => todo!(),
                    Instruction::Lt => todo!(),
                    Instruction::Ge => todo!(),
                    Instruction::Le => {
                        let rhs = self.stack_pop();
                        let lhs = self.stack_pop();
                        lhs.__le(&rhs, self)
                    }
                    Instruction::And => todo!(),
                    Instruction::Or => todo!(),
                    Instruction::Not => todo!(),
                    Instruction::Neg => todo!(),
                    Instruction::StringConcat => todo!(),
                    Instruction::ArrayNew => todo!(),
                    Instruction::ArrayGet => todo!(),
                    Instruction::ArraySet => todo!(),
                    Instruction::ObjectNew => todo!(),
                    Instruction::ObjectGet(_) => todo!(),
                    Instruction::ObjectSet(_) => todo!(),
                    Instruction::Jump(_) => todo!(),
                    Instruction::JumpIfFalse(point) => {
                        let target = *point;
                        let cond = self.stack_pop();
                        if !cond.is_truthy() {
                            self.ip = target;
                            // skip ip inc
                            continue 'frame;
                        }
                        Ok(())
                    }
                    Instruction::Call(fn_name, arity) => {
                        unsafe {
                            COUNTER1 += 1;
                        }
                        //let _span = tracing::debug_span!("call inst").entered();
                        let func = if &call_frame.function.name == fn_name {
                            if self.ip == call_frame.function.bytecode.instructions.len() - 1 {
                                // 尾递归?
                                self.ip = 0;
                                continue 'frame;
                            }
                            call_frame.function.bytecode.clone()
                        } else {
                            self.module
                                .funcs
                                .get(&**fn_name)
                                .ok_or_else(|| RuntimeError::msg("func not found"))?
                                .clone()
                        };

                        let name = fn_name.clone();
                        let arity = *arity;
                        self.call_frames.push(CallFrame {
                            function: Function {
                                name,
                                bytecode: func,
                                dyn_locals: HashMap::new(),
                            },
                            ret_ip: self.ip,
                            stack_base: self.stack.len() - arity,
                        });
                        self.ip = 0;
                        continue 'frame;
                    }
                    Instruction::Return => {
                        break 'frame;
                    }
                    Instruction::DebugPrint => todo!(),
                };

                if let Err(err) = rt_res {
                    rt_err = Some(err)
                }
                self.ip += 1;
            }

            //let _span = tracing::debug_span!("call frame exit").entered();

            // call frame end
            unsafe {
                COUNTER2 = COUNTER2.max(self.call_frames.len());
            }
            let old_call_frame = self.call_frames.pop().unwrap();
            self.ip = old_call_frame.ret_ip + 1;

            let truncated = self.stack.truncate_len(old_call_frame.stack_base);
            let a = truncated.len();
            if rt_err.is_none() {
                if let Some(_ret) = truncated.last() {
                    //self.stack.push(_ret);
                    if self.stack.data.len() <= self.stack.len {
                        self.stack.grow();
                    }
                    self.stack.data.swap(self.stack.len, self.stack.len + a - 1);
                    self.stack.len += 1;
                } else {
                    self.stack.push(Value::Nil);
                }
            }

            if self.call_frames.is_empty() {
                break 'run rt_err.map(Err).unwrap_or(Ok(()));
            }
        }
    }

    #[inline(always)]
    fn stack_pop(&mut self) -> Value {
        self.stack
            .pop()
            .ok_or_else(|| format!("{}", self.ip))
            .expect("stack underflow")
    }
}

#[cfg(test)]
mod tests {
    use crate::{args, bytecode::Compiler, vm::Value};

    use super::Vm;

    #[test]
    fn sum() {
        let mut compiler = Compiler::new();
        let module = compiler.compile_module("fn sum(a, b) = a + b;");
        let mut vm = Vm::new(module);

        let ret = vm.call_func("sum", args![2i64, 2i64]);
        assert!(matches!(ret, Ok(Value::Integer(4))))
    }

    #[test]
    fn fib() {
        use tracing_chrome::ChromeLayerBuilder;
        use tracing_subscriber::prelude::*;

        //let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
        //tracing_subscriber::registry().with(chrome_layer).init();

        let mut compiler = Compiler::new();
        let module =
            compiler.compile_module("fn fib(n) = { if n <= 1 { ret n }; fib(n - 1) + fib(n - 2) }");
        let mut vm = Vm::new(module);

        /* let guard = pprof::ProfilerGuardBuilder::default()
        .frequency(2000)
        .blocklist(&["libc", "test"])
        .build()
        .unwrap(); */

        let ret = vm.call_func("fib", args![40i64]);
        dbg!(ret);
        /* if let Ok(report) = guard.report().build() {
            let file = std::fs::File::create("flamegraph.svg").unwrap();
            report.flamegraph(file).unwrap();
        }; */
        //assert!(matches!(ret, Ok(Value::Integer(2))))
    }
}
