use std::{ops::Range, rc::Rc};

use hashbrown::HashMap;
use rust_decimal::Decimal;

use crate::parser::{Atom, Span};

use super::Instruction;

#[derive(Debug, Clone)]
pub struct Module {
    pub funcs: HashMap<String, Rc<Function>>,
    pub constants: Vec<Constant>,
}

impl Module {
}

#[derive(Debug, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub spans: Vec<Range<u32>>,
    pub arity: usize,
}

impl Function {
    pub fn new(arity: usize) -> Self {
        Function {
            instructions: Vec::new(),
            arity,
            spans: Vec::new(),
        }
    }
    pub fn add_inst(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }
}


#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Constant {
    Integer(i64),
    Float(Decimal),
    String(Atom),
    Boolean(bool),
    Nil,
}