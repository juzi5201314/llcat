use hashbrown::HashMap;
use pretty::RcDoc;

use crate::parser::{Atom, ToPrettyDoc};

crate::pub_use_mod!(instruction, compiler);

#[derive(Clone)]
pub struct Module {
    functions: HashMap<Atom, Function>,
}

#[derive(Clone)]
pub struct Function {
    instructions: Vec<Instruction>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            instructions: Vec::new(),
        }
    }

    pub fn add_inst(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

impl ToPrettyDoc for Module {
    fn to_doc(&self) -> pretty::RcDoc<()> {
        RcDoc::intersperse(
            self.functions.iter().map(|(name, func)| {
                RcDoc::text("fn ")
                    .append(&**name)
                    .append(":")
                    .append(RcDoc::line())
                    .append(func.to_doc())
                    .nest(2)
            }),
            RcDoc::line().append(RcDoc::line()),
        )
    }
}

impl ToPrettyDoc for Function {
    fn to_doc(&self) -> pretty::RcDoc<()> {
        RcDoc::intersperse(
            self.instructions.iter().map(|inst| RcDoc::as_string(inst)),
            RcDoc::line(),
        )
    }
}
