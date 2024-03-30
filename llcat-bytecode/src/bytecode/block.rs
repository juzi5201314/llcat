use petgraph::Graph;
use smol_str::SmolStr;

use super::instruction::Instruction;

#[derive(Clone, Debug)]
pub struct InstructionBlock(pub Vec<Instruction>);

#[derive(Clone, Debug)]
pub struct FuncBlock {
    pub params: usize,
    pub blocks: Vec<BasicBlock>,
    pub graph: Graph<SmolStr, ()>,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub insts: InstructionBlock,
    pub label: Option<SmolStr>,
    pub idx: u32,
    terminated: bool,
}

impl FuncBlock {
    pub fn new() -> Self {
        FuncBlock {
            params: 0,
            blocks: Vec::new(),
            graph: Graph::new(),
        }
    }

    pub fn add_block(&mut self, mut block: BasicBlock) -> u32 {
        let id = self.blocks.len() as u32;
        block.idx = id;
        self.blocks.push(block);
        id
    }
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            insts: InstructionBlock(Vec::new()),
            label: None,
            idx: 0,
            terminated: false,
        }
    }

    pub fn with_label(label: impl Into<SmolStr>) -> BasicBlock {
        BasicBlock {
            insts: InstructionBlock(Vec::new()),
            label: Some(label.into()),
            idx: 0,
            terminated: false,
        }
    }

    pub fn instruction(&mut self, instruction: Instruction) {
        if !self.terminated {
            match instruction {
                Instruction::Br(_) | Instruction::BrIf(_, _) | Instruction::Ret => {
                    self.terminated = true;
                }
                _ => {}
            }
            self.insts.0.push(instruction);
        }
    }

    pub fn name(&self) -> SmolStr {
        if let Some(label) = &self.label {
            smol_str::format_smolstr!("{} #bb{}", label, self.idx)
        } else {
            smol_str::format_smolstr!("#bb{}", self.idx)
        }
    }
}