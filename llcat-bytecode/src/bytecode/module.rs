use super::block::FuncBlock;

#[derive(Clone, Debug)]
pub struct Module {
    pub func_blocks: Vec<FuncBlock>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            func_blocks: Vec::new(),
        }
    }

    pub fn add_func(&mut self, func_block: FuncBlock) -> u32 {
        self.func_blocks.push(func_block);
        (self.func_blocks.len() - 1) as u32
    }
}