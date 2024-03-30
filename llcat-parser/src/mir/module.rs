use ahash::AHashMap;
use smol_str::SmolStr;

use crate::small_vec::SmallVec6;

use super::basic_block::BasicBlock;


#[derive(Debug)]
pub struct Module {
    pub blocks: AHashMap<SmolStr, FuncBlock>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            blocks: AHashMap::new()
        }
    }
}

#[derive(Debug)]
pub struct FuncBlock {
    pub locals: SmallVec6<SmolStr>,
    pub blocks: SmallVec6<BasicBlock>,
}

impl FuncBlock {
    pub fn new() -> FuncBlock {
        FuncBlock {
            locals: SmallVec6::new(),
            blocks: SmallVec6::new(),
        }
    }
}