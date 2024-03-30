use smol_str::SmolStr;

use crate::small_vec::SmallVec3;

use super::statement::Statement;

#[derive(Debug)]
pub struct BasicBlock {
    pub label: Option<SmolStr>,
    pub stmts: SmallVec3<Statement>,
}

impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            label: None,
            stmts: SmallVec3::new(),
        }
    }

    pub fn from_label(label: SmolStr) -> Self {
        BasicBlock {
            label: Some(label),
            stmts: SmallVec3::new(),
        }
    }
}