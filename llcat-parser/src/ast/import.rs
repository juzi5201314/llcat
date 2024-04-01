use smol_str::SmolStr;

use crate::small_vec::SmallVec3;

#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    Direct(Path),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub path: SmallVec3<SmolStr>,
}