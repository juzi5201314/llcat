use smol_str::SmolStr;

use crate::small_vec::SmallVec6;

use super::Block;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    /// fn ident(param0, param1, ..) <retrun_ty> { block }
    Fn {
        name: SmolStr,
        params: SmallVec6<SmolStr>,
        body: Block,
        retrun_ty: Option<SmolStr>,
    },
}
