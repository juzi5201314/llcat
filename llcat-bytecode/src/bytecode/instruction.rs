

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instruction {
    Consti64(i64),

    /// Load values from local variable onto the stack
    LoadLocal(u32),
    /// 
    StoreLocal(u32),

    FuncCall(u32),

    Ret,
    Br(u32),
    BrIf(u32, u32),

    Add,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    Ne,
    Sub,
    Mul,
    Div,
}
