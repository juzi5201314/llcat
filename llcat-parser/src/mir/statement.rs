use super::Local;


#[derive(Debug)]
pub struct Statement {
    kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Local(Local),
    Const(Const),
}

impl Statement {
    pub fn new(kind: StatementKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
pub enum Const {
    Int(i64),
}
