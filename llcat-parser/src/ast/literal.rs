use smol_str::SmolStr;


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Interger(i64),
    Float(f64),
    Boolean(bool),
    String(SmolStr),
}