
pub enum Value {
    Nil,
    Int(u64),
    Float(f64),
    String(String),
    Bool,
    Array(Vec<Value>),
    Struct(Vec<Value>),
}

