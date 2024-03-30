use std::mem::size_of;
use std::ops::{Add, Sub};


#[derive(Clone, Copy, Debug)]
pub enum Value {
    Unit,
    Consti64(i64),
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Unit => false,
            Value::Consti64(i) => *i == 1,
        }
    }

    pub fn from_bool(b: bool) -> Value {
        Value::Consti64(b as i64)
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Consti64(l), Value::Consti64(r)) => Value::Consti64(l + r),
            (Value::Unit, val) | (val, Value::Unit) => val,
        }
    }
}

impl Sub for Value {
    type Output = Value;
    
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Consti64(l), Value::Consti64(r)) => Value::Consti64(l - r),
            (Value::Unit, val) | (val, Value::Unit) => val,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Consti64(l), Value::Consti64(r)) => l == r,
            (Value::Unit, Value::Unit) => todo!(),
            (Value::Unit, Value::Consti64(r)) => todo!("{}", r),
            (Value::Consti64(_), Value::Unit) => todo!(),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Consti64(l), Value::Consti64(r)) => l.partial_cmp(r),
            (Value::Unit, Value::Unit) => todo!(),
            (Value::Unit, Value::Consti64(_)) => todo!(),
            (Value::Consti64(_), Value::Unit) => todo!(),
        }
    }
}

const _: () = {
    assert!(size_of::<Value>() == 16)
};