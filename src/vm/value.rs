use std::{rc::Rc, str::FromStr};

use hashbrown::HashMap;
use rust_decimal::Decimal;

use crate::{bytecode::Instruction, vm::Vm};

use super::RuntimeError;

macro_rules! binary_op {
    ($l:expr, $op:tt, $r:expr, $vm:expr, { $($p:tt)* }) => {
        match ($l, $r) {
            (Value::Integer(a), Value::Integer(b)) => {
                $vm.stack.push(Value::Integer(a $op b));
            }
            (Value::Float(f), other) | (other, Value::Float(f)) => {
                let Value::Float(other) = other.cast_to_float() else {
                    unreachable!("expected number")
                };
                $vm.stack.push(Value::Float(f $op other));
            }
            $($p)*
        }
    };
    (@bool $l:expr, $op:tt, $r:expr, $vm:expr, { $($p:tt)* }) => {
        match ($l, $r) {
            (Value::Integer(a), Value::Integer(b)) => {
                $vm.stack.push(Value::Boolean(a $op b));
            }
            (Value::Float(f), other) | (other, Value::Float(f)) => {
                let Value::Float(ref other) = other.cast_to_float() else {
                    unreachable!("expected number")
                };
                $vm.stack.push(Value::Boolean(f $op other));
            }
            $($p)*
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(Decimal),
    Boolean(bool),
    Byte(u8),
    String(String),                 // Heap allocated
    Array(Vec<Value>),              // Heap allocated
    Object(HashMap<String, Value>), // Heap allocated
    Function(Function),
    Nil,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Rc<String>,
    pub bytecode: Rc<crate::bytecode::Function>,
    pub dyn_locals: HashMap<String, Value>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Float(f) => !f.is_zero(),
            Value::String(s) => !s.is_empty(),
            Value::Array(_) => true,  // Non-empty array is true
            Value::Object(_) => true, // Non-empty object is true
            Value::Nil => false,
            Value::Byte(b) => *b != 0,
            Value::Function(_) => true,
        }
    }

    pub fn __add(&self, other: &Value, vm: &mut Vm) -> Result<(), RuntimeError> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => {
                vm.stack.push(Value::Integer(a + b));
            }
            /* (Value::Float(a), Value::Float(b)) => {
                vm.stack.push(Value::Float(a + b));
            } */
            (Value::Float(f), other) | (other, Value::Float(f)) => {
                let Value::Float(other) = other.cast_to_float() else {
                    unreachable!("expected number")
                };
                vm.stack.push(Value::Float(f + other));
            }
            (Value::String(a), Value::String(b)) => {
                vm.stack.push(Value::String(format!("{}{}", a, b)));
            }
            _ => return Err(RuntimeError::msg("`+` only supports numbers or strings")),
        }
        Ok(())
    }

    pub fn __sub(&self, other: &Value, vm: &mut Vm) -> Result<(), RuntimeError> {
        binary_op!(self, -, other, vm, {
            _ => return Err(RuntimeError::msg("`-` only supports numbers or strings"))
        });
        Ok(())
    }

    pub fn __eq(&self, other: &Value, vm: &mut Vm) -> Result<(), RuntimeError> {
        binary_op!(@bool self, ==, other, vm, {
            (Value::String(l), Value::String(r)) => {
                vm.stack.push(Value::Boolean(l == r));
            }
            (l, r) => {
                vm.stack.push(Value::Boolean(l == r));
            }
        });
        Ok(())
    }

    pub fn __le(&self, other: &Value, vm: &mut Vm) -> Result<(), RuntimeError> {
        binary_op!(@bool self, <=, other, vm, {
            _ => return Err(RuntimeError::msg("`<=` only supports numbers"))
        });
        Ok(())
    }

    pub fn __ge(&self, other: &Value, vm: &mut Vm) -> Result<(), RuntimeError> {
        binary_op!(@bool self, >=, other, vm, {
            _ => return Err(RuntimeError::msg("`<=` only supports numbers"))
        });
        Ok(())
    }

    fn cast_to_float(&self) -> Value {
        match self {
            Value::Integer(i) => Value::Float(Decimal::from(*i)),
            Value::Float(f) => Value::Float(*f),
            Value::Boolean(b) => Value::Float(if *b {
                Decimal::from(1)
            } else {
                Decimal::from(0)
            }),
            Value::String(s) => {
                let f = if s.contains('e') || s.contains('E') {
                    Decimal::from_scientific(s).unwrap()
                } else {
                    Decimal::from_str(s).unwrap()
                };
                Value::Float(f)
            }
            _ => panic!("Cannot cast to float"),
        }
    }

    fn is_numeric(&self) -> bool {
        match self {
            Value::Integer(_) | Value::Float(_) => true,
            _ => false,
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<Decimal> for Value {
    fn from(value: Decimal) -> Self {
        Value::Float(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Byte(value)
    }
}

pub trait IntoValues {
    type IntoIter: Iterator<Item = Value>;
    fn into_iter(self) -> Self::IntoIter;
}

impl<const N: usize> IntoValues for [Value; N] {
    type IntoIter = <[Value; N] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIterator::into_iter(self)
    }
}
