use crate::value::Value;

pub struct Stack {
    pub raw: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            raw: Vec::with_capacity(32),
        }
    }

    pub fn push(&mut self, value: Value) {
        self.raw.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.raw.pop()
    }

    pub fn assert_pop(&mut self) -> Value {
        self.pop().expect("pop value from stack but stack is empty")
    }

    pub fn pop_n<const N: usize>(&mut self) -> [Value; N] {
        let mut r = [Value::Unit; N];
        let mut idx = 0;
        self.raw.drain(self.raw.len() - N..).for_each(|v| {
            r[idx] = v;
            idx += 1;
        });
        r
    }
    
}