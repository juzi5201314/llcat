use std::ops::Index;

use super::Value;

const NIL: Value = Value::Nil;

pub struct Stack {
    pub data: Vec<Value>,
    pub len: usize,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            data: Vec::new(),
            len: 0,
        }
    }

    #[inline]
    pub fn push(&mut self, value: Value) {
        if self.data.len() <= self.len {
            self.grow();
        }

        //self.data[self.len] = value;
        unsafe {
            let end = self.data.as_mut_ptr().add(self.len);
            std::ptr::write(end, value);
        }
        self.len += 1;
    }

    pub fn grow(&mut self) {
        let new_len = self.data.len() + 32;
        self.data.resize(new_len, NIL);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<Value> {
        self.len.checked_sub(1).and_then(|len| {
            self.len = len;
            let val = std::mem::replace(&mut self.data[len], NIL);
            Some(val)
        })
    }

    pub fn pop_ref(&mut self) -> Option<&Value> {
        self.len.checked_sub(1).and_then(|len| {
            self.len = len;
            self.data.get(len)
        })
    }

    #[inline]
    pub fn peek(&self) -> Option<&Value> {
        self.len.checked_sub(1).and_then(|i| self.data.get(i))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn truncate_len(&mut self, len: usize) -> &[Value] {
        if len >= self.len {
            return &[];
        }
        let truncated = &self.data[len..self.len];
        self.len = len;
        truncated
    }
}

impl Index<usize> for Stack {
    type Output = Value;

    fn index(&self, index: usize) -> &Value {
        &self.data[index]
    }
}

#[cfg(test)]
mod tests {
    use super::Stack;
    use super::Value;

    #[test]
    fn stack_new_is_empty() {
        let stack = Stack::new();
        assert_eq!(stack.len(), 0);
        assert_eq!(stack.peek(), None);
    }

    #[test]
    fn stack_push_increases_length() {
        let mut stack = Stack::new();
        stack.push(Value::Integer(1));
        assert_eq!(stack.len(), 1);
        assert_eq!(stack.peek(), Some(&Value::Integer(1)));
    }

    #[test]
    fn stack_pop_decreases_length() {
        let mut stack = Stack::new();
        stack.push(Value::Integer(1));
        assert_eq!(stack.pop(), Some(Value::Integer(1)));
        assert_eq!(stack.len(), 0);
        assert_eq!(stack.peek(), None);
    }

    #[test]
    fn stack_pop_on_empty_returns_none() {
        let mut stack = Stack::new();
        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn stack_peek_returns_top_element() {
        let mut stack = Stack::new();
        stack.push(Value::Integer(1));
        stack.push(Value::Integer(2));
        assert_eq!(stack.peek(), Some(&Value::Integer(2)));
    }

    #[test]
    fn stack_peek_on_empty_returns_none() {
        let stack = Stack::new();
        assert_eq!(stack.peek(), None);
    }

    #[test]
    fn stack_grow_expands_capacity() {
        let mut stack = Stack::new();
        for i in 0..33 {
            stack.push(Value::Integer(i));
        }
        assert_eq!(stack.len(), 33);
        assert_eq!(stack.peek(), Some(&Value::Integer(32)));
    }
}
