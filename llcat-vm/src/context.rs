use crate::error::Error;
use crate::stack::Stack;
use crate::value::Value;

pub struct RuntimeContext {
    pub stack: Stack,
    pub local_table: Vec<Value>,

    /// Access the offset of local variables 
    /// so that the function context can correctly find the location of the function's local variable table during recursion
    pub local_offset: usize,
}

impl RuntimeContext {
    pub fn new() -> RuntimeContext {
        RuntimeContext {
            stack: Stack::new(),
            local_table: Vec::with_capacity(8),
            local_offset: 0,
        }
    }

    pub fn load_lcoal(&mut self, id: usize) -> Result<(), Error> {
        self.stack.push(
            self.local_table
                .get(id + self.local_offset)
                .ok_or_else(|| Error::LocalNotExist(id))?
                .clone(),
        );
        Ok(())
    }
}
