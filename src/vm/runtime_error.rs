
#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stack_trace: Vec<String>,
}

impl RuntimeError {
    pub fn msg(s: &str) -> Self {
        RuntimeError {
            message: s.to_owned(),
            stack_trace: Vec::new(),
        }
    }
}