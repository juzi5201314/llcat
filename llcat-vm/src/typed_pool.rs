pub struct Pool<T> {
    constructor: Box<dyn Fn() -> T>,
    pub free: Vec<T>,
}

impl<T> Pool<T> {
    pub fn with_capacity<F>(n: usize, constructor: F) -> Self
    where
        F: Fn() -> T + 'static,
    {
        Self {
            free: Vec::from_iter((0..n).map(|_| constructor())),
            constructor: Box::new(constructor),
        }
    }

    pub fn alloc(&mut self) -> T {
        if let Some(t) = self.free.pop() {
            t
        } else {
            (self.constructor)()
        }
    }

    pub fn free(&mut self, t: T) {
        self.free.push(t);
    }
}
