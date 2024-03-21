use smallvec::{Array, SmallVec};

macro_rules! _def {
    ($($num:literal),*) => {
        $(paste::paste!{
            pub type [<SmallVec $num>]<T> = SmallVec<[T; $num]>;
        })*
    };
}

_def!(1, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 32, 48, 64, 96, 128, 256, 512, 1024);

pub struct ContainerWrapper<A: Array>(pub SmallVec<A>);

impl<A> chumsky::container::Container<A::Item> for ContainerWrapper<A> where A: Array {
    fn push(&mut self, item: A::Item) {
        self.0.push(item)
    }
    
    fn with_capacity(n: usize) -> Self {
        ContainerWrapper(SmallVec::with_capacity(n))
    }
}

impl<A> Default for ContainerWrapper<A> where A: Array {
    fn default() -> Self {
        ContainerWrapper(SmallVec::new())
    }
}