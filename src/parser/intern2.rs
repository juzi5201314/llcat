use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

use hashbrown::HashSet;

use crate::arena::BumpArena;

const _: () = assert!(std::mem::size_of::<Atom2>() == 24);

#[derive(Clone)]
pub struct Atom2 {
    pub s: &'static str,
    // sacrifice clone performance in exchange for faster alloc speed and coding convenience
    _pool_ref: Rc<RefCell<InternPoolInner>>,
}

pub struct InternPool(Rc<RefCell<InternPoolInner>>);

pub struct InternPoolInner {
    arena: BumpArena,
    map: HashSet<Box<str, BumpArena>, hashbrown::DefaultHashBuilder, BumpArena>,
}

impl InternPool {
    pub fn with_capacity(bytes: usize, strings: usize) -> Self {
        let arena = BumpArena::with_capacity(bytes);
        InternPool(Rc::new(RefCell::new(InternPoolInner {
            map: HashSet::<_, _, BumpArena>::with_capacity_in(strings, arena.clone()),
            arena,
        })))
    }

    pub fn alloc(&self, s: &str) -> Atom2 {
        let mut pool = self.0.borrow_mut();
        let arena = pool.arena.clone();

        let boxed_str = pool.map.get_or_insert_with(s, |s| {
            // `s.into_boxed_str`
            let mut v = Vec::with_capacity_in(s.len(), arena.clone());
            unsafe { v.set_len(s.len()) };
            v.copy_from_slice(s.as_bytes());

            let box_str = unsafe {
                Box::<_, BumpArena>::from_raw_in(
                    Box::<_, BumpArena>::into_raw(v.into_boxed_slice()) as *mut str,
                    arena,
                )
            };
            box_str
        });

        // Safely: reference counting ensures that static_str will not survive longer than arena
        let static_str =
            unsafe { std::str::from_raw_parts::<'static>(boxed_str.as_ptr(), boxed_str.len()) };

        Atom2 {
            s: static_str,
            _pool_ref: Rc::clone(&self.0),
        }
    }
}

impl Default for InternPool {
    fn default() -> Self {
        InternPool::with_capacity(4096, 128)
    }
}

impl Deref for Atom2 {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.s
    }
}

impl AsRef<str> for Atom2 {
    fn as_ref(&self) -> &str {
        self
    }
}

impl<T> PartialEq<T> for Atom2
where
    T: AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
        self.s == other.as_ref()
    }
}
impl Eq for Atom2 {}

impl Debug for Atom2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Debug>::fmt(&self.s, f)
    }
}

impl Display for Atom2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Display>::fmt(&self.s, f)
    }
}

#[cfg(test)]
mod tests {
    use super::InternPool;

    #[test]
    fn intern_str() {
        let pool = InternPool::default();
        let a1 = pool.alloc("foo");
        let a2 = pool.alloc("foo");

        assert_eq!(&*a1, "foo");
        assert_eq!(&a1, &a2);
        assert!(pool.0.borrow().map.len() == 1);
    }
}
