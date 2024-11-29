use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

use hashbrown::HashSet;

use crate::arena::BumpArena;

const INLINE_CAP: usize = 23;
const _: () = assert!(std::mem::size_of::<Atom2>() == INLINE_CAP + 1);

#[derive(Clone)]
pub enum Atom2 {
    Inline {
        len: InlineSize,
        buf: [u8; INLINE_CAP],
    },
    Heap(HeapAtom),
}

#[derive(Clone)]
pub struct HeapAtom {
    pub s: *const Box<str, BumpArena>,
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
        if s.len() <= INLINE_CAP {
            return Atom2::new_inline(s);
        }

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
        /* let static_str =
        unsafe { std::str::from_raw_parts::<'static>(boxed_str.as_ptr(), boxed_str.len()) }; */

        Atom2::Heap(HeapAtom {
            s: boxed_str as *const _,
            _pool_ref: Rc::clone(&self.0),
        })
    }
}

impl Default for InternPool {
    fn default() -> Self {
        InternPool::with_capacity(4096, 64)
    }
}

impl Deref for Atom2 {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Atom2::Inline { len, buf } => unsafe {
                std::str::from_utf8_unchecked(&buf[..*len as usize])
            },
            Atom2::Heap(heap_atom) => unsafe { &*heap_atom.s },
        }
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
        self.as_ref() == other.as_ref()
    }
}
impl Eq for Atom2 {}

impl Debug for Atom2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Debug>::fmt(&self.as_ref(), f)
    }
}

impl Display for Atom2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Display>::fmt(&self.as_ref(), f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum InlineSize {
    _00 = 0,
    _01,
    _02,
    _03,
    _04,
    _05,
    _06,
    _07,
    _08,
    _09,
    _10,
    _11,
    _12,
    _13,
    _14,
    _15,
    _16,
    _17,
    _18,
    _19,
    _20,
    _21,
    _22,
    _23,
}

impl InlineSize {
    /// SAFETY: `value` must be less than or equal to [`INLINE_CAP`]
    #[inline(always)]
    const unsafe fn transmute_from_u8(value: u8) -> Self {
        debug_assert!(value <= InlineSize::_23 as u8);
        // SAFETY: The caller is responsible to uphold this invariant
        unsafe { std::mem::transmute::<u8, Self>(value) }
    }
}

impl Atom2 {
    pub fn new_inline(s: &str) -> Self {
        assert!(s.len() <= INLINE_CAP);
        let mut buf = [0; INLINE_CAP];
        buf[..s.len()].copy_from_slice(s.as_bytes());
        Atom2::Inline {
            len: unsafe { InlineSize::transmute_from_u8(s.len() as u8) },
            buf,
        }
    }

    pub fn is_inline(&self) -> bool {
        matches!(self, Atom2::Inline { .. })
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
        assert!(a2.is_inline());

        assert!(!pool.alloc(&"0".repeat(24)).is_inline());
        assert!(pool.0.borrow().map.len() == 1);
    }
}
