//! A thread-local, immutable, inline(23 byte), fast-allocated, fast-cloned string interning.
//!
//! * Allocate in a linear memory (arena)
//! * Not deallocate

use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

use hashbrown::{Equivalent, HashSet};
use once_cell::unsync::Lazy;

use crate::arena::BumpArena;

thread_local! {
    static POOL: RefCell<Lazy<ThreadLocalPool>> = RefCell::new(Lazy::new(|| ThreadLocalPool::default()));
}

const INLINE_CAP: usize = 23;
const _: () = assert!(std::mem::size_of::<Atom>() == INLINE_CAP + 1);

pub struct ThreadLocalPool {
    arena: BumpArena,
    hash_map: HashSet<HeapAtom, hashbrown::DefaultHashBuilder, BumpArena>,
    other_strings: Vec<String, BumpArena>,
}

impl ThreadLocalPool {
    fn alloc(&mut self, s: &str) -> Atom {
        let atom = self.hash_map.get_or_insert_with(s, |_| {
            let bytes = self.arena.0.alloc_slice_copy(s.as_bytes());
            let static_str = unsafe {
                let str = std::str::from_utf8_unchecked(bytes);
                &*(str as *const str)
            };
            HeapAtom { s: static_str }
        });
        Atom::Heap(atom as *const _)
    }

    fn alloc_owned(&mut self, s: String) -> Atom {
        let other_strings = &mut self.other_strings;

        if let Some(atom) = self.hash_map.get(s.as_str()) {
            return Atom::Heap(atom as *const _);
        };

        other_strings.push(s);
        let [.., last] = &**other_strings else {
            unreachable!()
        };
        let static_str = unsafe { &*(last.as_str() as *const str) };
        let atom = unsafe {
            self.hash_map
                .insert_unique_unchecked(HeapAtom { s: static_str })
        };
        Atom::Heap(atom as *const _)
    }

    pub fn with_capacity(capacity: usize, map_capacity: usize) -> Self {
        let arena = BumpArena::with_capacity(capacity);
        ThreadLocalPool {
            hash_map: HashSet::with_capacity_and_hasher_in(
                map_capacity,
                hashbrown::DefaultHashBuilder::default(),
                arena.clone(),
            ),
            other_strings: Vec::new_in(arena.clone()),
            arena,
        }
    }
}

impl Default for ThreadLocalPool {
    fn default() -> Self {
        ThreadLocalPool::with_capacity(4096, 64)
    }
}

#[derive(PartialEq, Eq)]
pub struct HeapAtom {
    s: &'static str,
    //hash: u64,
}

impl hashbrown::Equivalent<HeapAtom> for str {
    fn equivalent(&self, key: &HeapAtom) -> bool {
        self == key.s
    }
}

impl Hash for HeapAtom {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.s.hash(state);
    }
}

#[derive(Copy)]
pub enum Atom {
    Inline {
        len: InlineSize,
        buf: [u8; INLINE_CAP],
    },
    Static(&'static str),
    /// raw ptr is !Sync, so it is guaranteed to be on the same thread as the pool that allocated it
    Heap(*const HeapAtom),
}

impl Atom {
    pub fn new<'a>(s: impl Into<Cow<'a, str>>) -> Self {
        let s = s.into();
        Atom::try_inline(s.as_ref()).unwrap_or_else(|| {
            POOL.with_borrow_mut(|pool| match s {
                Cow::Borrowed(str) => pool.alloc(str),
                Cow::Owned(string) => pool.alloc_owned(string),
            })
        })
    }

    /// if the lifetime of Atom is longer than that of pool, then this is ub
    pub unsafe fn new_in<'a>(s: impl Into<Cow<'a, str>>, pool: &mut ThreadLocalPool) -> Self {
        let s = s.into();
        Atom::try_inline(s.as_ref()).unwrap_or_else(|| match s {
            Cow::Borrowed(str) => pool.alloc(str),
            Cow::Owned(string) => pool.alloc_owned(string),
        })
    }

    pub fn try_inline(s: &str) -> Option<Self> {
        (s.len() <= INLINE_CAP).then(|| Atom::new_inline(s))
    }

    pub fn new_inline(s: &str) -> Self {
        let mut buf = [0; INLINE_CAP];
        buf[..s.len()].copy_from_slice(s.as_bytes());
        Atom::Inline {
            len: unsafe { InlineSize::transmute_from_u8(s.len() as u8) },
            buf,
        }
    }

    pub fn new_static(s: &'static str) -> Self {
        Atom::Static(s)
    }

    pub fn is_inline(&self) -> bool {
        matches!(self, Atom::Inline { .. })
    }

    pub fn is_static(&self) -> bool {
        matches!(self, Atom::Static(..))
    }

    pub fn is_heap(&self) -> bool {
        matches!(self, Atom::Heap(..))
    }

    pub fn heap_count() -> usize {
        POOL.with_borrow(|pool| pool.hash_map.len())
    }
}

impl Clone for Atom {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl Deref for Atom {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Atom::Inline { len, buf } => unsafe {
                std::str::from_utf8_unchecked(&buf[..*len as usize])
            },
            Atom::Static(s) => s,
            Atom::Heap(a) => unsafe { (**a).s },
        }
    }
}

impl AsRef<str> for Atom {
    fn as_ref(&self) -> &str {
        self
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialEq<str> for Atom {
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<&str> for Atom {
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl Eq for Atom {}

impl Hash for Atom {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl Borrow<str> for Atom {
    fn borrow(&self) -> &str {
        self
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Debug>::fmt(&self.as_ref(), f)
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <&str as Display>::fmt(&self.as_ref(), f)
    }
}

impl From<&str> for Atom {
    fn from(value: &str) -> Self {
        Atom::new(value)
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Atom::new(value)
    }
}

impl Equivalent<String> for Atom {
    fn equivalent(&self, key: &String) -> bool {
        self.as_ref() == key
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

#[cfg(test)]
mod tests {
    use super::Atom;

    #[test]
    fn intern_str() {
        let a1 = Atom::new("foo");
        let a2 = Atom::new("foo");

        assert_eq!(a1.as_ref(), "foo");
        assert_eq!(&a1, &a2);
        assert!(a2.is_inline());
        assert!(Atom::heap_count() == 0);
        let on_heap = Atom::new(&"0".repeat(24));
        assert!(!on_heap.is_inline());
        assert!(Atom::heap_count() == 1);

        let a3 = Atom::new("0".repeat(24));
        assert_eq!(on_heap, a3);
    }
}
