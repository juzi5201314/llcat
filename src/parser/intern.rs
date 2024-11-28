use std::{
    cell::RefCell,
    collections::HashMap,
    mem::MaybeUninit,
    num::{NonZeroU32, NonZeroUsize},
    ops::Deref,
    rc::Rc,
};

pub struct Intern(Rc<RefCell<lasso::Rodeo<lasso::Spur, ahash::RandomState>>>);

#[derive(Debug, Clone)]
pub struct Atom {
    idx: lasso::Spur,
    pool: Rc<RefCell<lasso::Rodeo<lasso::Spur, ahash::RandomState>>>,
}

impl Eq for Atom {}
impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        (self.idx == other.idx && Rc::ptr_eq(&self.pool, &other.pool))
            || self.as_ref().as_str() == other.as_ref().as_str()
    }
}

impl PartialEq<str> for Atom {
    fn eq(&self, other: &str) -> bool {
        self.as_ref().as_str() == other
    }
}

/// 请保证在任何AtomRef存活时不写入Intern, 否则将panic
pub struct AtomRef<'a> {
    guard: std::cell::Ref<'a, lasso::Rodeo<lasso::Spur, ahash::RandomState>>,
    idx: lasso::Spur,
}

impl<'a> Atom {
    pub fn as_ref(&'a self) -> AtomRef<'a> {
        let guard = self.pool.borrow();
        AtomRef {
            guard,
            idx: self.idx,
        }
    }
}

impl<'a> AsRef<str> for AtomRef<'a> {
    fn as_ref(&self) -> &str {
        self.guard.resolve(&self.idx)
    }
}

impl<'a> AtomRef<'a> {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }
}

impl Intern {
    pub fn new() -> Self {
        Intern(Rc::new(RefCell::new(
            lasso::Rodeo::with_capacity_and_hasher(
                lasso::Capacity::new(512, unsafe { NonZeroUsize::new_unchecked(4096 * 2) }),
                ahash::RandomState::new(),
            ),
        )))
    }

    pub fn intern(&mut self, s: &str) -> Atom {
        Atom {
            idx: self.0.borrow_mut().get_or_intern(s),
            pool: self.0.clone(),
        }
    }

    pub fn intern_static(&mut self, s: &'static str) -> Atom {
        Atom {
            idx: self.0.borrow_mut().get_or_intern_static(s),
            pool: self.0.clone(),
        }
    }
}

impl Default for Intern {
    fn default() -> Self {
        Intern::new()
    }
}