use std::{collections::HashMap, num::NonZeroUsize};

use lasso::Key;

pub struct Intern(lasso::Rodeo<Atom, ahash::RandomState>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Atom(u32);

unsafe impl Key for Atom {
    fn into_usize(self) -> usize {
        self.0 as usize
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        if int < std::u32::MAX as usize {
            Some(Atom(int as u32))
        } else {
            None
        }
    }
}

impl Intern {
    pub fn new() -> Self {
        Intern(lasso::Rodeo::with_capacity_and_hasher(
            lasso::Capacity::new(512, unsafe { NonZeroUsize::new_unchecked(4096 * 2) }),
            ahash::RandomState::new(),
        ))
    }

    pub fn intern(&mut self, s: &str) -> Atom {
        self.0.get_or_intern(s)
    }

    pub fn intern_static(&mut self, s: &'static str) -> Atom {
        self.0.get_or_intern_static(s)
    }

    pub fn get(&self, atom: &Atom) -> &str {
        self.0.resolve(atom)
    }
}

impl Default for Intern {
    fn default() -> Self {
        Intern::new()
    }
}