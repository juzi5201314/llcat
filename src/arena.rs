use std::{alloc::Allocator, ptr::NonNull, rc::Rc};

use allocator_api2::alloc::Allocator as Allocator2;

#[derive(Clone)]
pub struct BumpArena(pub Rc<bumpalo::Bump>);

unsafe impl Allocator for BumpArena {
    fn allocate(
        &self,
        layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        <&bumpalo::Bump as Allocator>::allocate(&&*self.0, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: std::alloc::Layout) {
        <&bumpalo::Bump as Allocator>::deallocate(&&*self.0, ptr, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        <&bumpalo::Bump as Allocator>::grow(&&*self.0, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        <&bumpalo::Bump as Allocator>::grow_zeroed(&&*self.0, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        <&bumpalo::Bump as Allocator>::shrink(&&*self.0, ptr, old_layout, new_layout)
    }
}

unsafe impl Allocator2 for BumpArena {
    fn allocate(
        &self,
        layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, allocator_api2::alloc::AllocError> {
        <BumpArena as Allocator>::allocate(self, layout)
            .map_err(|_| allocator_api2::alloc::AllocError)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: std::alloc::Layout) {
        <BumpArena as Allocator>::deallocate(self, ptr, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, allocator_api2::alloc::AllocError> {
        <BumpArena as Allocator>::grow(self, ptr, old_layout, new_layout)
            .map_err(|_| allocator_api2::alloc::AllocError)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, allocator_api2::alloc::AllocError> {
        <BumpArena as Allocator>::grow_zeroed(self, ptr, old_layout, new_layout)
            .map_err(|_| allocator_api2::alloc::AllocError)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, allocator_api2::alloc::AllocError> {
        <BumpArena as Allocator>::shrink(self, ptr, old_layout, new_layout)
            .map_err(|_| allocator_api2::alloc::AllocError)
    }
}

impl Default for BumpArena {
    fn default() -> Self {
        BumpArena::with_capacity(4096)
    }
}

impl BumpArena {
    pub fn with_capacity(capacity: usize) -> Self {
        BumpArena(Rc::new(bumpalo::Bump::with_capacity(capacity)))
    }
}

#[cfg(test)]
mod tests {
    use crate::arena::BumpArena;

    #[test]
    fn unsafe_arena() {
        let arena = BumpArena::default();
        let mut vec = Vec::new_in(arena.clone());
        vec.push("1");
        vec.push("2");
        let mut map = hashbrown::HashMap::<_, _, _, BumpArena>::new_in(arena.clone());
        map.insert("foo", "1");
        map.insert("bar", "2");
        assert_eq!(&vec[0], map.get("foo").unwrap());
        let s = { vec.pop().unwrap() };
        assert_eq!(&s, map.get("bar").unwrap());
    }
}
