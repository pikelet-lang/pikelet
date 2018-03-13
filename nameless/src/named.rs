use std::hash::{Hash, Hasher};

use {AlphaEq, Debruijn, LocallyNameless, OnBoundFn, OnFreeFn};

/// A type annotated with a name for debugging purposes
///
/// The name is ignored for alpha equality comparisons
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named<N, T> {
    pub name: N,
    pub inner: T,
}

impl<N, T> Named<N, T> {
    pub fn new(name: N, inner: T) -> Named<N, T> {
        Named { name, inner }
    }
}

impl<N, T: AlphaEq> AlphaEq for Named<N, T> {
    fn alpha_eq(&self, other: &Named<N, T>) -> bool {
        T::alpha_eq(&self.inner, &other.inner)
    }
}

impl<T: LocallyNameless> LocallyNameless for Named<T::Name, T> {
    type Name = T::Name;
    type Bound = T::Bound;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<T::Name, T::Bound>) {
        self.inner.close_at(index, on_free);
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<T::Name, T::Bound>) {
        self.inner.open_at(index, on_bound);
    }
}

impl<N, T: Hash> Hash for Named<N, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}
