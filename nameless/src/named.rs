use std::hash::{Hash, Hasher};

use {AlphaEq, Binder, Bound, Debruijn, FreeName};

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

impl<T: Bound> Bound for Named<T::FreeName, T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.inner.close_at(index, binder);
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.inner.open_at(index, binder);
    }
}

impl<N: FreeName, T> Binder for Named<N, T>
where
    T: Bound<FreeName = N, BoundName = Debruijn>,
{
    fn on_free(&self, index: Debruijn, name: &Self::FreeName) -> Option<Debruijn> {
        match *name == self.name {
            true => Some(index),
            false => None,
        }
    }

    fn on_bound(&self, index: Debruijn, name: &Debruijn) -> Option<Self::FreeName> {
        match *name == index {
            true => Some(self.name.clone()),
            false => None,
        }
    }
}

impl<N, T: Hash> Hash for Named<N, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}
