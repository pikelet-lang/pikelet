//! Data types related to variable binding
//!
//! We use a locally nameless representation for variable binding.
//!
//! # References
//!
//! - [How I learned to stop worrying and love de Bruijn indices](http://disciple-devel.blogspot.com.au/2011/08/how-i-learned-to-stop-worrying-and-love.html)
//! - [The Locally Nameless Representation](https://www.chargueraud.org/research/2009/ln/main.pdf)
//! - [Locally nameless representation with cofinite quantification](http://www.chargueraud.org/softs/ln/)
//! - [A Locally-nameless Backend for Ott](http://www.di.ens.fr/~zappa/projects/ln_ott/)
//! - [Library STLC_Tutorial](https://www.cis.upenn.edu/~plclub/popl08-tutorial/code/coqdoc/STLC_Tutorial.html)

#[macro_use]
extern crate lazy_static;

use std::fmt;
use std::hash::{Hash, Hasher};

/// Free names
pub trait FreeName: Clone + PartialEq {
    type Hint;

    /// Generate a new, globally unique name
    fn fresh(hint: Option<Self::Hint>) -> Self;

    fn hint(&self) -> Option<Self::Hint>;
}

pub trait LocallyNameless: Sized {
    type Name: FreeName;

    fn close(&mut self, name: &Self::Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    fn open(&mut self, name: &Self::Name) {
        self.open_at(Debruijn::ZERO, name);
    }

    fn close_at(&mut self, index: Debruijn, name: &Self::Name);
    fn open_at(&mut self, index: Debruijn, name: &Self::Name);
}

impl<T: LocallyNameless> LocallyNameless for Option<T> {
    type Name = T::Name;

    fn close_at(&mut self, index: Debruijn, name: &T::Name) {
        if let Some(ref mut inner) = *self {
            inner.close_at(index, name);
        }
    }

    fn open_at(&mut self, index: Debruijn, name: &T::Name) {
        if let Some(ref mut inner) = *self {
            inner.open_at(index, name);
        }
    }
}

/// A generated id
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GenId(u32);

impl GenId {
    /// Generate a new, globally unique id
    pub fn fresh() -> GenId {
        use std::sync::atomic::{AtomicUsize, Ordering};

        lazy_static! {
            static ref NEXT_ID : AtomicUsize = AtomicUsize::new(0);
        }

        // FIXME: check for integer overflow
        GenId(NEXT_ID.fetch_add(1, Ordering::SeqCst) as u32)
    }
}

impl fmt::Display for GenId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// A type annotated with a name for debugging purposes
///
/// The name is ignored for equality comparisons
#[derive(Debug, Clone, Eq)]
pub struct Named<N, T> {
    pub name: N,
    pub inner: T,
}

impl<N, T> Named<N, T> {
    pub fn new(name: N, inner: T) -> Named<N, T> {
        Named { name, inner }
    }
}

impl<T: LocallyNameless> LocallyNameless for Named<T::Name, T> {
    type Name = T::Name;

    fn close_at(&mut self, index: Debruijn, name: &T::Name) {
        self.inner.close_at(index, name);
    }

    fn open_at(&mut self, index: Debruijn, name: &T::Name) {
        self.inner.open_at(index, name);
    }
}

impl<N, T: PartialEq> PartialEq for Named<N, T> {
    fn eq(&self, other: &Named<N, T>) -> bool {
        &self.inner == &other.inner
    }
}

impl<N, T: Hash> Hash for Named<N, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

/// The [debruijn index] of the binder that introduced the variable
///
/// For example:
///
/// ```text
/// λx.∀y.λz. x z (y z)
/// λ  ∀  λ   2 0 (1 0)
/// ```
///
/// [debruijn index]: https://en.wikipedia.org/wiki/De_Bruijn_index
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Debruijn(pub u32);

impl Debruijn {
    /// The debruijn index of the current binder
    pub const ZERO: Debruijn = Debruijn(0);

    /// Move the current debruijn index into an inner binder
    pub fn succ(self) -> Debruijn {
        Debruijn(self.0 + 1)
    }

    pub fn pred(self) -> Option<Debruijn> {
        match self {
            Debruijn::ZERO => None,
            Debruijn(i) => Some(Debruijn(i - 1)),
        }
    }
}

impl fmt::Display for Debruijn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

/// A variable that can either be free or bound
#[derive(Debug, Clone, PartialEq)]
pub enum Var<N, B> {
    /// A free variable
    Free(N),
    /// A variable that is bound by a lambda or pi binder
    Bound(Named<N, B>),
}

impl<N: FreeName> LocallyNameless for Var<N, Debruijn> {
    type Name = N;

    fn close_at(&mut self, index: Debruijn, name: &N) {
        *self = match *self {
            Var::Free(ref n) if n == name => Var::Bound(Named::new(n.clone(), index)),
            Var::Bound(_) | Var::Free(_) => return,
        };
    }

    fn open_at(&mut self, index: Debruijn, name: &N) {
        *self = match *self {
            Var::Bound(Named { inner: i, .. }) if i == index => Var::Free(name.clone()),
            Var::Bound(_) | Var::Free(_) => return,
        };
    }
}

impl<N: fmt::Display, B: fmt::Display> fmt::Display for Var<N, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(ref bound) if f.alternate() => write!(f, "{}{}", bound.name, bound.inner),
            Var::Bound(Named { ref name, .. }) | Var::Free(ref name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<B, T> {
    pub unsafe_binder: B,
    pub unsafe_body: T,
}

impl<N: FreeName, B, T> Scope<Named<N, B>, T>
where
    B: LocallyNameless<Name = N>,
    T: LocallyNameless<Name = N>,
{
    pub fn bind(binder: Named<N, B>, mut body: T) -> Scope<Named<N, B>, T> {
        body.close(&binder.name);

        Scope {
            unsafe_binder: binder,
            unsafe_body: body,
        }
    }

    pub fn unbind(self) -> (Named<N, B>, T) {
        let mut binder = self.unsafe_binder;
        let mut body = self.unsafe_body;

        let free_name = N::fresh(binder.name.hint());
        body.open(&free_name);
        binder.name = free_name;

        (binder, body)
    }
}

impl<N: FreeName, B, T> LocallyNameless for Scope<Named<N, B>, T>
where
    B: LocallyNameless<Name = N>,
    T: LocallyNameless<Name = N>,
{
    type Name = N;

    fn close_at(&mut self, index: Debruijn, name: &N) {
        self.unsafe_binder.close_at(index, name);
        self.unsafe_body.close_at(index.succ(), name);
    }

    fn open_at(&mut self, index: Debruijn, name: &N) {
        self.unsafe_binder.open_at(index, &name);
        self.unsafe_body.open_at(index.succ(), &name);
    }
}

pub fn unbind2<N, B1, T1, B2, T2>(
    scope1: Scope<Named<N, B1>, T1>,
    scope2: Scope<Named<N, B2>, T2>,
) -> (Named<N, B1>, T1, Named<N, B2>, T2)
where
    N: FreeName,
    B1: LocallyNameless<Name = N>,
    T1: LocallyNameless<Name = N>,
    B2: LocallyNameless<Name = N>,
    T2: LocallyNameless<Name = N>,
{
    let mut scope1_binder = scope1.unsafe_binder;
    let mut scope1_body = scope1.unsafe_body;
    let mut scope2_binder = scope2.unsafe_binder;
    let mut scope2_body = scope2.unsafe_body;

    let free_name = N::fresh(scope1_binder.name.hint());
    scope1_body.open(&free_name);
    scope2_body.open(&free_name);
    scope1_binder.name = free_name.clone();
    scope2_binder.name = free_name;

    (scope1_binder, scope1_body, scope2_binder, scope2_body)
}
