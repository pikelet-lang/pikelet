use std::fmt;

/// The name of a free variable
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name(pub String);

/// A type annotated with a name for debugging purposes
///
/// The name is ignored for equality comparisons
#[derive(Debug, Clone, Eq)]
pub struct Named<T>(pub Name, pub T);

impl<T: PartialEq> PartialEq for Named<T> {
    fn eq(&self, other: &Named<T>) -> bool {
        &self.1 == &other.1
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
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Debruijn(pub u32);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debruijn {
    /// The debruijn index of the current binder
    pub fn zero() -> Debruijn {
        Debruijn(0)
    }

    /// Move the current debruijn index into an inner binder
    pub fn succ(self) -> Debruijn {
        Debruijn(self.0 + 1)
    }
}

impl fmt::Display for Debruijn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Var {
    /// A free variable
    Free(Name),
    /// A variable that is bound by a lambda or pi binder
    Bound(Named<Debruijn>),
}

impl Var {
    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *self {
            Var::Free(ref n) if n == name => Var::Bound(Named(n.clone(), level)),
            Var::Bound(_) | Var::Free(_) => return,
        };
    }

    pub fn instantiate_at(&self, level: Debruijn) -> bool {
        match *self {
            Var::Bound(Named(_, b)) if b == level => true,
            Var::Bound(_) | Var::Free(_) => false,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(Named(ref name, ref b)) if f.alternate() => write!(f, "{}#{}", name, b),
            Var::Bound(Named(ref name, _)) | Var::Free(ref name) => write!(f, "{}", name),
        }
    }
}
