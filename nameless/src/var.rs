use std::fmt;

use {AlphaEq, Debruijn, FreeName, LocallyNameless, Named, OnBoundFn, OnFreeFn};

/// A variable that can either be free or bound
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Var<N, B> {
    /// A free variable
    Free(N),
    /// A variable that is bound by a lambda or pi binder
    Bound(Named<N, B>),
}

impl<N: AlphaEq, B: AlphaEq> AlphaEq for Var<N, B> {
    fn alpha_eq(&self, other: &Var<N, B>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => N::alpha_eq(lhs, rhs),
            (&Var::Bound(ref lhs), &Var::Bound(ref rhs)) => Named::alpha_eq(lhs, rhs),
            (_, _) => false,
        }
    }
}

impl<N: FreeName, B> LocallyNameless for Var<N, B> {
    type Name = N;
    type Bound = B;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<N, B>) {
        *self = match *self {
            Var::Bound(_) => return,
            Var::Free(ref name) => match on_free(index, name) {
                Some(index) => Var::Bound(Named::new(name.clone(), index)),
                None => return,
            },
        };
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<N, B>) {
        *self = match *self {
            Var::Free(_) => return,
            Var::Bound(Named { ref inner, .. }) => match on_bound(index, inner) {
                Some(name) => Var::Free(name),
                None => return,
            },
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
