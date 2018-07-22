//! The syntax of the language

use moniker::{Binder, BoundPattern, BoundTerm, ScopeState, Var};
use std::fmt;
use std::hash::Hash;

pub mod concrete;
pub mod context;
pub mod core;
pub mod parse;
pub mod pretty;
pub mod prim;
pub mod raw;
pub mod translation;

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, BoundTerm)]
pub struct Level(pub u32);

impl Level {
    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A record label
///
/// Labels are significant when comparing for alpha-equality, both in terms and
/// in patterns
#[derive(Debug, Clone, PartialEq)]
pub struct Label<N>(pub Binder<N>);

impl<N> BoundTerm<N> for Label<N>
where
    N: PartialEq,
{
    fn term_eq(&self, other: &Label<N>) -> bool {
        (self.0).0.ident() == (other.0).0.ident() || self.0 == other.0
    }

    fn close_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn open_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N> BoundPattern<N> for Label<N>
where
    N: Clone + Eq + Hash + fmt::Debug + fmt::Display,
{
    fn pattern_eq(&self, other: &Label<N>) -> bool {
        Label::term_eq(self, other)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &[Binder<N>]) {
        self.0.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &[Binder<N>]) {
        self.0.open_pattern(state, pattern);
    }

    fn visit_binders(&self, on_bound: &mut impl FnMut(&Binder<N>)) {
        self.0.visit_binders(on_bound);
    }

    fn visit_mut_binders(&mut self, on_bound: &mut impl FnMut(&mut Binder<N>)) {
        self.0.visit_mut_binders(on_bound);
    }
}

impl<N: fmt::Display> fmt::Display for Label<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
