use moniker::{Binder, BoundPattern, BoundTerm, OnBoundFn, OnFreeFn, ScopeState, Var};

pub mod concrete;
pub mod raw;

const PRETTY_INDENT_WIDTH: usize = 4;

/// An effectively 'infinite' line length for when we don't have an explicit
/// width provided for pretty printing.
///
/// `pretty.rs` seems to bug-out and break on every line when using
/// `usize::MAX`, so we'll just use a really big number instead...
pub const PRETTY_FALLBACK_WIDTH: usize = 1_000_000;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntFormat {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl<N: Clone + PartialEq> BoundTerm<N> for IntFormat {
    fn term_eq(&self, _: &IntFormat) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}
    fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}
    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}
    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N: Clone + PartialEq> BoundPattern<N> for IntFormat {
    fn pattern_eq(&self, _: &IntFormat) -> bool {
        true
    }

    fn close_pattern(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}
    fn open_pattern(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}
    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}
    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}
    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatFormat {
    Dec,
    // TODO: Binary and Hex floats?
}

impl<N: Clone + PartialEq> BoundTerm<N> for FloatFormat {
    fn term_eq(&self, _: &FloatFormat) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}
    fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}
    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}
    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N: Clone + PartialEq> BoundPattern<N> for FloatFormat {
    fn pattern_eq(&self, _: &FloatFormat) -> bool {
        true
    }

    fn close_pattern(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}
    fn open_pattern(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}
    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}
    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}
    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}
