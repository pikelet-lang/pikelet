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
//!
//! # Inspiration
//!
//! There are a number of libraries out there for other languages that abstract
//! away handling locally nameless representations, but I've not yet figured out
//! how to port them to Rust yet:
//!
//! - DBLib: Facilities for working with de Bruijn indices in Coq
//!     - [Blog Post](http://gallium.inria.fr/blog/announcing-dblib/)
//!     - [Github](https://github.com/coq-contribs/dblib)
//! - Unbound: Specify the binding structure of your data type with an
//!   expressive set of type combinators, and Unbound handles the rest!
//!   Automatically derives alpha-equivalence, free variable calculation,
//!   capture-avoiding substitution, and more.
//!     - [Github](https://github.com/sweirich/replib)
//!     - [Hackage](https://hackage.haskell.org/package/unbound)
//! - Unbound-Generics: an independent re-implementation of Unbound but using
//!   GHC.Generics instead of RepLib.
//!     - [Github](http://github.com/lambdageek/unbound-generics)
//!     - [Hackage](https://hackage.haskell.org/package/unbound-generics)
//! - Bound: Bruijn indices for Haskell
//!     - [Blog Post](https://www.schoolofhaskell.com/user/edwardk/bound)
//!     - [Github](https://github.com/ekmett/bound/)
//!     - [Hackage](https://hackage.haskell.org/package/bound)
//! - The Penn Locally Nameless Metatheory Library
//!     - [Github](https://github.com/plclub/metalib)

#[macro_use]
extern crate lazy_static;
#[cfg(feature = "nameless-derive")]
#[allow(unused_imports)]
#[macro_use]
extern crate nameless_derive;

use std::rc::Rc;

#[cfg(feature = "nameless-derive")]
#[doc(hidden)]
pub use nameless_derive::*;

mod debruijn;
mod gen_id;
mod named;
mod scope;
mod var;

pub use self::debruijn::Debruijn;
pub use self::gen_id::GenId;
pub use self::named::Named;
pub use self::scope::{Scope, unbind2};
pub use self::var::Var;

/// Free names
pub trait FreeName: Clone + PartialEq {
    fn freshen(&mut self);
}

/// Types that may be compared for alpha equality
///
/// Alpha equality ignores the specific _name_ given to a binding, instead
/// looking at the structure of the bindings. For example, the following two
/// terms should be equal despite the fact that they use different names:
///
/// ```text
/// \x y -> x
/// \a b -> a
/// ```
pub trait AlphaEq<Other = Self> {
    fn alpha_eq(&self, other: &Other) -> bool;
}

macro_rules! impl_alpha_eq_eq {
    ($T:ty) => {
        impl AlphaEq for $T {
            fn alpha_eq(&self, other: &$T) -> bool {
                self == other
            }
        }
    };
}

impl_alpha_eq_eq!(u8);
impl_alpha_eq_eq!(u16);
impl_alpha_eq_eq!(u32);
impl_alpha_eq_eq!(u64);
impl_alpha_eq_eq!(usize);
impl_alpha_eq_eq!(i8);
impl_alpha_eq_eq!(i16);
impl_alpha_eq_eq!(i32);
impl_alpha_eq_eq!(i64);
impl_alpha_eq_eq!(isize);
impl_alpha_eq_eq!(String);
impl_alpha_eq_eq!(());

impl<T: AlphaEq> AlphaEq for Option<T> {
    fn alpha_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::alpha_eq(lhs, rhs),
            (_, _) => false,
        }
    }
}

impl<T: AlphaEq> AlphaEq for Box<T> {
    fn alpha_eq(&self, other: &Box<T>) -> bool {
        T::alpha_eq(self, other)
    }
}

impl<T: AlphaEq> AlphaEq for Rc<T> {
    fn alpha_eq(&self, other: &Rc<T>) -> bool {
        T::alpha_eq(self, other)
    }
}

/// Asserts that two expressions are alpha equalent to each other (using [`AlphaEq`]).
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations.
///
/// Like [`assert!`], this macro has a second form, where a custom
/// panic message can be provided.
#[macro_export]
macro_rules! assert_alpha_eq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !::nameless::AlphaEq::alpha_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `(<_>::alpha_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`"#, left_val, right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_alpha_eq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !::nameless::AlphaEq::alpha_eq(left_val, right_val) {
                    panic!(r#"assertion failed: `(<_>::alpha_eq(&left, &right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, left_val, right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}

pub type OnFreeFn<'a, N, B> = &'a Fn(Debruijn, &N) -> Option<B>;
pub type OnBoundFn<'a, N, B> = &'a Fn(Debruijn, &B) -> Option<N>;

pub trait LocallyNameless {
    type FreeName: FreeName;
    type BoundName;

    fn close(&mut self, on_free: OnFreeFn<Self::FreeName, Self::BoundName>) {
        self.close_at(Debruijn::ZERO, on_free);
    }

    fn open(&mut self, on_bound: OnBoundFn<Self::FreeName, Self::BoundName>) {
        self.open_at(Debruijn::ZERO, on_bound);
    }

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<Self::FreeName, Self::BoundName>);
    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<Self::FreeName, Self::BoundName>);
}

impl<T: LocallyNameless> LocallyNameless for Option<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<T::FreeName, T::BoundName>) {
        if let Some(ref mut inner) = *self {
            inner.close_at(index, on_free);
        }
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<T::FreeName, T::BoundName>) {
        if let Some(ref mut inner) = *self {
            inner.open_at(index, on_bound);
        }
    }
}

impl<T: LocallyNameless> LocallyNameless for Box<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<T::FreeName, T::BoundName>) {
        (**self).close_at(index, on_free);
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<T::FreeName, T::BoundName>) {
        (**self).open_at(index, on_bound);
    }
}

impl<T: LocallyNameless + Clone> LocallyNameless for Rc<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<T::FreeName, T::BoundName>) {
        Rc::make_mut(self).close_at(index, on_free);
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<T::FreeName, T::BoundName>) {
        Rc::make_mut(self).open_at(index, on_bound);
    }
}

impl<T: LocallyNameless + Clone> LocallyNameless for [T] {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at(&mut self, index: Debruijn, on_free: OnFreeFn<T::FreeName, T::BoundName>) {
        for elem in self {
            elem.close_at(index, on_free);
        }
    }

    fn open_at(&mut self, index: Debruijn, on_bound: OnBoundFn<T::FreeName, T::BoundName>) {
        for elem in self {
            elem.open_at(index, on_bound);
        }
    }
}
