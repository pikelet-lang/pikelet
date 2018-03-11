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

use std::rc::Rc;

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
    fn fresh() -> Self;

    /// Generate a new, globally unique name
    fn freshen(&mut self);
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

impl<T: LocallyNameless> LocallyNameless for Box<T> {
    type Name = T::Name;

    fn close_at(&mut self, index: Debruijn, name: &T::Name) {
        (**self).close_at(index, name);
    }

    fn open_at(&mut self, index: Debruijn, name: &T::Name) {
        (**self).open_at(index, name);
    }
}

impl<T: LocallyNameless + Clone> LocallyNameless for Rc<T> {
    type Name = T::Name;

    fn close_at(&mut self, index: Debruijn, name: &T::Name) {
        Rc::make_mut(self).close_at(index, name);
    }

    fn open_at(&mut self, index: Debruijn, name: &T::Name) {
        Rc::make_mut(self).open_at(index, name);
    }
}
