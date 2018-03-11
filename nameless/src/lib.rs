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
