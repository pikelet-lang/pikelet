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
//! ## Libraries
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

use std::fmt;

/// The name of a free variable
#[derive(Debug, Clone, Eq)]
pub enum Name {
    /// Names originating from user input
    User(String),
    /// Abstract names, `_`
    ///
    /// These are generally used in non-dependent function types, ie:
    ///
    /// ```text
    /// t1 -> t2 -> t3
    /// ```
    ///
    /// will be stored as:
    ///
    /// ```text
    /// [_ : t1] -> [_ : t2] -> t3
    /// ```
    ///
    /// They should never actually appear in terms.
    ///
    /// Comparing two abstract names will always return false because we cannot
    /// be sure what they actually refer to. For example, in the type
    /// shown above, `_` could refer to either `t1` or `t2`.
    Abstract,
}

impl Name {
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        match (self, other) {
            (&Name::User(ref lhs), &Name::User(ref rhs)) => lhs == rhs,
            (&Name::Abstract, &Name::Abstract) | (_, _) => false,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Abstract => write!(f, "_"),
        }
    }
}

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

/// A variable that can either be free or bound
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
