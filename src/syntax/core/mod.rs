//! The core syntax of the language

use rpds::List;
use std::fmt;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::var::{Debruijn, GenId, Named, Var};

// YUCK!
mod nameplate_ickiness;

pub use self::nameplate_ickiness::unbind2;

#[cfg(test)]
mod tests;

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// Names originating from user input
    User(String),
    /// A generated id with an optional string that may have come from user input
    Gen(Named<Option<String>, GenId>),
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }

    /// Generate a new, globally unique name
    pub fn fresh<S: Into<String>>(name: Option<S>) -> Name {
        Name::Gen(Named::new(name.map(S::into), GenId::fresh()))
    }

    pub fn name(&self) -> Option<&str> {
        match *self {
            Name::User(ref name) => Some(name),
            Name::Gen(Named { ref name, .. }) => name.as_ref().map(String::as_str),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Gen(ref gen) => match gen.name {
                None => write!(f, "{}", gen.inner),
                Some(ref name) => write!(f, "{}{}", name, gen.inner),
            },
        }
    }
}

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(pub u32);

impl Level {
    pub const ZERO: Level = Level(0);

    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A module definition
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// Top level definitions
pub struct Definition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: RcTerm,
    /// An optional type annotation to aid in type inference
    pub ann: Option<RcTerm>,
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// The core term syntax
///
/// ```text
/// e,ρ ::= e:ρ         1. annotated terms
///       | Typeᵢ       2. universes
///       | x           3. variables
///       | λx:ρ₁.ρ₂    4. lambda abstractions
///       | Πx:ρ₁.ρ₂    5. dependent function types
///       | ρ₁ ρ₂       6. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm), // 1.
    /// Universes
    Universe(Level), // 2.
    /// A variable
    Var(Var<Name>), // 3.
    /// Lambda abstractions
    Lam(TermLam), // 4.
    /// Dependent function types
    Pi(TermPi), // 5.
    /// Term application
    App(RcTerm, RcTerm), // 6.
}

impl From<Var<Name>> for Term {
    fn from(src: Var<Name>) -> Term {
        Term::Var(src)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct TermLam {
    pub unsafe_param: Named<Name, Option<RcTerm>>,
    pub unsafe_body: RcTerm,
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct TermPi {
    pub unsafe_param: Named<Name, RcTerm>,
    pub unsafe_body: RcTerm,
}

/// Normal forms
///
/// ```text
/// v,τ ::= Typeᵢ       1. universes
///       | x           2. variables
///       | λx:τ₁.τ₂    3. lambda abstractions
///       | Πx:τ₁.τ₂    4. dependent function types
///       | τ₁ τ₂       5. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Universes
    Universe(Level), // 1.
    /// Variables
    Var(Var<Name>), // 2.
    /// A lambda abstraction
    Lam(ValueLam), // 3.
    /// A pi type
    Pi(ValuePi), // 4.
    /// Term application
    App(RcValue, RcValue), // 5.
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct ValueLam {
    pub unsafe_param: Named<Name, Option<RcValue>>,
    pub unsafe_body: RcValue,
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct ValuePi {
    pub unsafe_param: Named<Name, RcValue>,
    pub unsafe_body: RcValue,
}

// Wrapper types

macro_rules! make_wrapper {
    ($name:ident, $wrapper:ident, $inner:ty) => {
        #[derive(Clone, PartialEq)]
        pub struct $name {
            pub inner: $wrapper<$inner>,
        }

        impl From<$inner> for $name {
            fn from(src: $inner) -> $name {
                $name {
                    inner: $wrapper::new(src),
                }
            }
        }

        impl $crate::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut $crate::std::fmt::Formatter) -> $crate::std::fmt::Result {
                $crate::std::fmt::Debug::fmt(&self.inner, f)
            }
        }

        impl $crate::std::fmt::Display for $name {
            fn fmt(&self, f: &mut $crate::std::fmt::Formatter) -> $crate::std::fmt::Result {
                $crate::std::fmt::Display::fmt(&self.inner, f)
            }
        }
    };
}

make_wrapper!(RcTerm, Rc, Term);
make_wrapper!(RcValue, Rc, Value);

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

/// A binder that introduces a variable into the context
///
/// ```text
/// b ::= λx:τ           1. lambda abstraction
///     | Πx:τ           2. dependent function
///     | let x:τ = v    3. let binding
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam(Option<RcType>), // 1.
    /// A type introduced after entering a pi type
    Pi(RcType), // 2.
    /// A value and type binding that was introduced by passing over a let binding
    Let(RcValue, RcType), // 3.
}

/// A list of binders that have been accumulated during typechecking
///
/// ```text
/// Γ ::= ε           1. empty context
///     | Γ,b         2. context extension
/// ```
#[derive(Clone, PartialEq)]
pub struct Context {
    pub binders: List<(Name, Binder)>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, name: Name, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front((name, binder)),
        }
    }

    pub fn lookup_binder(&self, name: &Name) -> Option<&Binder> {
        self.binders
            .iter()
            .find(|&&(ref n, _)| n == name)
            .map(|&(_, ref b)| b)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtBinders<'a>(&'a List<(Name, Binder)>);

        impl<'a> fmt::Debug for FmtBinders<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("binders", &FmtBinders(&self.binders))
            .finish()
    }
}
