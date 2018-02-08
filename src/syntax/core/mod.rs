//! The core syntax of the language

use rpds::List;
use std::fmt;
use std::rc::Rc;

use syntax::concrete;
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

/// Top level definitions
pub struct Definition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: RcTerm,
    /// An optional type annotation to aid in type inference
    pub ann: Option<RcTerm>,
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

// Conversions from the concrete syntax

fn lam_from_concrete(
    params: &[(String, Option<Box<concrete::Term>>)],
    body: &concrete::Term,
) -> RcTerm {
    let mut term = RcTerm::from_concrete(body);

    for &(ref name, ref ann) in params.iter().rev() {
        let name = Name::User(name.clone());
        term = match *ann {
            None => Term::Lam(TermLam::bind(Named::new(name, None), term)).into(),
            Some(ref ann) => {
                let ann = RcTerm::from_concrete(ann);
                Term::Lam(TermLam::bind(Named::new(name, Some(ann)), term)).into()
            },
        };
    }

    term
}

fn pi_from_concrete(param_names: &[String], ann: &concrete::Term, body: &concrete::Term) -> RcTerm {
    let ann = RcTerm::from_concrete(ann);
    let mut term = RcTerm::from_concrete(body);

    for name in param_names.iter().rev() {
        // This could be wrong... :/
        term = Term::Pi(TermPi::bind(
            Named::new(Name::User(name.clone()), ann.clone()),
            term,
        )).into();
    }

    term
}

impl Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    pub fn from_concrete(module: &concrete::Module) -> Module {
        use std::collections::BTreeMap;
        use std::collections::btree_map::Entry;

        // The type claims that we have encountered so far! We'll use these when
        // we encounter their corresponding definitions later as type annotations
        let mut claims = BTreeMap::new();
        // The definitions, desugared from the concrete syntax
        let mut definitions = Vec::<Definition>::new();

        for declaration in &module.declarations {
            match *declaration {
                concrete::Declaration::Import(_, _, _) => unimplemented!("import declarations"),
                // We've enountered a claim! Let's try to add it to the claims
                // that we've seen so far...
                concrete::Declaration::Claim(ref name, ref term) => {
                    match claims.entry(name) {
                        // Oh no! We've already seen a claim for this name!
                        Entry::Occupied(_) => panic!(), // FIXME: Better error
                        // This name does not yet have a claim associated with it
                        Entry::Vacant(mut entry) => {
                            let mut term = RcTerm::from_concrete(term);

                            for (level, definition) in definitions.iter().rev().enumerate() {
                                term.close_at(
                                    Debruijn(level as u32),
                                    &Name::user(definition.name.clone()),
                                );
                            }

                            entry.insert(term)
                        },
                    };
                },
                // We've encountered a definition. Let's desugar it!
                concrete::Declaration::Definition(ref name, ref params, ref term) => {
                    let name = name.clone();
                    let mut term = lam_from_concrete(params, term);
                    let ann = claims.remove(&name);

                    for (level, definition) in definitions.iter().rev().enumerate() {
                        term.close_at(Debruijn(level as u32), &Name::user(definition.name.clone()));
                    }

                    definitions.push(Definition { name, term, ann });
                },
            }
        }

        // FIXME: Better error
        assert!(claims.is_empty());

        Module {
            name: module.name.clone(),
            definitions,
        }
    }
}

impl RcTerm {
    /// Convert a term in the concrete syntax into a core term
    pub fn from_concrete(term: &concrete::Term) -> RcTerm {
        match *term {
            concrete::Term::Parens(ref term) => RcTerm::from_concrete(term),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = RcTerm::from_concrete(expr).into();
                let ty = RcTerm::from_concrete(ty).into();

                Term::Ann(expr, ty).into()
            },
            concrete::Term::Universe(None) => Term::Universe(Level::ZERO).into(),
            concrete::Term::Universe(Some(level)) => Term::Universe(Level(level)).into(),
            concrete::Term::Var(ref x) => Term::Var(Var::Free(Name::User(x.clone()))).into(),
            concrete::Term::Lam(ref params, ref body) => lam_from_concrete(params, body),
            concrete::Term::Pi(ref names, ref ann, ref body) => pi_from_concrete(names, ann, body),
            concrete::Term::Arrow(ref ann, ref body) => Term::Pi(TermPi::bind(
                Named::new(Name::fresh(None::<&str>), RcTerm::from_concrete(ann).into()),
                RcTerm::from_concrete(body),
            )).into(),
            concrete::Term::App(ref fn_expr, ref arg) => {
                let fn_expr = RcTerm::from_concrete(fn_expr).into();
                let arg = RcTerm::from_concrete(arg).into();

                Term::App(fn_expr, arg).into()
            },
        }
    }
}
