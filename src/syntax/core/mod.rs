//! The core syntax of the language

use rpds::List;
use std::fmt;
use std::rc::Rc;

use syntax::concrete;
use syntax::pretty::{self, ToDoc};
use syntax::var::{Debruijn, Name, Named, Var};

#[cfg(test)]
mod tests;

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
///       | Type        2. universes
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
    Universe, // 2.
    /// A variable
    Var(Var), // 3.
    /// Lambda abstractions
    Lam(Named<Option<RcTerm>>, RcTerm), // 4.
    /// Dependent function types
    Pi(Named<RcTerm>, RcTerm), // 5.
    /// Term application
    App(RcTerm, RcTerm), // 6.
}

impl From<Var> for Term {
    fn from(src: Var) -> Term {
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

/// Normal forms
///
/// ```text
/// v,τ ::= Type        1. universes
///       | x           2. variables
///       | λx:τ₁.τ₂    3. lambda abstractions
///       | Πx:τ₁.τ₂    4. dependent function types
///       | τ₁ τ₂       5. term application
///```
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Universes
    Universe, // 1.
    /// Variables
    Var(Var), // 2.
    /// A lambda abstraction
    Lam(Named<Option<RcValue>>, RcValue), // 3.
    /// A pi type
    Pi(Named<RcValue>, RcValue), // 4.
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

impl RcTerm {
    pub fn universe() -> RcTerm {
        Term::Universe.into()
    }
}

impl RcValue {
    pub fn universe() -> RcValue {
        Value::Universe.into()
    }
}

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

impl fmt::Display for Binder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// A list of binders that have been accumulated during typechecking
///
/// ```text
/// Γ ::= ε           1. empty context
///     | Γ,b         2. context extension
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub binders: List<Named<Binder>>,
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, binder: Named<Binder>) -> Context {
        Context {
            binders: self.binders.push_front(binder),
        }
    }

    /// Look up a binder based on the given Debruijn index, returning `None` if
    /// the index is out of scope
    pub fn lookup_binder(&self, index: Debruijn) -> Option<&Named<Binder>> {
        self.binders.iter().nth(index.0 as usize)
    }
}

// Abstraction and instantiation

impl RcTerm {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Term::Ann(ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
            },
            Term::Universe => {},
            Term::Var(ref mut var) => var.close_at(level, name),
            Term::Lam(Named(_, None), ref mut body) => body.close_at(level.succ(), name),
            Term::Lam(Named(_, Some(ref mut ty)), ref mut body) => {
                ty.close_at(level, name);
                body.close_at(level.succ(), name);
            },
            Term::Pi(Named(_, ref mut ty), ref mut body) => {
                ty.close_at(level, name);
                body.close_at(level.succ(), name);
            },
            Term::App(ref mut f, ref mut x) => {
                f.close_at(level, name);
                x.close_at(level, name);
            },
        }
    }
}

impl RcValue {
    pub fn open(&self, x: &RcValue) -> RcValue {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcValue) -> RcValue {
        match *self.inner {
            Value::Universe => self.clone(),
            Value::Var(ref var) => match var.open_at(level) {
                true => x.clone(),
                false => self.clone(),
            },
            Value::Lam(Named(ref name, ref param_ty), ref body) => {
                let param_ty = param_ty.as_ref().map(|param_ty| param_ty.open_at(level, x));
                let body = body.open_at(level.succ(), x);

                Value::Lam(Named(name.clone(), param_ty), body).into()
            },
            Value::Pi(Named(ref name, ref param_ty), ref body) => {
                let param_ty = param_ty.open_at(level, x);
                let body = body.open_at(level.succ(), x);

                Value::Pi(Named(name.clone(), param_ty), body).into()
            },
            Value::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Value::App(fn_expr, arg).into()
            },
        }
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
        term.close(&name);
        term = match *ann {
            None => Term::Lam(Named(name, None), term).into(),
            Some(ref ann) => {
                let ann = RcTerm::from_concrete(ann);
                Term::Lam(Named(name, Some(ann)), term).into()
            },
        };
    }

    term
}

fn pi_from_concrete(param_names: &[String], ann: &concrete::Term, body: &concrete::Term) -> RcTerm {
    let mut ann = RcTerm::from_concrete(ann);
    let mut term = RcTerm::from_concrete(body);

    for name in param_names.iter().rev() {
        let name = Name::User(name.clone());
        ann.close(&Name::Abstract);
        term.close(&name);
        term = Term::Pi(Named(name, ann.clone()), term).into();
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
            concrete::Term::Universe => RcTerm::universe(),
            concrete::Term::Var(ref x) => Term::Var(Var::Free(Name::User(x.clone()))).into(),
            concrete::Term::Lam(ref params, ref body) => lam_from_concrete(params, body),
            concrete::Term::Pi(ref names, ref ann, ref body) => pi_from_concrete(names, ann, body),
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = Name::Abstract;
                let mut body = RcTerm::from_concrete(body);
                body.close(&name);

                Term::Pi(Named(name, RcTerm::from_concrete(ann).into()), body).into()
            },
            concrete::Term::App(ref fn_expr, ref arg) => {
                let fn_expr = RcTerm::from_concrete(fn_expr).into();
                let arg = RcTerm::from_concrete(arg).into();

                Term::App(fn_expr, arg).into()
            },
        }
    }
}
