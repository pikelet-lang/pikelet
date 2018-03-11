//! The core syntax of the language

use codespan::ByteSpan;
use nameless::{Debruijn, LocallyNameless, Named, Scope, Var};
use rpds::List;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::usize;

use syntax::pretty::{self, ToDoc};

#[macro_use]
mod macros;

mod name;
#[cfg(test)]
mod tests;

pub use self::name::{Ident, Name};

/// Source metadata that should be ignored when checking for alpha equality
#[derive(Debug, Copy, Clone)]
pub struct SourceMeta {
    pub span: ByteSpan,
}

// TODO: Derive this
impl LocallyNameless for SourceMeta {
    type Name = Name;

    fn close_at(&mut self, _: Debruijn, _: &Name) {}
    fn open_at(&mut self, _: Debruijn, _: &Name) {}
}

impl Default for SourceMeta {
    fn default() -> SourceMeta {
        SourceMeta {
            span: ByteSpan::default(),
        }
    }
}

impl PartialEq for SourceMeta {
    fn eq(&self, _: &SourceMeta) -> bool {
        true
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

// TODO: Derive this
impl LocallyNameless for Level {
    type Name = Name;

    fn close_at(&mut self, _: Debruijn, _: &Name) {}
    fn open_at(&mut self, _: Debruijn, _: &Name) {}
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A module definition
pub struct RawModule {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<RawDefinition>,
}

impl fmt::Display for RawModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Top level definitions
pub struct RawDefinition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: RcRawTerm,
    /// An optional type annotation to aid in type inference
    pub ann: RcRawTerm,
}

impl fmt::Display for RawDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Raw terms, unchecked and with implicit syntax that needs to be elaborated
///
/// ```text
/// r,R ::= r:R         1. annotated terms
///       | Typeᵢ       2. universes
///       | _           3. hole
///       | x           4. variables
///       | Πx:R₁.R₂    5. dependent function types
///       | λx:R.r      6. lambda abstractions
///       | R₁ R₂       7. term application
/// ```
#[derive(Debug, Clone, PartialEq, LocallyNameless)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(SourceMeta, RcRawTerm, RcRawTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// A hole
    Hole(SourceMeta), // 3.
    /// A variable
    Var(SourceMeta, Var<Name, Debruijn>), // 4.
    /// Dependent function types
    Pi(SourceMeta, Scope<Named<Name, RcRawTerm>, RcRawTerm>), // 5.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<Named<Name, RcRawTerm>, RcRawTerm>), // 6.
    /// RawTerm application
    App(SourceMeta, RcRawTerm, RcRawTerm), // 7.
}

impl RcRawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            RawTerm::Ann(meta, _, _)
            | RawTerm::Universe(meta, _)
            | RawTerm::Hole(meta)
            | RawTerm::Var(meta, _)
            | RawTerm::Pi(meta, _)
            | RawTerm::Lam(meta, _)
            | RawTerm::App(meta, _, _) => meta.span,
        }
    }
}

impl fmt::Display for RawTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

impl RcRawTerm {
    fn visit_vars<F: FnMut(&Var<Name, Debruijn>)>(&self, on_var: &mut F) {
        match *self.inner {
            RawTerm::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            RawTerm::Universe(_, _) | RawTerm::Hole(_) => {},
            RawTerm::Var(_, ref var) => on_var(var),
            RawTerm::Pi(_, ref pi) => {
                pi.unsafe_binder.inner.visit_vars(on_var);
                pi.unsafe_body.visit_vars(on_var);
            },
            RawTerm::Lam(_, ref lam) => {
                lam.unsafe_binder.inner.visit_vars(on_var);
                lam.unsafe_body.visit_vars(on_var);
            },
            RawTerm::App(_, ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
        };
    }

    pub fn free_vars(&self) -> HashSet<Name> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

/// A typechecked and elaborated module
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

/// A typechecked and elaborated definition
pub struct Definition {
    /// The name of the definition
    pub name: String,
    /// The elaborated value
    pub term: RcTerm,
    /// The type of the definition
    pub ann: RcType,
}

/// The core term syntax
///
/// ```text
/// t,T ::= t:T         1. annotated terms
///       | Typeᵢ       2. universes
///       | x           3. variables
///       | Πx:T₁.T₂    4. dependent function types
///       | λx:T.t      5. lambda abstractions
///       | t₁ t₂       6. term application
/// ```
#[derive(Debug, Clone, PartialEq, LocallyNameless)]
pub enum Term {
    /// A term annotated with a type
    Ann(SourceMeta, RcTerm, RcTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// A variable
    Var(SourceMeta, Var<Name, Debruijn>), // 3.
    /// Dependent function types
    Pi(SourceMeta, Scope<Named<Name, RcTerm>, RcTerm>), // 5.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<Named<Name, RcTerm>, RcTerm>), // 4.
    /// Term application
    App(SourceMeta, RcTerm, RcTerm), // 6.
}

impl RcTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            Term::Ann(meta, _, _)
            | Term::Universe(meta, _)
            | Term::Var(meta, _)
            | Term::Lam(meta, _)
            | Term::Pi(meta, _)
            | Term::App(meta, _, _) => meta.span,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Normal forms
///
/// ```text
/// v,V ::= Typeᵢ       1. universes
///       | Πx:V₁.V₂    2. dependent function types
///       | λx:V.v      3. lambda abstractions
///       | n           4. neutral terms
/// ```
#[derive(Debug, Clone, PartialEq, LocallyNameless)]
pub enum Value {
    /// Universes
    Universe(Level), // 1.
    /// A pi type
    Pi(Scope<Named<Name, RcValue>, RcValue>), // 2.
    /// A lambda abstraction
    Lam(Scope<Named<Name, RcValue>, RcValue>), // 3.
    /// Neutral terms
    Neutral(RcNeutral), // 4.
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Neutral terms
///
/// These might be able to be reduced further depending on the bindings in the
/// context
///
/// ```text
/// n,N ::= x           1. variables
///       | n t         2. term application
/// ```
#[derive(Debug, Clone, PartialEq, LocallyNameless)]
pub enum Neutral {
    /// Variables
    Var(Var<Name, Debruijn>), // 1.
    /// RawTerm application
    App(RcNeutral, RcTerm), // 2.
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

// Wrapper types

make_wrapper!(RcRawTerm, Rc, RawTerm);
make_wrapper!(RcTerm, Rc, Term);
make_wrapper!(RcValue, Rc, Value);
make_wrapper!(RcNeutral, Rc, Neutral);

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

impl From<Neutral> for RcValue {
    fn from(src: Neutral) -> RcValue {
        Value::Neutral(src.into()).into()
    }
}

impl<'a> From<&'a RcValue> for RcTerm {
    fn from(src: &'a RcValue) -> RcTerm {
        let meta = SourceMeta::default();

        match *src.inner {
            Value::Universe(level) => Term::Universe(meta, level).into(),
            Value::Lam(ref lam) => {
                let (param, body) = lam.clone().unbind();
                let param = Named::new(param.name.clone(), RcTerm::from(&param.inner));

                Term::Lam(meta, Scope::bind(param, RcTerm::from(&body))).into()
            },
            Value::Pi(ref pi) => {
                let (param, body) = pi.clone().unbind();
                let param = Named::new(param.name.clone(), RcTerm::from(&param.inner));

                Term::Pi(meta, Scope::bind(param, RcTerm::from(&body))).into()
            },
            Value::Neutral(ref n) => RcTerm::from(n),
        }
    }
}

impl<'a> From<&'a RcNeutral> for RcTerm {
    fn from(src: &'a RcNeutral) -> RcTerm {
        let meta = SourceMeta::default();

        match *src.inner {
            Neutral::Var(ref var) => Term::Var(meta, var.clone()).into(),
            Neutral::App(ref fn_expr, ref arg_expr) => {
                Term::App(meta, RcTerm::from(fn_expr), arg_expr.clone()).into()
            },
        }
    }
}

/// A binder that introduces a variable into the context
///
/// ```text
/// b ::= λx:V           1. lambda abstraction
///     | Πx:V           2. dependent function
///     | let x:V = t    3. let binding
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam { name: Name, ann: RcType }, // 1.
    /// A type introduced after entering a pi type
    Pi { name: Name, ann: RcType }, // 2.
    /// A value and type binding that was introduced by passing over a let binding
    Let {
        name: Name,
        ann: RcType,
        value: RcTerm,
    }, // 3.
}

impl Binder {
    pub fn span(&self) -> ByteSpan {
        // TODO: real span
        ByteSpan::default()
    }

    pub fn name(&self) -> &Name {
        match *self {
            Binder::Lam { ref name, .. }
            | Binder::Pi { ref name, .. }
            | Binder::Let { ref name, .. } => name,
        }
    }
}

/// A list of binders that have been accumulated during typechecking
///
/// ```text
/// Γ ::= ε           1. empty context
///     | Γ,b         2. context extension
/// ```
#[derive(Clone, PartialEq)]
pub struct Context {
    pub binders: List<Binder>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front(binder),
        }
    }

    pub fn extend_lam(&self, name: Name, ann: RcType) -> Context {
        self.extend(Binder::Lam { name, ann })
    }

    pub fn extend_pi(&self, name: Name, ann: RcType) -> Context {
        self.extend(Binder::Pi { name, ann })
    }

    pub fn extend_let(&self, name: Name, ann: RcType, value: RcTerm) -> Context {
        self.extend(Binder::Let { name, ann, value })
    }

    pub fn lookup_binder(&self, name: &Name) -> Option<&Binder> {
        self.binders.iter().find(|binder| binder.name() == name)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtBinders<'a>(&'a List<Binder>);

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
