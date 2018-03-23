//! The core syntax of the language

use codespan::ByteSpan;
use nameless::{self, BoundTerm, Embed, Scope, Var};
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
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourceMeta {
    pub span: ByteSpan,
}

impl BoundTerm for SourceMeta {
    type Free = Name;

    fn term_eq(&self, _: &SourceMeta) -> bool {
        true
    }
}

impl Default for SourceMeta {
    fn default() -> SourceMeta {
        SourceMeta {
            span: ByteSpan::default(),
        }
    }
}

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(pub u32);

impl Level {
    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl BoundTerm for Level {
    type Free = Name;

    fn term_eq(&self, other: &Level) -> bool {
        self == other
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw primitive constants
///
/// These are either the literal values or the types that describe them.
///
/// We could church encode all the things, but that would be prohibitively
/// expensive computationally!
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RawConstant {
    String(String),
    Char(char),
    Int(u64),
    Float(f64),
    StringType,
    CharType,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    I8Type,
    I16Type,
    I32Type,
    I64Type,
    F32Type,
    F64Type,
}

impl BoundTerm for RawConstant {
    type Free = Name;

    fn term_eq(&self, other: &RawConstant) -> bool {
        self == other
    }
}

/// Primitive constants
///
/// These are either the literal values or the types that describe them.
///
/// We could church encode all the things, but that would be prohibitively
/// expensive computationally!
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Constant {
    String(String),
    Char(char),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    StringType,
    CharType,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    I8Type,
    I16Type,
    I32Type,
    I64Type,
    F32Type,
    F64Type,
}

// TODO: Derive this
impl BoundTerm for Constant {
    type Free = Name;

    fn term_eq(&self, other: &Constant) -> bool {
        self == other
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
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
///
/// ```text
/// r,R ::= r:R         1. annotated terms
///       | Typeᵢ       2. universes
///       | c           3. constants
///       | _           4. hole
///       | x           5. variables
///       | Πx:R₁.R₂    6. dependent function types
///       | λx:R.r      7. lambda abstractions
///       | R₁ R₂       8. term application
/// ```
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(SourceMeta, RcRawTerm, RcRawTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// Constants
    Constant(SourceMeta, RawConstant), // 3.
    /// A hole
    Hole(SourceMeta), // 4.
    /// A variable
    Var(SourceMeta, Var<Name>), // 5.
    /// Dependent function types
    Pi(SourceMeta, Scope<(Name, Embed<RcRawTerm>), RcRawTerm>), // 6.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<(Name, Embed<RcRawTerm>), RcRawTerm>), // 7.
    /// RawTerm application
    App(SourceMeta, RcRawTerm, RcRawTerm), // 8.
}

impl RcRawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            RawTerm::Ann(meta, _, _)
            | RawTerm::Universe(meta, _)
            | RawTerm::Hole(meta)
            | RawTerm::Constant(meta, _)
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
    fn visit_vars<F: FnMut(&Var<Name>)>(&self, on_var: &mut F) {
        match *self.inner {
            RawTerm::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            RawTerm::Universe(_, _) | RawTerm::Hole(_) | RawTerm::Constant(_, _) => {},
            RawTerm::Var(_, ref var) => on_var(var),
            RawTerm::Pi(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
            },
            RawTerm::Lam(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
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
            Var::Bound(_, _) => {},
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
///       | c           3. constants
///       | x           4. variables
///       | Πx:T₁.T₂    5. dependent function types
///       | λx:T.t      6. lambda abstractions
///       | t₁ t₂       7. term application
/// ```
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(SourceMeta, RcTerm, RcTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// Constants
    Constant(SourceMeta, Constant), // 3.
    /// A variable
    Var(SourceMeta, Var<Name>), // 4.
    /// Dependent function types
    Pi(SourceMeta, Scope<(Name, Embed<RcTerm>), RcTerm>), // 5.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<(Name, Embed<RcTerm>), RcTerm>), // 6.
    /// Term application
    App(SourceMeta, RcTerm, RcTerm), // 7.
}

impl RcTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            Term::Ann(meta, _, _)
            | Term::Universe(meta, _)
            | Term::Constant(meta, _)
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
/// These are either in _weak head normal form_ (they cannot be reduced further)
/// or are _neutral terms_ (there is a possibility of reducing further depending
/// on the bindings given in the context)
///
/// ```text
/// v,V ::= Typeᵢ       1. universes
///       | c           2. constants
///       | Πx:V₁.V₂    3. dependent function types
///       | λx:V.v      4. lambda abstractions
///       | n           5. neutral terms
/// ```
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Value {
    /// Universes
    Universe(Level), // 1.
    /// Constants
    Constant(Constant), // 2.
    /// A pi type
    Pi(Scope<(Name, Embed<RcValue>), RcValue>), // 3.
    /// A lambda abstraction
    Lam(Scope<(Name, Embed<RcValue>), RcValue>), // 4.
    /// Neutral terms
    Neutral(RcNeutral), // 5.
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
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Neutral {
    /// Variables
    Var(Var<Name>), // 1.
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
            Value::Constant(ref c) => Term::Constant(meta, c.clone()).into(),
            Value::Pi(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(RcTerm::from(&param_ann)));

                Term::Pi(meta, Scope::bind(param, RcTerm::from(&body))).into()
            },
            Value::Lam(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(RcTerm::from(&param_ann)));

                Term::Lam(meta, Scope::bind(param, RcTerm::from(&body))).into()
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

impl Default for Context {
    fn default() -> Context {
        Context::new()
            .extend_let(
                Name::user("String"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::StringType).into(),
            )
            .extend_let(
                Name::user("Char"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::CharType).into(),
            )
            .extend_let(
                Name::user("U8"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::U8Type).into(),
            )
            .extend_let(
                Name::user("U16"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::U16Type).into(),
            )
            .extend_let(
                Name::user("U32"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::U32Type).into(),
            )
            .extend_let(
                Name::user("U64"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::U64Type).into(),
            )
            .extend_let(
                Name::user("I8"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::I8Type).into(),
            )
            .extend_let(
                Name::user("I16"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::I16Type).into(),
            )
            .extend_let(
                Name::user("I32"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::I32Type).into(),
            )
            .extend_let(
                Name::user("I64"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::I64Type).into(),
            )
            .extend_let(
                Name::user("F32"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::F32Type).into(),
            )
            .extend_let(
                Name::user("F64"),
                Value::Universe(Level(0)).into(),
                Term::Constant(SourceMeta::default(), Constant::F64Type).into(),
            )
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
