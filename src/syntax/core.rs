//! The core syntax of the language

use moniker::{BoundPattern, Embed, FreeVar, Nest, Scope, Var};
use std::fmt;
use std::ops;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::{Label, Level};

/// Literals
///
/// We could church encode all the things, but that would be prohibitively expensive!
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm)]
pub enum Literal {
    Bool(bool),
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
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// A type checked and elaborated module
pub struct Module {
    /// The definitions contained in the module
    pub definitions: Nest<(FreeVar<String>, Embed<Definition>)>,
}

/// A type checked and elaborated definition
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct Definition {
    /// The elaborated value
    pub term: RcTerm,
    /// The type of the definition
    pub ann: RcType,
}

/// The core term syntax
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(Level),
    /// Literals
    Literal(Literal),
    /// A variable
    Var(Var<String>),
    /// Dependent function types
    Pi(Scope<(FreeVar<String>, Embed<RcTerm>), RcTerm>),
    /// Lambda abstractions
    Lam(Scope<(FreeVar<String>, Embed<RcTerm>), RcTerm>),
    /// Term application
    App(RcTerm, RcTerm),
    /// If expression
    If(RcTerm, RcTerm, RcTerm),
    /// Dependent record types
    RecordType(Scope<(Label<String>, Embed<RcTerm>), RcTerm>),
    /// The unit type
    RecordTypeEmpty,
    /// Dependent record
    Record(Scope<(Label<String>, Embed<RcTerm>), RcTerm>),
    /// The element of the unit type
    RecordEmpty,
    /// Field projection
    Proj(RcTerm, Label<String>),
    /// Array literals
    Array(Vec<RcTerm>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted terms
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcTerm {
    pub inner: Rc<Term>,
}

impl RcTerm {
    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        match *self.inner {
            Term::Ann(ref term, ref ty) => {
                RcTerm::from(Term::Ann(term.substs(mappings), ty.substs(mappings)))
            },
            Term::Var(Var::Free(ref name)) => match mappings.iter().find(|s| *name == s.0) {
                Some(&(_, ref term)) => term.clone(),
                None => self.clone(),
            },
            Term::Var(_) | Term::Universe(_) | Term::Literal(_) => self.clone(),
            Term::Pi(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::Pi(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::Lam(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::Lam(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::App(ref term, ref arg) => {
                RcTerm::from(Term::App(term.substs(mappings), arg.substs(mappings)))
            },
            Term::If(ref cond, ref if_true, ref if_false) => RcTerm::from(Term::If(
                cond.substs(mappings),
                if_true.substs(mappings),
                if_false.substs(mappings),
            )),
            Term::RecordType(ref scope) => {
                let (ref label, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::RecordType(Scope {
                    unsafe_pattern: (label.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::Record(ref scope) => {
                let (ref label, Embed(ref expr)) = scope.unsafe_pattern;
                RcTerm::from(Term::Record(Scope {
                    unsafe_pattern: (label.clone(), Embed(expr.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::RecordTypeEmpty | Term::RecordEmpty => self.clone(),
            Term::Proj(ref expr, ref label) => {
                RcTerm::from(Term::Proj(expr.substs(mappings), label.clone()))
            },
            Term::Array(ref elems) => RcTerm::from(Term::Array(
                elems.iter().map(|elem| elem.substs(mappings)).collect(),
            )),
        }
    }
}

impl From<Term> for RcTerm {
    fn from(src: Term) -> RcTerm {
        RcTerm {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcTerm {
    type Target = Term;

    fn deref(&self) -> &Term {
        &self.inner
    }
}

impl fmt::Display for RcTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// Values
///
/// These are either in _normal form_ (they cannot be reduced further) or are
/// _neutral terms_ (there is a possibility of reducing further depending
/// on the bindings given in the context)
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Value {
    /// Universes
    Universe(Level),
    /// Literals
    Literal(Literal),
    /// A pi type
    Pi(Scope<(FreeVar<String>, Embed<RcValue>), RcValue>),
    /// A lambda abstraction
    Lam(Scope<(FreeVar<String>, Embed<RcValue>), RcValue>),
    /// Dependent record types
    RecordType(Scope<(Label<String>, Embed<RcValue>), RcValue>),
    /// The unit type
    RecordTypeEmpty,
    /// Dependent record
    Record(Scope<(Label<String>, Embed<RcValue>), RcValue>),
    /// The element of the unit type
    RecordEmpty,
    /// Array literals
    Array(Vec<RcValue>),
    /// Neutral terms
    Neutral(RcNeutral),
}

impl Value {
    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        // FIXME: This seems quite wasteful!
        RcTerm::from(Term::from(self)).substs(mappings)
    }

    pub fn record_ty(&self) -> Option<Scope<(Label<String>, Embed<RcValue>), RcValue>> {
        match *self {
            Value::RecordType(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record_ty(&self, label: &Label<String>) -> Option<RcValue> {
        let mut current_scope = self.record_ty();

        while let Some(scope) = current_scope {
            let ((current_label, Embed(value)), body) = scope.unbind();
            if Label::pattern_eq(&current_label, label) {
                return Some(value);
            }
            current_scope = body.record_ty();
        }

        None
    }

    pub fn record(&self) -> Option<Scope<(Label<String>, Embed<RcValue>), RcValue>> {
        match *self {
            Value::Record(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record(&self, label: &Label<String>) -> Option<RcValue> {
        let mut current_scope = self.record();

        while let Some(scope) = current_scope {
            let ((current_label, Embed(value)), body) = scope.unbind();
            if Label::pattern_eq(&current_label, label) {
                return Some(value);
            }
            current_scope = body.record();
        }

        None
    }

    /// Returns `true` if the value is in weak head normal form
    pub fn is_whnf(&self) -> bool {
        match *self {
            Value::Universe(_)
            | Value::Literal(_)
            | Value::Pi(_)
            | Value::Lam(_)
            | Value::RecordType(_)
            | Value::RecordTypeEmpty
            | Value::Record(_)
            | Value::RecordEmpty
            | Value::Array(_) => true,
            Value::Neutral(_) => false,
        }
    }

    /// Returns `true` if the value is in normal form (ie. it contains no neutral terms within it)
    pub fn is_nf(&self) -> bool {
        match *self {
            Value::Universe(_)
            | Value::Literal(_)
            | Value::RecordTypeEmpty
            | Value::RecordEmpty => true,
            Value::Pi(ref scope) | Value::Lam(ref scope) => {
                (scope.unsafe_pattern.1).0.is_nf() && scope.unsafe_body.is_nf()
            },
            Value::RecordType(ref scope) | Value::Record(ref scope) => {
                (scope.unsafe_pattern.1).0.is_nf() && scope.unsafe_body.is_nf()
            },
            Value::Array(ref elems) => elems.iter().all(|elem| elem.is_nf()),
            Value::Neutral(_) => false,
        }
    }

    pub fn free_app(&self) -> Option<(&FreeVar<String>, &[RcValue])> {
        if let Value::Neutral(ref neutral) = *self {
            if let Neutral::App(Head::Var(Var::Free(ref name)), ref spine) = **neutral {
                return Some((name, spine));
            }
        }
        None
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted values
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcValue {
    pub inner: Rc<Value>,
}

impl From<Value> for RcValue {
    fn from(src: Value) -> RcValue {
        RcValue {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcValue {
    type Target = Value;

    fn deref(&self) -> &Value {
        &self.inner
    }
}

impl fmt::Display for RcValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// The head of an application
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Head {
    /// Variables that have not yet been replaced with a definition
    Var(Var<String>),
    // TODO: Metavariables
}

/// The spine of a neutral term
///
/// These are arguments that are awaiting application
pub type Spine = Vec<RcValue>;

/// Neutral values
///
/// These might be able to be reduced further depending on the bindings in the
/// context
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Neutral {
    /// Term application
    App(Head, Spine),
    /// If expression
    If(RcNeutral, RcValue, RcValue, Spine),
    /// Field projection
    Proj(RcNeutral, Label<String>, Spine),
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted neutral values
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcNeutral {
    pub inner: Rc<Neutral>,
}

impl From<Neutral> for RcNeutral {
    fn from(src: Neutral) -> RcNeutral {
        RcNeutral {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcNeutral {
    type Target = Neutral;

    fn deref(&self) -> &Neutral {
        &self.inner
    }
}

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

impl From<Var<String>> for Neutral {
    fn from(src: Var<String>) -> Neutral {
        Neutral::App(Head::Var(src), vec![])
    }
}

impl From<Var<String>> for Value {
    fn from(src: Var<String>) -> Value {
        Value::from(Neutral::from(src))
    }
}

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::Neutral(RcNeutral::from(src))
    }
}

impl<'a> From<&'a Value> for Term {
    fn from(src: &'a Value) -> Term {
        // Bypassing `Scope::new` and `Scope::unbind` here should be fine
        // because we aren't altering the structure of the scopes during this
        // transformation. This should save on some traversals of the AST!
        match *src {
            Value::Universe(level) => Term::Universe(level),
            Value::Literal(ref lit) => Term::Literal(lit.clone()),
            Value::Pi(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Pi(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::Lam(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Lam(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::RecordType(ref scope) => {
                let (ref label, Embed(ref ann)) = scope.unsafe_pattern;
                Term::RecordType(Scope {
                    unsafe_pattern: (label.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::RecordTypeEmpty => Term::RecordTypeEmpty,
            Value::Record(ref scope) => {
                let (ref label, Embed(ref term)) = scope.unsafe_pattern;
                Term::Record(Scope {
                    unsafe_pattern: (label.clone(), Embed(RcTerm::from(&**term))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::RecordEmpty => Term::RecordEmpty,
            Value::Array(ref elems) => {
                Term::Array(elems.iter().map(|elem| RcTerm::from(&**elem)).collect())
            },
            Value::Neutral(ref n) => Term::from(&**n),
        }
    }
}

impl<'a> From<&'a Value> for RcTerm {
    fn from(src: &'a Value) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        let (head, spine) = match *src {
            Neutral::App(ref head, ref spine) => (Term::from(head), spine),
            Neutral::If(ref cond, ref if_true, ref if_false, ref spine) => (
                Term::If(
                    RcTerm::from(&**cond),
                    RcTerm::from(&**if_true),
                    RcTerm::from(&**if_false),
                ),
                spine,
            ),
            Neutral::Proj(ref expr, ref name, ref spine) => {
                (Term::Proj(RcTerm::from(&**expr), name.clone()), spine)
            },
        };

        spine.iter().fold(head, |acc, arg| {
            Term::App(RcTerm::from(acc), RcTerm::from(&**arg))
        })
    }
}

impl<'a> From<&'a Neutral> for RcTerm {
    fn from(src: &'a Neutral) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Head> for Term {
    fn from(src: &'a Head) -> Term {
        match *src {
            Head::Var(ref var) => Term::Var(var.clone()),
        }
    }
}
