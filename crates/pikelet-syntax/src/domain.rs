//! The semantic domain of the language

use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};
use std::ops;
use std::rc::Rc;

use core::{Literal, RcPattern, RcTerm, Term};
use {Label, Level, LevelShift};

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
    /// Dependent function types
    FunType(Scope<(Binder<String>, Embed<RcValue>), RcValue>),
    /// Function introductions
    FunIntro(Scope<(Binder<String>, Embed<RcValue>), RcValue>),
    /// Dependent record types
    RecordType(Scope<Nest<(Label, Binder<String>, Embed<RcValue>)>, ()>),
    /// Dependent record introductions
    RecordIntro(Scope<Nest<(Label, Binder<String>, Embed<RcValue>)>, ()>),
    /// Array literals
    ArrayIntro(Vec<RcValue>),
    /// Neutral terms
    ///
    /// A term whose computation has stopped because of an attempt to compute an
    /// application `Head`.
    Neutral(RcNeutral, Spine),
}

impl Value {
    pub fn universe(level: impl Into<Level>) -> Value {
        Value::Universe(level.into())
    }

    pub fn var(var: impl Into<Var<String>>, shift: impl Into<LevelShift>) -> Value {
        Value::Neutral(RcNeutral::from(Neutral::var(var, shift)), Spine::new())
    }

    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        // FIXME: This seems quite wasteful!
        RcTerm::from(Term::from(self)).substs(mappings)
    }

    /// Returns `true` if the value is in weak head normal form
    pub fn is_whnf(&self) -> bool {
        match *self {
            Value::Universe(_)
            | Value::Literal(_)
            | Value::FunType(_)
            | Value::FunIntro(_)
            | Value::RecordType(_)
            | Value::RecordIntro(_)
            | Value::ArrayIntro(_) => true,
            Value::Neutral(_, _) => false,
        }
    }

    /// Returns `true` if the value is in normal form (ie. it contains no neutral terms within it)
    pub fn is_nf(&self) -> bool {
        match *self {
            Value::Universe(_) | Value::Literal(_) => true,
            Value::FunType(ref scope) | Value::FunIntro(ref scope) => {
                (scope.unsafe_pattern.1).0.is_nf() && scope.unsafe_body.is_nf()
            },
            Value::RecordType(ref scope) | Value::RecordIntro(ref scope) => scope
                .unsafe_pattern
                .unsafe_patterns
                .iter()
                .all(|(_, _, Embed(ref term))| term.is_nf()),
            Value::ArrayIntro(ref elems) => elems.iter().all(|elem| elem.is_nf()),
            Value::Neutral(_, _) => false,
        }
    }

    pub fn head_app(&self) -> Option<(&Head, &Spine)> {
        if let Value::Neutral(ref neutral, ref spine) = *self {
            if let Neutral::Head(ref head) = **neutral {
                return Some((head, spine));
            }
        }
        None
    }

    pub fn free_var_app(&self) -> Option<(&FreeVar<String>, LevelShift, &[RcValue])> {
        self.head_app().and_then(|(head, spine)| match *head {
            Head::Var(Var::Free(ref free_var), shift) => Some((free_var, shift, &spine[..])),
            Head::Import(_) | Head::Var(Var::Bound(_), _) => None,
        })
    }
}

/// Reference counted values
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcValue {
    pub inner: Rc<Value>,
}

impl RcValue {
    pub fn shift_universes(&mut self, shift: LevelShift) {
        match *Rc::make_mut(&mut self.inner) {
            Value::Universe(ref mut level) => *level += shift,
            Value::Literal(_) => {},
            Value::FunType(ref mut scope) | Value::FunIntro(ref mut scope) => {
                (scope.unsafe_pattern.1).0.shift_universes(shift);
                scope.unsafe_body.shift_universes(shift);
            },
            Value::RecordType(ref mut scope) | Value::RecordIntro(ref mut scope) => {
                for &mut (_, _, Embed(ref mut term)) in &mut scope.unsafe_pattern.unsafe_patterns {
                    term.shift_universes(shift);
                }
            },
            Value::ArrayIntro(ref mut elems) => {
                for elem in elems {
                    elem.shift_universes(shift);
                }
            },
            Value::Neutral(ref mut neutral, ref mut spine) => {
                neutral.shift_universes(shift);
                for arg in spine {
                    arg.shift_universes(shift);
                }
            },
        }
    }
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

/// The head of an application
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Head {
    /// Variables that have not yet been replaced with a definition
    Var(Var<String>, LevelShift),
    /// Imported definitions
    Import(String),
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
    /// Head of an application
    Head(Head),
    /// Field projection
    RecordProj(RcNeutral, Label, LevelShift),
    /// Case expressions
    Case(RcNeutral, Vec<Scope<RcPattern, RcValue>>),
}

impl Neutral {
    pub fn var(var: impl Into<Var<String>>, shift: impl Into<LevelShift>) -> Neutral {
        Neutral::Head(Head::Var(var.into(), shift.into()))
    }
}

/// Reference counted neutral values
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcNeutral {
    pub inner: Rc<Neutral>,
}

impl RcNeutral {
    pub fn shift_universes(&mut self, shift: LevelShift) {
        match *Rc::make_mut(&mut self.inner) {
            // Neutral::Head(Head::Var(_, ref mut head_shift)) => {
            //     *head_shift += shift; // NOTE: Not sure if this is correct!
            // },
            Neutral::Head(Head::Var(_, _)) | Neutral::Head(Head::Import(_)) => {},
            Neutral::RecordProj(ref mut expr, _, _) => expr.shift_universes(shift),
            Neutral::Case(ref mut expr, ref mut clauses) => {
                expr.shift_universes(shift);
                for clause in clauses {
                    // FIXME: implement shifting for patterns as well!
                    // clause.unsafe_pattern.shift_universes(shift);
                    clause.unsafe_body.shift_universes(shift);
                }
            },
        }
    }
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

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::Neutral(RcNeutral::from(src), Spine::new())
    }
}
