//! The core syntax of the language

use nameless::{BoundPattern, Embed, FreeVar, Nest, Scope, Var};
use std::fmt;
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
    pub definitions: Nest<(FreeVar, Embed<Definition>)>,
}

/// A type checked and elaborated definition
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct Definition {
    /// The elaborated value
    pub term: Rc<Term>,
    /// The type of the definition
    pub ann: Rc<Type>,
}

/// The core term syntax
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(Rc<Term>, Rc<Term>),
    /// Universes
    Universe(Level),
    /// Literals
    Literal(Literal),
    /// A variable
    Var(Var),
    /// Dependent function types
    Pi(Scope<(FreeVar, Embed<Rc<Term>>), Rc<Term>>),
    /// Lambda abstractions
    Lam(Scope<(FreeVar, Embed<Rc<Term>>), Rc<Term>>),
    /// Term application
    App(Rc<Term>, Rc<Term>),
    /// If expression
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    /// Dependent record types
    RecordType(Scope<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// The unit type
    RecordTypeEmpty,
    /// Dependent record
    Record(Scope<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// The element of the unit type
    RecordEmpty,
    /// Field projection
    Proj(Rc<Term>, Label),
    /// Array literals
    Array(Vec<Rc<Term>>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
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
    Pi(Scope<(FreeVar, Embed<Rc<Value>>), Rc<Value>>),
    /// A lambda abstraction
    Lam(Scope<(FreeVar, Embed<Rc<Value>>), Rc<Value>>),
    /// Dependent record types
    RecordType(Scope<(Label, Embed<Rc<Value>>), Rc<Value>>),
    /// The unit type
    RecordTypeEmpty,
    /// Dependent record
    Record(Scope<(Label, Embed<Rc<Value>>), Rc<Value>>),
    /// The element of the unit type
    RecordEmpty,
    /// Array literals
    Array(Vec<Rc<Value>>),
    /// Neutral terms
    Neutral(Rc<Neutral>),
}

impl Value {
    pub fn record_ty(&self) -> Option<Scope<(Label, Embed<Rc<Value>>), Rc<Value>>> {
        match *self {
            Value::RecordType(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record_ty(&self, label: &Label) -> Option<Rc<Value>> {
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

    pub fn record(&self) -> Option<Scope<(Label, Embed<Rc<Value>>), Rc<Value>>> {
        match *self {
            Value::Record(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record(&self, label: &Label) -> Option<Rc<Value>> {
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

    pub fn free_app(&self) -> Option<(&FreeVar, &[Rc<Value>])> {
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

/// The head of an application
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Head {
    /// Variables that have not yet been replaced with a definition
    Var(Var),
    // TODO: Metavariables
}

/// The spine of a neutral term
///
/// These are arguments that are awaiting application
pub type Spine = Vec<Rc<Value>>;

/// Neutral terms
///
/// These might be able to be reduced further depending on the bindings in the
/// context
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Neutral {
    /// Term application
    App(Head, Spine),
    /// If expression
    If(Rc<Neutral>, Rc<Value>, Rc<Value>, Spine),
    /// Field projection
    Proj(Rc<Neutral>, Label, Spine),
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Types are at the term level, so this is just an alias
pub type Type = Value;

impl From<Var> for Neutral {
    fn from(src: Var) -> Neutral {
        Neutral::App(Head::Var(src), vec![])
    }
}

impl From<Var> for Value {
    fn from(src: Var) -> Value {
        Value::from(Neutral::from(src))
    }
}

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::Neutral(Rc::new(src))
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
                    unsafe_pattern: (name.clone(), Embed(Rc::new(Term::from(&**ann)))),
                    unsafe_body: Rc::new(Term::from(&*scope.unsafe_body)),
                })
            },
            Value::Lam(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Lam(Scope {
                    unsafe_pattern: (name.clone(), Embed(Rc::new(Term::from(&**ann)))),
                    unsafe_body: Rc::new(Term::from(&*scope.unsafe_body)),
                })
            },
            Value::RecordType(ref scope) => {
                let (ref label, Embed(ref ann)) = scope.unsafe_pattern;
                Term::RecordType(Scope {
                    unsafe_pattern: (label.clone(), Embed(Rc::new(Term::from(&**ann)))),
                    unsafe_body: Rc::new(Term::from(&*scope.unsafe_body)),
                })
            },
            Value::RecordTypeEmpty => Term::RecordTypeEmpty,
            Value::Record(ref scope) => {
                let (ref label, Embed(ref term)) = scope.unsafe_pattern;
                Term::Record(Scope {
                    unsafe_pattern: (label.clone(), Embed(Rc::new(Term::from(&**term)))),
                    unsafe_body: Rc::new(Term::from(&*scope.unsafe_body)),
                })
            },
            Value::RecordEmpty => Term::RecordEmpty,
            Value::Array(ref elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| Rc::new(Term::from(&**elem)))
                    .collect();

                Term::Array(elems)
            },
            Value::Neutral(ref n) => Term::from(&**n),
        }
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        let (head, spine) = match *src {
            Neutral::App(ref head, ref spine) => (Term::from(head), spine),
            Neutral::If(ref cond, ref if_true, ref if_false, ref spine) => {
                let head = Term::If(
                    Rc::new(Term::from(&**cond)),
                    Rc::new(Term::from(&**if_true)),
                    Rc::new(Term::from(&**if_false)),
                );
                (head, spine)
            },
            Neutral::Proj(ref expr, ref name, ref spine) => {
                let head = Term::Proj(Rc::new(Term::from(&**expr)), name.clone());
                (head, spine)
            },
        };

        spine.iter().fold(head, |acc, arg| {
            Term::App(Rc::new(acc), Rc::new(Term::from(&**arg)))
        })
    }
}

impl<'a> From<&'a Head> for Term {
    fn from(src: &'a Head) -> Term {
        match *src {
            Head::Var(ref var) => Term::Var(var.clone()),
        }
    }
}
