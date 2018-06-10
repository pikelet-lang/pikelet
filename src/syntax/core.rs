//! The core syntax of the language

use codespan::{ByteIndex, ByteSpan};
use nameless::{
    self, Bind, BoundName, BoundPattern, BoundTerm, Embed, Ignore, Name, ScopeState, Var,
};
use std::fmt;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, BoundTerm)]
pub struct Level(pub u32);

impl Level {
    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw literals
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm)]
pub enum RawLiteral {
    String(String),
    Char(char),
    Int(u64),
    Float(f64),
}

impl fmt::Display for RawLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

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

/// A module definition
pub struct RawModule {
    /// The definitions contained in the module
    pub definitions: Vec<RawDefinition>,
}

impl fmt::Display for RawModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Top level definitions
pub struct RawDefinition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: Rc<RawTerm>,
    /// An optional type annotation to aid in type inference
    pub ann: Rc<RawTerm>,
}

impl fmt::Display for RawDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// A record label
///
/// Labels are significant when comparing for alpha-equality, both in terms and
/// in patterns
#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub Name);

impl BoundTerm for Label {
    fn term_eq(&self, other: &Label) -> bool {
        match (self.0.name(), other.0.name()) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            (_, _) => Name::term_eq(&self.0, &other.0),
        }
    }
}

impl BoundPattern for Label {
    fn pattern_eq(&self, other: &Label) -> bool {
        Label::term_eq(self, other)
    }

    fn freshen(&mut self) -> Vec<Name> {
        self.0.freshen()
    }

    fn rename(&mut self, perm: &[Name]) {
        self.0.rename(perm)
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<BoundName> {
        self.0.on_free(state, name)
    }

    fn on_bound(&self, state: ScopeState, name: BoundName) -> Option<Name> {
        self.0.on_bound(state, name)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Raw terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(Ignore<ByteSpan>, Rc<RawTerm>, Rc<RawTerm>),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Literals
    Literal(Ignore<ByteSpan>, RawLiteral),
    /// A hole
    Hole(Ignore<ByteSpan>),
    /// A variable
    Var(Ignore<ByteSpan>, Var),
    /// Dependent function types
    Pi(
        Ignore<ByteSpan>,
        Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// Lambda abstractions
    Lam(
        Ignore<ByteSpan>,
        Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// Term application
    App(Rc<RawTerm>, Rc<RawTerm>),
    /// If expression
    If(Ignore<ByteIndex>, Rc<RawTerm>, Rc<RawTerm>, Rc<RawTerm>),
    /// Dependent record types
    RecordType(
        Ignore<ByteSpan>,
        Bind<(Label, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// Dependent record
    Record(
        Ignore<ByteSpan>,
        Bind<(Label, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// The unit type
    RecordTypeEmpty(Ignore<ByteSpan>),
    /// The element of the unit type
    RecordEmpty(Ignore<ByteSpan>),
    /// Field projection
    Proj(Ignore<ByteSpan>, Rc<RawTerm>, Ignore<ByteSpan>, Label),
    /// Array literals
    Array(Ignore<ByteSpan>, Vec<Rc<RawTerm>>),
}

impl RawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self {
            RawTerm::Ann(span, _, _)
            | RawTerm::Universe(span, _)
            | RawTerm::Hole(span)
            | RawTerm::Literal(span, _)
            | RawTerm::Var(span, _)
            | RawTerm::Pi(span, _)
            | RawTerm::Lam(span, _)
            | RawTerm::RecordType(span, _)
            | RawTerm::Record(span, _)
            | RawTerm::RecordTypeEmpty(span)
            | RawTerm::RecordEmpty(span)
            | RawTerm::Proj(span, _, _, _)
            | RawTerm::Array(span, _) => span.0,
            RawTerm::App(ref fn_term, ref arg) => fn_term.span().to(arg.span()),
            RawTerm::If(start, _, _, ref if_false) => ByteSpan::new(start.0, if_false.span().end()),
        }
    }
}

impl fmt::Display for RawTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// A type checked and elaborated module
pub struct Module {
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

/// A type checked and elaborated definition
pub struct Definition {
    /// The name of the definition
    pub name: String,
    /// The elaborated value
    pub term: Rc<Term>,
    /// The type of the definition
    pub ann: Rc<Type>,
}

/// The core term syntax
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(Ignore<ByteSpan>, Rc<Term>, Rc<Term>),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Literals
    Literal(Ignore<ByteSpan>, Literal),
    /// A variable
    Var(Ignore<ByteSpan>, Var),
    /// Dependent function types
    Pi(Ignore<ByteSpan>, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Lambda abstractions
    Lam(Ignore<ByteSpan>, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Term application
    App(Rc<Term>, Rc<Term>),
    /// If expression
    If(Ignore<ByteIndex>, Rc<Term>, Rc<Term>, Rc<Term>),
    /// Dependent record types
    RecordType(Ignore<ByteSpan>, Bind<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// The unit type
    RecordTypeEmpty(Ignore<ByteSpan>),
    /// Dependent record
    Record(Ignore<ByteSpan>, Bind<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// The element of the unit type
    RecordEmpty(Ignore<ByteSpan>),
    /// Field projection
    Proj(Ignore<ByteSpan>, Rc<Term>, Ignore<ByteSpan>, Label),
    /// Array literals
    Array(Ignore<ByteSpan>, Vec<Rc<Term>>),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Ann(span, _, _)
            | Term::Universe(span, _)
            | Term::Literal(span, _)
            | Term::Var(span, _)
            | Term::Lam(span, _)
            | Term::Pi(span, _)
            | Term::RecordType(span, _)
            | Term::RecordTypeEmpty(span)
            | Term::Record(span, _)
            | Term::RecordEmpty(span)
            | Term::Proj(span, _, _, _)
            | Term::Array(span, _) => span.0,
            Term::App(ref fn_term, ref arg) => fn_term.span().to(arg.span()),
            Term::If(start, _, _, ref if_false) => ByteSpan::new(start.0, if_false.span().end()),
        }
    }
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
    Pi(Bind<(Name, Embed<Rc<Value>>), Rc<Value>>),
    /// A lambda abstraction
    Lam(Bind<(Name, Embed<Rc<Value>>), Rc<Value>>),
    /// Dependent record types
    RecordType(Bind<(Label, Embed<Rc<Value>>), Rc<Value>>),
    /// The unit type
    RecordTypeEmpty,
    /// Dependent record
    Record(Bind<(Label, Embed<Rc<Value>>), Rc<Value>>),
    /// The element of the unit type
    RecordEmpty,
    /// Array literals
    Array(Vec<Rc<Value>>),
    /// Neutral terms
    Neutral(Rc<Neutral>),
}

impl Value {
    pub fn record_ty(&self) -> Option<Bind<(Label, Embed<Rc<Value>>), Rc<Value>>> {
        match *self {
            Value::RecordType(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record_ty(&self, label: &Label) -> Option<Rc<Value>> {
        let mut current_scope = self.record_ty();

        while let Some(scope) = current_scope {
            let ((current_label, Embed(value)), body) = nameless::unbind(scope);
            if Label::pattern_eq(&current_label, label) {
                return Some(value);
            }
            current_scope = body.record_ty();
        }

        None
    }

    pub fn record(&self) -> Option<Bind<(Label, Embed<Rc<Value>>), Rc<Value>>> {
        match *self {
            Value::Record(ref scope) => Some(scope.clone()),
            _ => None,
        }
    }

    pub fn lookup_record(&self, label: &Label) -> Option<Rc<Value>> {
        let mut current_scope = self.record();

        while let Some(scope) = current_scope {
            let ((current_label, Embed(value)), body) = nameless::unbind(scope);
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

    pub fn free_app(&self) -> Option<(&Name, &[Rc<Value>])> {
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
        match *src {
            Value::Universe(level) => Term::Universe(Ignore::default(), level),
            Value::Literal(ref lit) => Term::Literal(Ignore::default(), lit.clone()),
            Value::Pi(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Pi(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::Lam(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Lam(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::RecordType(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::RecordType(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::RecordTypeEmpty => Term::RecordTypeEmpty(Ignore::default()).into(),
            Value::Record(ref scope) => {
                let ((name, Embed(param_value)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_value))));

                Term::Record(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::RecordEmpty => Term::RecordEmpty(Ignore::default()).into(),
            Value::Array(ref elems) => Term::Array(
                Ignore::default(),
                elems
                    .iter()
                    .map(|elem| Rc::new(Term::from(&**elem)))
                    .collect(),
            ).into(),
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
                    Ignore::default(),
                    Rc::new(Term::from(&**cond)),
                    Rc::new(Term::from(&**if_true)),
                    Rc::new(Term::from(&**if_false)),
                );
                (head, spine)
            },
            Neutral::Proj(ref expr, ref name, ref spine) => {
                let head = Term::Proj(
                    Ignore::default(),
                    Rc::new(Term::from(&**expr)),
                    Ignore::default(),
                    name.clone(),
                );
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
            Head::Var(ref var) => Term::Var(Ignore::default(), var.clone()),
        }
    }
}
