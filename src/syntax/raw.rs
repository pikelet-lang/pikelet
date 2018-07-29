//! The syntax of the language, unchecked and with implicit parts that need to
//! be elaborated in a type-directed way during type checking and inference

use codespan::{ByteIndex, ByteSpan};
use moniker::{Binder, Embed, Nest, Scope, Var};
use std::fmt;
use std::ops;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::{Label, Level};

/// A module definition
pub struct Module {
    /// The definitions contained in the module
    pub definitions: Nest<(Binder<String>, Embed<Definition>)>,
}

/// Top level definitions
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct Definition {
    /// The body of the definition
    pub term: RcTerm,
    /// An optional type annotation to aid in type inference
    pub ann: RcTerm,
}

/// Literals
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm, BoundPattern)]
pub enum Literal {
    String(ByteSpan, String),
    Char(ByteSpan, char),
    Int(ByteSpan, u64),
    Float(ByteSpan, f64),
}

impl Literal {
    /// Return the span of source code that the literal originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Literal::String(span, _)
            | Literal::Char(span, _)
            | Literal::Int(span, _)
            | Literal::Float(span, _) => span,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub enum Pattern {
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcTerm>),
    /// Patterns that bind variables
    Binder(ByteSpan, Binder<String>),
    /// Literal patterns
    Literal(Literal),
}

impl Pattern {
    /// Return the span of source code that this pattern originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => pattern.span().to(ty.span()),
            Pattern::Binder(span, _) => span,
            Pattern::Literal(ref literal) => literal.span(),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted patterns
#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub struct RcPattern {
    pub inner: Rc<Pattern>,
}

impl From<Pattern> for RcPattern {
    fn from(src: Pattern) -> RcPattern {
        RcPattern {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcPattern {
    type Target = Pattern;

    fn deref(&self) -> &Pattern {
        &self.inner
    }
}

impl fmt::Display for RcPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// Terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(ByteSpan, Level),
    /// Literals
    Literal(Literal),
    /// A hole
    Hole(ByteSpan),
    /// A variable
    Var(ByteSpan, Var<String>),
    /// Dependent function types
    Pi(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Lambda abstractions
    Lam(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Term application
    App(RcTerm, RcTerm),
    /// If expression
    If(ByteIndex, RcTerm, RcTerm, RcTerm),
    /// Dependent record types
    RecordType(ByteSpan, Scope<(Label<String>, Embed<RcTerm>), RcTerm>),
    /// Dependent record
    Record(ByteSpan, Scope<(Label<String>, Embed<RcTerm>), RcTerm>),
    /// The unit type
    RecordTypeEmpty(ByteSpan),
    /// The element of the unit type
    RecordEmpty(ByteSpan),
    /// Field projection
    Proj(ByteSpan, RcTerm, ByteSpan, String),
    /// Case expressions
    Case(ByteSpan, RcTerm, Vec<Scope<RcPattern, RcTerm>>),
    /// Array literals
    Array(ByteSpan, Vec<RcTerm>),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Universe(span, _)
            | Term::Hole(span)
            | Term::Var(span, _)
            | Term::Pi(span, _)
            | Term::Lam(span, _)
            | Term::RecordType(span, _)
            | Term::Record(span, _)
            | Term::RecordTypeEmpty(span)
            | Term::RecordEmpty(span)
            | Term::Proj(span, _, _, _)
            | Term::Case(span, _, _)
            | Term::Array(span, _) => span,
            Term::Literal(ref literal) => literal.span(),
            Term::Ann(ref expr, ref ty) => expr.span().to(ty.span()),
            Term::App(ref head, ref arg) => head.span().to(arg.span()),
            Term::If(start, _, _, ref if_false) => ByteSpan::new(start, if_false.span().end()),
        }
    }
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
