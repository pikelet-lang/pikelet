//! The syntax of the language, unchecked and with implicit parts that need to
//! be elaborated in a type-directed way during type checking and inference

use codespan::{ByteIndex, ByteSpan};
use moniker::{Embed, FreeVar, Ignore, Nest, Scope, Var};
use std::fmt;
use std::ops;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::{Label, Level};

/// A module definition
pub struct Module {
    /// The definitions contained in the module
    pub definitions: Nest<(FreeVar<String>, Embed<Definition>)>,
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
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm)]
pub enum Literal {
    String(String),
    Char(char),
    Int(u64),
    Float(f64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(Ignore<ByteSpan>, RcTerm, RcTerm),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Literals
    Literal(Ignore<ByteSpan>, Literal),
    /// A hole
    Hole(Ignore<ByteSpan>),
    /// A variable
    Var(Ignore<ByteSpan>, Var<String>),
    /// Dependent function types
    Pi(
        Ignore<ByteSpan>,
        Scope<(FreeVar<String>, Embed<RcTerm>), RcTerm>,
    ),
    /// Lambda abstractions
    Lam(
        Ignore<ByteSpan>,
        Scope<(FreeVar<String>, Embed<RcTerm>), RcTerm>,
    ),
    /// Term application
    App(RcTerm, RcTerm),
    /// If expression
    If(Ignore<ByteIndex>, RcTerm, RcTerm, RcTerm),
    /// Dependent record types
    RecordType(
        Ignore<ByteSpan>,
        Scope<(Label<String>, Embed<RcTerm>), RcTerm>,
    ),
    /// Dependent record
    Record(
        Ignore<ByteSpan>,
        Scope<(Label<String>, Embed<RcTerm>), RcTerm>,
    ),
    /// The unit type
    RecordTypeEmpty(Ignore<ByteSpan>),
    /// The element of the unit type
    RecordEmpty(Ignore<ByteSpan>),
    /// Field projection
    Proj(Ignore<ByteSpan>, RcTerm, Ignore<ByteSpan>, Label<String>),
    /// Array literals
    Array(Ignore<ByteSpan>, Vec<RcTerm>),
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

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Ann(span, _, _)
            | Term::Universe(span, _)
            | Term::Hole(span)
            | Term::Literal(span, _)
            | Term::Var(span, _)
            | Term::Pi(span, _)
            | Term::Lam(span, _)
            | Term::RecordType(span, _)
            | Term::Record(span, _)
            | Term::RecordTypeEmpty(span)
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
