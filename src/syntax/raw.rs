//! The syntax of the language, unchecked and with implicit parts that need to
//! be elaborated in a type-directed way during type checking and inference

use codespan::{ByteIndex, ByteSpan};
use moniker::{Embed, FreeVar, Ignore, Nest, Scope, Var};
use std::fmt;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::{Label, Level};

/// A module definition
pub struct Module {
    /// The definitions contained in the module
    pub definitions: Nest<(FreeVar, Embed<Definition>)>,
}

/// Top level definitions
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct Definition {
    /// The body of the definition
    pub term: Rc<Term>,
    /// An optional type annotation to aid in type inference
    pub ann: Rc<Term>,
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
    Ann(Ignore<ByteSpan>, Rc<Term>, Rc<Term>),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Literals
    Literal(Ignore<ByteSpan>, Literal),
    /// A hole
    Hole(Ignore<ByteSpan>),
    /// A variable
    Var(Ignore<ByteSpan>, Var),
    /// Dependent function types
    Pi(
        Ignore<ByteSpan>,
        Scope<(FreeVar, Embed<Rc<Term>>), Rc<Term>>,
    ),
    /// Lambda abstractions
    Lam(
        Ignore<ByteSpan>,
        Scope<(FreeVar, Embed<Rc<Term>>), Rc<Term>>,
    ),
    /// Term application
    App(Rc<Term>, Rc<Term>),
    /// If expression
    If(Ignore<ByteIndex>, Rc<Term>, Rc<Term>, Rc<Term>),
    /// Dependent record types
    RecordType(Ignore<ByteSpan>, Scope<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// Dependent record
    Record(Ignore<ByteSpan>, Scope<(Label, Embed<Rc<Term>>), Rc<Term>>),
    /// The unit type
    RecordTypeEmpty(Ignore<ByteSpan>),
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
