//! The surface language.
//!
//! This is a user-friendly concrete syntax for the language.

use crate::lang::Ranged;
use crate::reporting::Message;

mod lexer;

#[allow(clippy::all, unused_parens)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/lang/surface/grammar.rs"));
}

/// Literals.
#[derive(Debug, Clone)]
pub enum Literal {
    /// Character literals.
    Char(String),
    /// String literals.
    String(String),
    /// Numeric literals.
    Number(String),
}

/// Entry in a [record type](Term::RecordType).
pub type TypeEntry = (Ranged<String>, Option<Ranged<String>>, Term);
/// Entry in a [record term](Term::RecordTerm).
pub type TermEntry = (Ranged<String>, Term);
/// A group of function inputs that are elements of the same type.
pub type InputGroup = (Vec<Ranged<String>>, Term);

pub type Term = Ranged<TermData>;

/// Terms in the surface language.
#[derive(Debug, Clone)]
pub enum TermData {
    /// Names.
    Name(String),

    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),

    /// Lift a term by the given number of universe levels.
    Lift(Box<Term>, u32),

    /// Function types.
    ///
    /// Also known as: pi type, dependent product type.
    FunctionType(Vec<InputGroup>, Box<Term>),
    /// Arrow function types.
    ///
    /// Also known as: non-dependent function type.
    FunctionArrowType(Box<Term>, Box<Term>),
    /// Function terms.
    ///
    /// Also known as: lambda abstraction, anonymous function.
    FunctionTerm(Vec<Ranged<String>>, Box<Term>),
    /// Function eliminations.
    ///
    /// Also known as: function application.
    FunctionElim(Box<Term>, Vec<Term>),

    /// Record types.
    RecordType(Vec<TypeEntry>),
    /// Record terms.
    RecordTerm(Vec<TermEntry>),
    /// Record eliminations.
    ///
    /// Also known as: record projections, field lookup.
    RecordElim(Box<Term>, Ranged<String>),

    /// Ordered sequences.
    Sequence(Vec<Term>),

    /// Literals.
    Literal(Literal),

    /// Error sentinel.
    Error,
}

impl<'input> Term {
    /// Parse a term from an input string.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(input: &str) -> Result<Term, Message> {
        let tokens = lexer::tokens(input);
        grammar::TermParser::new()
            .parse(tokens)
            .map_err(Message::from_lalrpop)
    }
}
