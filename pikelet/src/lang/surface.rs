//! The surface language.
//!
//! This is a user-friendly concrete syntax for the language.

use crossbeam_channel::Sender;

use crate::lang::Ranged;
use crate::reporting::Message;

mod lexer;

#[allow(clippy::all, unused_parens)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/lang/surface/grammar.rs"));
}

/// Entry in a [record type](Term::RecordType).
pub type TypeEntry = (Ranged<String>, Option<Ranged<String>>, Term);
/// Entry in a [record term](Term::RecordTerm).
pub type TermEntry = (Ranged<String>, Option<Ranged<String>>, Term);
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
    SequenceTerm(Vec<Term>),
    /// Character literals.
    CharTerm(String),
    /// String literals.
    StringTerm(String),
    /// Numeric literals.
    NumberTerm(String),

    /// Error sentinel.
    Error,
}

impl<'input> Term {
    /// Parse a term from an input string.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(input: &str, messages_tx: &Sender<Message>) -> Term {
        let tokens = lexer::tokens(input);
        grammar::TermParser::new()
            .parse(tokens)
            .unwrap_or_else(|error| {
                messages_tx.send(Message::from_lalrpop(error)).unwrap();
                Term::from(TermData::Error)
            })
    }
}
