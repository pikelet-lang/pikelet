//! The surface language.
//!
//! This is a user-friendly concrete syntax for the language.

use std::ops::{Range, RangeTo};

pub mod projections;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/surface/grammar.rs"));
}

pub enum Term<S> {
    /// Names.
    Name(Range<usize>, S),
    /// Annotated terms.
    Ann(Box<Term<S>>, Box<Term<S>>),
    /// Literals.
    Literal(Range<usize>, Literal<S>),
    /// Ordered sequences.
    Sequence(Range<usize>, Vec<Term<S>>),
    /// Record types.
    RecordType(Range<usize>, Vec<(S, Term<S>)>),
    /// Record terms.
    RecordTerm(Range<usize>, Vec<(S, Term<S>)>),
    /// Record eliminations (field access).
    RecordElim(RangeTo<usize>, Box<Term<S>>, S),
    /// Function types.
    FunctionType(Box<Term<S>>, Box<Term<S>>),
    /// Function eliminations (function application).
    FunctionElim(Box<Term<S>>, Vec<Term<S>>),
    /// Lift a term by the given number of universe levels.
    Lift(Range<usize>, Box<Term<S>>, u32),
    /// Error sentinel.
    Error(Range<usize>),
}

type ParseError<'input> = lalrpop_util::ParseError<usize, grammar::Token<'input>, &'static str>;

impl<'input> Term<&'input str> {
    pub fn from_str(input: &'input str) -> Result<Term<&'input str>, ParseError<'input>> {
        grammar::TermParser::new().parse(input)
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Term::Name(span, _)
            | Term::Literal(span, _)
            | Term::Sequence(span, _)
            | Term::RecordType(span, _)
            | Term::RecordTerm(span, _)
            | Term::Lift(span, _, _)
            | Term::Error(span) => span.clone(),
            Term::Ann(term, r#type) => term.span().start..r#type.span().end,
            Term::RecordElim(span, term, _) => term.span().start..span.end,
            Term::FunctionType(param_type, body_type) => {
                param_type.span().start..body_type.span().end
            }
            Term::FunctionElim(head, arguments) => match arguments.last() {
                Some(argument) => head.span().start..argument.span().end,
                None => head.span(),
            },
        }
    }
}

/// Literals.
pub enum Literal<S> {
    /// Character literals.
    Char(S),
    /// String literals.
    String(S),
    /// Numeric literals.
    Number(S),
}
