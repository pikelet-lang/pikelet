//! Parser utilities

use lalrpop_util::ParseError as LalrpopError;
use source::pos::Span;
use std::fmt;
use std::str::FromStr;

use syntax::concrete;
use syntax::parse::lexer::{Lexer, Token};

pub use syntax::parse::lexer::Error as LexerError;

mod lexer;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/syntax/parse/grammar.rs"));
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Lexer(LexerError),
    IdentifierExpectedInPiType,
    UnknownReplCommand(String),
    Other(String),
}

impl From<lexer::Error> for ParseError {
    fn from(src: lexer::Error) -> ParseError {
        ParseError::Lexer(src)
    }
}

impl<L: fmt::Debug, T: fmt::Debug> From<LalrpopError<L, T, ParseError>> for ParseError {
    fn from(src: LalrpopError<L, T, ParseError>) -> ParseError {
        match src {
            LalrpopError::User { error } => error,
            _ => ParseError::Other(format!("{:?}", src)),
        }
    }
}

impl FromStr for concrete::ReplCommand<Span> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::ReplCommand<Span>, ParseError> {
        grammar::parse_ReplCommand(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Module<Span> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Module<Span>, ParseError> {
        grammar::parse_Module(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Declaration<Span> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Declaration<Span>, ParseError> {
        grammar::parse_Declaration(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Term<Span> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Term<Span>, ParseError> {
        grammar::parse_Term(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammer.lalrpop` for
/// more information.
fn reparse_pi_type_hack<L, T>(
    span: Span,
    binder: concrete::Term<Span>,
    body: concrete::Term<Span>,
) -> Result<concrete::Term<Span>, LalrpopError<L, T, ParseError>> {
    use syntax::concrete::Term;

    fn param_names<L, T>(
        term: Term<Span>,
        names: &mut Vec<(Span, String)>,
    ) -> Result<(), LalrpopError<L, T, ParseError>> {
        match term {
            Term::Var(span, name) => names.push((span, name)),
            Term::App(_, fn_expr, arg) => {
                param_names(*fn_expr, names)?;
                param_names(*arg, names)?;
            },
            _ => {
                return Err(LalrpopError::User {
                    error: ParseError::IdentifierExpectedInPiType, // TODO: better error!
                });
            },
        }
        Ok(())
    }

    match binder {
        Term::Parens(paren_span, term) => {
            let term = *term; // HACK: see https://github.com/rust-lang/rust/issues/16223
            match term {
                Term::Ann(_, params, ann) => {
                    let mut names = Vec::new();
                    param_names(*params, &mut names)?;
                    Ok(Term::Pi(span, (names, ann), body.into()))
                },
                ann => {
                    let parens = Term::Parens(paren_span, ann.into()).into();
                    Ok(Term::Arrow(span, parens, body.into()))
                },
            }
        },
        ann => Ok(Term::Arrow(span, ann.into(), body.into())),
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn pi_bad_ident() {
        let parse_result = concrete::Term::from_str("((x : Type) : Type) -> Type");

        assert_eq!(parse_result, Err(ParseError::IdentifierExpectedInPiType),);
    }
}
