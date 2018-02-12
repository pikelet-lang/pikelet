//! Parser utilities

use std::fmt;
use std::str::FromStr;

use syntax::concrete;
use syntax::parse::lexer::{Lexer, Token};

use lalrpop_util::ParseError as LalrpopError;

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

impl FromStr for concrete::ReplCommand {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::ReplCommand, ParseError> {
        grammar::parse_ReplCommand(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Module {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Module, ParseError> {
        grammar::parse_Module(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Declaration {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Declaration, ParseError> {
        grammar::parse_Declaration(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

impl FromStr for concrete::Term {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Term, ParseError> {
        grammar::parse_Term(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
            .map_err(ParseError::from)
    }
}

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammer.lalrpop` for
/// more information.
fn reparse_pi_type_hack<L, T>(
    binder: concrete::Term,
    body: concrete::Term,
) -> Result<concrete::Term, LalrpopError<L, T, ParseError>> {
    use syntax::concrete::Term;

    fn param_names<L, T>(
        term: Term,
        names: &mut Vec<String>,
    ) -> Result<(), LalrpopError<L, T, ParseError>> {
        match term {
            Term::Var(name) => names.push(name),
            Term::App(fn_expr, arg) => {
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
        Term::Parens(term) => {
            let term = *term; // HACK: see https://github.com/rust-lang/rust/issues/16223
            match term {
                Term::Ann(params, ann) => {
                    let mut names = Vec::new();
                    param_names(*params, &mut names)?;
                    Ok(Term::Pi(names, ann, body.into()))
                },
                ann => Ok(Term::Arrow(Term::Parens(ann.into()).into(), body.into())),
            }
        },
        ann => Ok(Term::Arrow(ann.into(), body.into())),
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
