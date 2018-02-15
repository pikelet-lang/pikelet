//! Parser utilities

use lalrpop_util::ParseError as LalrpopError;
use source::pos::{BytePos, RawIndex, Span};

use syntax::concrete;
use syntax::parse::lexer::Lexer;

mod grammar;
mod lexer;
mod errors;

pub use self::lexer::{LexerError, Token};
pub use self::errors::{ExpectedTokens, ParseError};

// TODO: DRY up these wrappers...

pub fn repl_command<'err, 'src>(src: &'src str) -> (concrete::ReplCommand, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(src).map(|x| x.map_err(ParseError::from));
    match grammar::parse_ReplCommand(&mut errors, src, lexer) {
        Ok(value) => (value, errors),
        Err(_) => unimplemented!(),
    }
}

pub fn module<'err, 'src>(src: &'src str) -> (concrete::Module, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(src).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Module(&mut errors, src, lexer) {
        Ok(value) => (value, errors),
        Err(_) => unimplemented!(),
    }
}

pub fn declaration<'err, 'src>(src: &'src str) -> (concrete::Declaration, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(src).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Declaration(&mut errors, src, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            let src_span = Span::new(BytePos(0), BytePos(src.len() as RawIndex));
            errors.push(errors::from_lalrpop(src, err));
            (concrete::Declaration::Error(src_span), errors)
        },
    }
}

pub fn term<'err, 'src>(src: &'src str) -> (concrete::Term, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(src).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Term(&mut errors, src, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            let src_span = Span::new(BytePos(0), BytePos(src.len() as RawIndex));
            errors.push(errors::from_lalrpop(src, err));
            (concrete::Term::Error(src_span), errors)
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pi_bad_ident() {
        let parse_result = term("((x : Type) : Type) -> Type");

        assert_eq!(
            parse_result,
            (
                concrete::Term::Error(Span::new(BytePos(0), BytePos(27))),
                vec![
                    ParseError::IdentifierExpectedInPiType {
                        span: Span::new(BytePos(1), BytePos(11)),
                    },
                ],
            )
        );
    }
}
