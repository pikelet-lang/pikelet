//! Parser utilities

use lalrpop_util::ParseError as LalrpopError;
use codespan::FileMap;

use syntax::concrete;
use syntax::parse::lexer::Lexer;

mod grammar;
mod lexer;
mod errors;

pub use self::lexer::{LexerError, Token};
pub use self::errors::{ExpectedTokens, ParseError};

// TODO: DRY up these wrappers...

pub fn repl_command<'input>(filemap: &'input FileMap) -> (concrete::ReplCommand, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_ReplCommand(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::ReplCommand::Error(filemap.span()), errors)
        },
    }
}

pub fn module<'input>(filemap: &'input FileMap) -> (concrete::Module, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Module(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Module::Error(filemap.span()), errors)
        },
    }
}

pub fn term<'input>(filemap: &'input FileMap) -> (concrete::Term, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Term(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Term::Error(filemap.span()), errors)
        },
    }
}

#[cfg(test)]
mod tests {
    use codespan::{CodeMap, FileName};
    use codespan::{BytePos, Span};

    use super::*;

    #[test]
    fn pi_bad_ident() {
        let src = "((x : Type) : Type) -> Type";
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

        let parse_result = term(&filemap);

        assert_eq!(
            parse_result,
            (
                concrete::Term::Error(Span::new(BytePos(1), BytePos(28))),
                vec![
                    ParseError::IdentifierExpectedInPiType {
                        span: Span::new(BytePos(2), BytePos(12)),
                    },
                ],
            )
        );
    }
}
