//! Parser utilities

use codespan::FileMap;

use crate::parse::lexer::Lexer;
use crate::syntax::concrete;

mod errors;
mod lexer;

pub use self::errors::{ExpectedTokens, ParseError};
pub use self::lexer::{LexerError, Token};

macro_rules! parser {
    ($name:ident, $output:ident, $parser_name:ident) => {
        pub fn $name<'input>(
            filemap: &'input FileMap,
        ) -> (concrete::$output, Vec<String>, Vec<ParseError>) {
            let mut import_paths = Vec::new();
            let mut errors = Vec::new();
            let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
            let value = grammar::$parser_name::new()
                .parse(&mut import_paths, &mut errors, filemap, lexer)
                .unwrap_or_else(|err| {
                    errors.push(errors::from_lalrpop(filemap, err));
                    concrete::$output::Error(filemap.span())
                });

            (value, import_paths, errors)
        }
    };
}

parser!(pattern, Pattern, PatternParser);
parser!(term, Term, TermParser);

mod grammar {
    #![allow(clippy::all)]

    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}
