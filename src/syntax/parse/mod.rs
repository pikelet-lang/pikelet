//! Parser utilities

use codespan::{ByteIndex, ByteSpan, FileMap};
use lalrpop_util::ParseError as LalrpopError;

use syntax::concrete;
use syntax::parse::lexer::Lexer;

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

parser!(repl_command, ReplCommand, ReplCommandParser);
parser!(module, Module, ModuleParser);
parser!(pattern, Pattern, PatternParser);
parser!(term, Term, TermParser);

mod grammar {
    #![cfg_attr(feature = "cargo-clippy", allow(clippy))]

    include!(concat!(env!("OUT_DIR"), "/syntax/parse/grammar.rs"));
}

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammar.lalrpop` for
/// more information.
fn reparse_pi_type_hack<L, T>(
    span: ByteSpan,
    binder: concrete::Term,
    body: concrete::Term,
) -> Result<concrete::Term, LalrpopError<L, T, ParseError>> {
    use syntax::concrete::Term;

    fn pi_binder<L, T>(
        binder: &Term,
    ) -> Result<Option<concrete::PiParamGroup>, LalrpopError<L, T, ParseError>> {
        match *binder {
            Term::Parens(_, ref term) => match **term {
                Term::Ann(ref params, ref ann) => {
                    let mut names = Vec::new();
                    param_names(&**params, &mut names)?;
                    Ok(Some((names, (**ann).clone())))
                },
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn param_names<L, T>(
        term: &Term,
        names: &mut Vec<(ByteIndex, String)>,
    ) -> Result<(), LalrpopError<L, T, ParseError>> {
        match *term {
            Term::Name(span, ref name, None) => names.push((span.start(), name.clone())),
            Term::App(ref head, ref args) => {
                param_names(head, names)?;
                for arg in args {
                    param_names(arg, names)?;
                }
            },
            _ => {
                return Err(LalrpopError::User {
                    error: ParseError::IdentifierExpectedInPiType { span: term.span() },
                });
            },
        }
        Ok(())
    }

    match binder {
        Term::App(ref head, ref args) => {
            use std::iter;

            let mut binders = Vec::with_capacity(args.len() + 1);

            for next in iter::once(&**head).chain(args).map(pi_binder) {
                match next? {
                    Some((names, ann)) => binders.push((names, ann)),
                    None => return Ok(Term::Arrow(Box::new(binder.clone()), Box::new(body))),
                }
            }

            Ok(Term::Pi(span.start(), binders, Box::new(body)))
        },
        binder => match pi_binder(&binder)? {
            Some(binder) => Ok(Term::Pi(span.start(), vec![binder], Box::new(body))),
            None => Ok(Term::Arrow(binder.into(), Box::new(body))),
        },
    }
}

#[cfg(test)]
mod tests {
    use codespan::{ByteIndex, ByteSpan};
    use codespan::{CodeMap, FileName};

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
                concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(28))),
                vec![],
                vec![ParseError::IdentifierExpectedInPiType {
                    span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
                }],
            )
        );
    }

    #[test]
    fn pi_bad_ident_multi() {
        let src = "((x : Type) : Type) (x : Type) -> Type";
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

        let parse_result = term(&filemap);

        assert_eq!(
            parse_result,
            (
                concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(39))),
                vec![],
                vec![ParseError::IdentifierExpectedInPiType {
                    span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
                }],
            )
        );
    }

    #[test]
    fn integer_overflow() {
        let src = "Type^111111111111111111111111111111";
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

        let parse_result = term(&filemap);

        assert_eq!(
            parse_result,
            (
                concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(36))),
                vec![],
                vec![ParseError::Lexer(LexerError::IntegerLiteralOverflow {
                    span: ByteSpan::new(ByteIndex(6), ByteIndex(36)),
                    value: "111111111111111111111111111111".to_owned(),
                })],
            )
        );
    }
}
