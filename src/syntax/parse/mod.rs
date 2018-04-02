//! Parser utilities

use codespan::{ByteIndex, ByteSpan, FileMap};
use lalrpop_util::ParseError as LalrpopError;

use syntax::concrete;
use syntax::parse::lexer::Lexer;

mod errors;
mod lexer;

pub use self::errors::{ExpectedTokens, ParseError};
pub use self::lexer::{LexerError, Token};

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

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/syntax/parse/grammar.rs"));
}

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammer.lalrpop` for
/// more information.
fn reparse_pi_type_hack<L, T>(
    span: ByteSpan,
    binder: concrete::Term,
    body: concrete::Term,
) -> Result<concrete::Term, LalrpopError<L, T, ParseError>> {
    use syntax::concrete::Term;

    fn pi_binder<L, T>(
        binder: &Term,
    ) -> Result<Option<(Vec<(ByteIndex, String)>, Box<Term>)>, LalrpopError<L, T, ParseError>> {
        match *binder {
            Term::Parens(_, ref term) => match **term {
                Term::Ann(ref params, ref ann) => {
                    let mut names = Vec::new();
                    param_names(&**params, &mut names)?;
                    Ok(Some((names, ann.clone())))
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
            Term::Var(start, ref name) => names.push((start, name.clone())),
            Term::App(ref fn_expr, ref args) => {
                param_names(fn_expr, names)?;
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
        Term::App(ref fn_expr, ref args) => {
            use std::iter;

            let mut binders = Vec::with_capacity(args.len() + 1);

            for next in iter::once(&**fn_expr).chain(args).map(pi_binder) {
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
                vec![
                    ParseError::IdentifierExpectedInPiType {
                        span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
                    },
                ],
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
                vec![
                    ParseError::IdentifierExpectedInPiType {
                        span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
                    },
                ],
            )
        );
    }

    #[test]
    fn integer_overflow() {
        let src = "Type 111111111111111111111111111111";
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

        let parse_result = term(&filemap);

        assert_eq!(
            parse_result,
            (
                concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(36))),
                vec![
                    ParseError::Lexer(LexerError::IntegerLiteralOverflow {
                        span: ByteSpan::new(ByteIndex(6), ByteIndex(36)),
                        value: String::from("111111111111111111111111111111"),
                    }),
                ],
            )
        );
    }
}
