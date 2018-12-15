//! Parser utilities

use codespan::{ByteIndex, ByteSpan, FileMap};
use lalrpop_util::ParseError as LalrpopError;

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

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammar.lalrpop` for
/// more information.
fn reparse_fun_ty_hack<L, T>(
    span: ByteSpan,
    binder: concrete::Term,
    body: concrete::Term,
) -> Result<concrete::Term, LalrpopError<L, T, ParseError>> {
    use crate::syntax::concrete::Term;

    fn fun_ty_binder<L, T>(
        binder: &Term,
    ) -> Result<Option<concrete::FunTypeParamGroup>, LalrpopError<L, T, ParseError>> {
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
            Term::FunApp(ref head, ref args) => {
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
        Term::FunApp(ref head, ref args) => {
            use std::iter;

            let mut binders = Vec::with_capacity(args.len() + 1);

            for next in iter::once(&**head).chain(args).map(fun_ty_binder) {
                match next? {
                    Some((names, ann)) => binders.push((names, ann)),
                    None => return Ok(Term::FunArrow(Box::new(binder.clone()), Box::new(body))),
                }
            }

            Ok(Term::FunType(span.start(), binders, Box::new(body)))
        },
        binder => match fun_ty_binder(&binder)? {
            Some(binder) => Ok(Term::FunType(span.start(), vec![binder], Box::new(body))),
            None => Ok(Term::FunArrow(binder.into(), Box::new(body))),
        },
    }
}
