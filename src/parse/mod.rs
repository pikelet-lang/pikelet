use std::str::FromStr;

use core;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}

/// The AST of the concrete syntax
#[derive(Debug, Clone)]
pub enum Term {
    Var(String),
    Type,
    Ann(Box<Term>, Box<Term>),
    Lam(String, Option<Box<Term>>, Box<Term>),
    Pi(String, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl FromStr for Term {
    type Err = String;

    fn from_str(src: &str) -> Result<Term, String> {
        grammar::parse_Term(src).map_err(|e| format!("{:?}", e))
    }
}

// FIXME: use a proper error type
//
// enum ToCoreError {
//     /// Tried to convert a checkable term to an inferrable term
//     CTerm(CTerm),
//     /// Tried to apply an argument to a checkable term
//     TypeAnnotationsNeeded,
// }

impl Term {
    /// Convert a parsed term into an inferrable term.
    ///
    /// This may fail if the term is not fully inferrable.
    pub fn to_core(&self) -> Result<core::ITerm, ()> {
        use std::rc::Rc;
        use core::{CTerm, ITerm, Name, Named};

        fn to_cterm<T>(tm: &Term) -> Result<CTerm, Option<T>> {
            match to_iterm(tm) {
                Ok(itm) => Ok(CTerm::Inf(Rc::new(itm))),
                Err(Some(etm)) => Ok(etm),
                Err(None) => Err(None),
            }
        }

        fn to_iterm(tm: &Term) -> Result<ITerm, Option<CTerm>> {
            match *tm {
                Term::Var(ref x) => Ok(ITerm::Free(Name(x.clone()))),
                Term::Type => Ok(ITerm::Type),
                Term::Ann(ref e, ref t) => {
                    Ok(ITerm::Ann(Rc::new(to_cterm(e)?), Rc::new(to_cterm(t)?)))
                }
                Term::Lam(ref n, Some(ref t), ref body) => {
                    let name = Name(n.clone());
                    let body = match body.to_core() {
                        Ok(body) => body.abstract0(&name),
                        Err(()) => return Err(None),
                    };
                    Ok(ITerm::Lam(
                        Named(name, Rc::new(to_cterm(t)?)),
                        Rc::new(body),
                    ))
                }
                Term::Lam(ref n, None, ref body) => {
                    let name = Name(n.clone());
                    let body = to_cterm(body)?.abstract0(&name);
                    Err(Some(CTerm::Lam(Named(name, ()), Rc::new(body))))
                }
                Term::Pi(ref n, ref t, ref body) => {
                    let name = Name(n.clone());
                    let body = to_cterm(body)?.abstract0(&name);
                    Ok(ITerm::Pi(Named(name, Rc::new(to_cterm(t)?)), Rc::new(body)))
                }
                Term::App(ref f, ref x) => match f.to_core() {
                    Ok(f) => Ok(ITerm::App(Rc::new(f), Rc::new(to_cterm(x)?))),
                    // Type annotations needed!
                    Err(()) => Err(None),
                },
            }
        }

        // FIXME: use a proper error type
        to_iterm(self).map_err(|_| ())
    }
}
