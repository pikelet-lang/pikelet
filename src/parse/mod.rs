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
    Pi(Option<String>, Box<Term>, Box<Term>),
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
//     TypeAnnRequiredForTopLevelLam,
//     /// Tried to apply an argument to a checkable term
//     ArgumentAppliedToAmbiguousTerm,
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
                Term::Lam(ref n, Some(ref t), ref body) => match body.to_core() {
                    Ok(mut body) => {
                        let name = Name(n.clone());
                        body.abstract0(&name);

                        Ok(ITerm::Lam(
                            Named(name, Rc::new(to_cterm(t)?)),
                            Rc::new(body),
                        ))
                    }
                    Err(()) => return Err(None),
                },
                Term::Lam(ref n, None, ref body) => {
                    let name = Name(n.clone());
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Err(Some(CTerm::Lam(Named(name, ()), Rc::new(body))))
                }
                Term::Pi(ref n, ref t, ref body) => {
                    let name = match *n {
                        Some(ref n) => Name(n.clone()),
                        None => Name(String::from("_")),
                    };
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

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

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use core::{CTerm, Debruijn, ITerm, Name, Named};
    use super::*;

    fn parse(src: &str) -> ITerm {
        Term::to_core(&src.parse().unwrap()).unwrap()
    }

    #[test]
    fn var() {
        assert_eq!(parse(r"x"), ITerm::Free(Name(String::from("x"))));
    }

    #[test]
    fn ty() {
        assert_eq!(parse(r"*"), ITerm::Type);
    }

    #[test]
    fn lam_ann() {
        let x = Name(String::from("x"));

        assert_eq!(
            parse(r"\x : *, x"),
            ITerm::Lam(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(ITerm::Bound(Named(x, Debruijn(0)))),
            ),
        );
    }

    #[test]
    fn lam() {
        let x = Name(String::from("x"));
        let y = Name(String::from("y"));

        assert_eq!(
            parse(r"\x : (\y, y), x"),
            ITerm::Lam(
                Named(
                    x.clone(),
                    Rc::new(CTerm::Lam(
                        Named(y.clone(), ()),
                        Rc::new(CTerm::from(ITerm::Bound(Named(y, Debruijn(0)))))
                    ))
                ),
                Rc::new(ITerm::Bound(Named(x, Debruijn(0)))),
            ),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = Name(String::from("x"));
        let y = Name(String::from("y"));

        assert_eq!(
            parse(r"\x : *, \y : *, x"),
            ITerm::Lam(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(ITerm::Lam(
                    Named(y, Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(ITerm::Bound(Named(x, Debruijn(1)))),
                )),
            ),
        );
    }

    #[test]
    fn arrow() {
        let u = Name(String::from("_"));

        assert_eq!(
            parse(r"* -> *"),
            ITerm::Pi(
                Named(u, Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Type)),
            ),
        );
    }

    #[test]
    fn pi() {
        let x = Name(String::from("x"));

        assert_eq!(
            parse(r"[x : *] -> x"),
            ITerm::Pi(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Bound(Named(x, Debruijn(0))))),
            ),
        );
    }

    #[test]
    fn pi_pi() {
        let x = Name(String::from("x"));
        let y = Name(String::from("y"));

        assert_eq!(
            parse(r"[x : *] -> [y : *] -> x"),
            ITerm::Pi(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Pi(
                    Named(y, Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(CTerm::from(ITerm::Bound(Named(x, Debruijn(1))))),
                ))),
            ),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = Name(String::from("x"));
        let u = Name(String::from("_"));

        assert_eq!(
            parse(r"[x : *] -> x -> x"),
            ITerm::Pi(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Pi(
                    Named(
                        u,
                        Rc::new(CTerm::from(ITerm::Bound(Named(x.clone(), Debruijn(0)))))
                    ),
                    Rc::new(CTerm::from(ITerm::Bound(Named(x, Debruijn(1))))),
                ))),
            ),
        );
    }

    #[test]
    fn lam_app() {
        let x = Name(String::from("x"));
        let y = Name(String::from("y"));
        let u = Name(String::from("_"));

        assert_eq!(
            parse(r"\x : (* -> *), \y : *, x y"),
            ITerm::Lam(
                Named(
                    x.clone(),
                    Rc::new(CTerm::from(ITerm::Pi(
                        Named(u, Rc::new(CTerm::from(ITerm::Type))),
                        Rc::new(CTerm::from(ITerm::Type)),
                    ))),
                ),
                Rc::new(ITerm::Lam(
                    Named(y.clone(), Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(ITerm::App(
                        Rc::new(ITerm::Bound(Named(x, Debruijn(1)))),
                        Rc::new(CTerm::from(ITerm::Bound(Named(y, Debruijn(0)))))
                    )),
                )),
            ),
        );
    }
}
