use std::str::FromStr;

use core;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError(pub String);

#[derive(Debug, Clone)]
pub enum ReplCommand {
    NoOp,
    Help,
    Eval(Box<Term>),
    TypeOf(Box<Term>),
}

impl FromStr for ReplCommand {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<ReplCommand, ParseError> {
        grammar::parse_ReplCommand(src).map_err(|e| ParseError(format!("{:?}", e)))
    }
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
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Term, ParseError> {
        grammar::parse_Term(src).map_err(|e| ParseError(format!("{:?}", e)))
    }
}

// FIXME: use a proper error type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToCoreError;

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
    pub fn to_core(&self) -> Result<core::ITerm, ToCoreError> {
        use std::rc::Rc;

        use core::{CTerm, ITerm};
        use var::{Name, Named, Var};

        fn to_cterm<T>(tm: &Term) -> Result<CTerm, Option<T>> {
            match to_iterm(tm) {
                Ok(itm) => Ok(CTerm::Inf(Rc::new(itm))),
                Err(Some(etm)) => Ok(etm),
                Err(None) => Err(None),
            }
        }

        fn to_iterm(tm: &Term) -> Result<ITerm, Option<CTerm>> {
            match *tm {
                Term::Var(ref x) => Ok(ITerm::Var(Var::Free(Name::User(x.clone())))),
                Term::Type => Ok(ITerm::Type),
                Term::Ann(ref e, ref t) => {
                    Ok(ITerm::Ann(Rc::new(to_cterm(e)?), Rc::new(to_cterm(t)?)))
                }
                Term::Lam(ref n, Some(ref t), ref body) => match body.to_core() {
                    Ok(mut body) => {
                        let name = Name::User(n.clone());
                        body.abstract0(&name);

                        Ok(ITerm::Lam(
                            Named(name, Rc::new(to_cterm(t)?)),
                            Rc::new(body),
                        ))
                    }
                    Err(ToCoreError) => return Err(None),
                },
                Term::Lam(ref n, None, ref body) => {
                    let name = Name::User(n.clone());
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Err(Some(CTerm::Lam(Named(name, ()), Rc::new(body))))
                }
                Term::Pi(ref n, ref t, ref body) => {
                    let name = match *n {
                        Some(ref n) => Name::User(n.clone()),
                        None => Name::Abstract,
                    };
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Ok(ITerm::Pi(Named(name, Rc::new(to_cterm(t)?)), Rc::new(body)))
                }
                Term::App(ref f, ref x) => match f.to_core() {
                    Ok(f) => Ok(ITerm::App(Rc::new(f), Rc::new(to_cterm(x)?))),
                    // Type annotations needed!
                    Err(ToCoreError) => Err(None),
                },
            }
        }

        to_iterm(self).map_err(|_| ToCoreError)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use core::{CTerm, ITerm};
    use var::{Debruijn, Name, Named, Var};

    use super::*;

    fn parse(src: &str) -> ITerm {
        Term::to_core(&src.parse().unwrap()).unwrap()
    }

    #[test]
    fn var() {
        assert_eq!(parse(r"x"), ITerm::from(Var::Free(Name::user("x"))));
    }

    #[test]
    fn ty() {
        assert_eq!(parse(r"Type"), ITerm::Type);
    }

    #[test]
    fn ann() {
        assert_eq!(
            parse(r"Type : Type"),
            ITerm::Ann(
                Rc::new(CTerm::from(ITerm::Type)),
                Rc::new(CTerm::from(ITerm::Type)),
            )
        );
    }

    #[test]
    fn ann_ann_left() {
        assert_eq!(
            parse(r"Type : Type : Type"),
            ITerm::Ann(
                Rc::new(CTerm::from(ITerm::Ann(
                    Rc::new(CTerm::from(ITerm::Type)),
                    Rc::new(CTerm::from(ITerm::Type)),
                ),),),
                Rc::new(CTerm::from(ITerm::Type)),
            )
        );
    }

    #[test]
    fn ann_ann_right() {
        assert_eq!(
            parse(r"Type : (Type : Type)"),
            ITerm::Ann(
                Rc::new(CTerm::from(ITerm::Type)),
                Rc::new(CTerm::from(ITerm::Ann(
                    Rc::new(CTerm::from(ITerm::Type)),
                    Rc::new(CTerm::from(ITerm::Type)),
                ),),),
            )
        );
    }

    #[test]
    fn lam_ann() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"\x : Type -> Type => x"),
            ITerm::Lam(
                Named(
                    x.clone(),
                    Rc::new(CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, Rc::new(CTerm::from(ITerm::Type))),
                        Rc::new(CTerm::from(ITerm::Type)),
                    ),),),
                ),
                Rc::new(ITerm::from(Var::Bound(Named(x, Debruijn(0))))),
            ),
        );
    }

    #[test]
    fn lam() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : (\y => y) => x"),
            ITerm::Lam(
                Named(
                    x.clone(),
                    Rc::new(CTerm::Lam(
                        Named(y.clone(), ()),
                        Rc::new(CTerm::from(Var::Bound(Named(y, Debruijn(0)))))
                    )),
                ),
                Rc::new(ITerm::from(Var::Bound(Named(x, Debruijn(0))))),
            ),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : Type => \y : Type => x"),
            ITerm::Lam(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(ITerm::Lam(
                    Named(y, Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(ITerm::from(Var::Bound(Named(x, Debruijn(1))))),
                ),),
            ),
        );
    }

    #[test]
    fn arrow() {
        assert_eq!(
            parse(r"Type -> Type"),
            ITerm::Pi(
                Named(Name::Abstract, Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Type)),
            ),
        );
    }

    #[test]
    fn pi() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"[x : Type -> Type] -> x"),
            ITerm::Pi(
                Named(
                    x.clone(),
                    Rc::new(CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, Rc::new(CTerm::from(ITerm::Type))),
                        Rc::new(CTerm::from(ITerm::Type)),
                    ),),),
                ),
                Rc::new(CTerm::from(Var::Bound(Named(x, Debruijn(0))))),
            ),
        );
    }

    #[test]
    fn pi_pi() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"[x : Type] -> [y : Type] -> x"),
            ITerm::Pi(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Pi(
                    Named(y, Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(CTerm::from(Var::Bound(Named(x, Debruijn(1))))),
                ),),),
            ),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"[x : Type] -> x -> x"),
            ITerm::Pi(
                Named(x.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Pi(
                    Named(
                        Name::Abstract,
                        Rc::new(CTerm::from(Var::Bound(Named(x.clone(), Debruijn(0)))))
                    ),
                    Rc::new(CTerm::from(Var::Bound(Named(x, Debruijn(1))))),
                ),),),
            ),
        );
    }

    #[test]
    fn lam_app() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : (Type -> Type) => \y : Type => x y"),
            ITerm::Lam(
                Named(
                    x.clone(),
                    Rc::new(CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, Rc::new(CTerm::from(ITerm::Type))),
                        Rc::new(CTerm::from(ITerm::Type)),
                    ),),),
                ),
                Rc::new(ITerm::Lam(
                    Named(y.clone(), Rc::new(CTerm::from(ITerm::Type))),
                    Rc::new(ITerm::App(
                        Rc::new(ITerm::from(Var::Bound(Named(x, Debruijn(1))))),
                        Rc::new(CTerm::from(Var::Bound(Named(y, Debruijn(0))))),
                    ),),
                ),),
            ),
        );
    }

    #[test]
    fn id() {
        let x = Name::user("x");
        let a = Name::user("a");

        assert_eq!(
            parse(r"\a : Type => \x : a => x"),
            ITerm::Lam(
                Named(a.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(ITerm::Lam(
                    Named(
                        x.clone(),
                        Rc::new(CTerm::from(ITerm::from(Var::Bound(Named(a, Debruijn(0)))))),
                    ),
                    Rc::new(ITerm::from(Var::Bound(Named(x, Debruijn(0))))),
                ),),
            )
        );
    }

    #[test]
    fn id_ty() {
        let a = Name::user("a");

        assert_eq!(
            parse(r"[a : Type] -> a -> a"),
            ITerm::Pi(
                Named(a.clone(), Rc::new(CTerm::from(ITerm::Type))),
                Rc::new(CTerm::from(ITerm::Pi(
                    Named(
                        Name::Abstract,
                        Rc::new(CTerm::from(ITerm::from(Var::Bound(Named(
                            a.clone(),
                            Debruijn(0)
                        ))))),
                    ),
                    Rc::new(CTerm::from(ITerm::from(Var::Bound(Named(a, Debruijn(1)))))),
                ),),),
            ),
        );
    }

    #[test]
    fn id_ty_arr() {
        assert_eq!(
            parse(r"[a : Type] -> a -> a"),
            parse(r"[a : Type] -> [x : a] -> a"),
        )
    }
}
