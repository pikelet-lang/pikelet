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
    Pi(String, Box<Term>, Box<Term>),
    Arrow(Box<Term>, Box<Term>),
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
    pub fn to_core(&self) -> Result<core::RcITerm, ToCoreError> {
        use core::{CTerm, ITerm, RcCTerm, RcITerm};
        use var::{Name, Named, Var};

        fn to_cterm<T>(term: &Term) -> Result<RcCTerm, Option<T>> {
            match to_iterm(term) {
                Ok(iterm) => Ok(CTerm::Inf(RcITerm::from(iterm)).into()),
                Err(Some(cterm)) => Ok(cterm),
                Err(None) => Err(None),
            }
        }

        fn to_iterm(term: &Term) -> Result<RcITerm, Option<RcCTerm>> {
            match *term {
                Term::Var(ref x) => Ok(ITerm::Var(Var::Free(Name::User(x.clone()))).into()),
                Term::Type => Ok(ITerm::Type.into()),
                Term::Ann(ref e, ref t) => {
                    Ok(ITerm::Ann(to_cterm(e)?.into(), to_cterm(t)?.into()).into())
                }
                Term::Lam(ref name, Some(ref ann), ref body) => match body.to_core() {
                    Ok(mut body) => {
                        let name = Name::User(name.clone());
                        body.abstract0(&name);

                        Ok(ITerm::Lam(Named(name, to_cterm(ann)?.into()), body.into()).into())
                    }
                    Err(ToCoreError) => return Err(None),
                },
                Term::Lam(ref name, None, ref body) => {
                    let name = Name::User(name.clone());
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Err(Some(CTerm::Lam(Named(name, ()), body.into()).into()).into())
                }
                Term::Pi(ref name, ref ann, ref body) => {
                    let name = Name::User(name.clone());
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Ok(ITerm::Pi(Named(name, to_cterm(ann)?.into()), body.into()).into())
                }
                Term::Arrow(ref ann, ref body) => {
                    let name = Name::Abstract;
                    let mut body = to_cterm(body)?;
                    body.abstract0(&name);

                    Ok(ITerm::Pi(Named(name, to_cterm(ann)?.into()), body.into()).into())
                }
                Term::App(ref f, ref arg) => match f.to_core() {
                    Ok(f) => Ok(ITerm::App(f.into(), to_cterm(arg)?.into()).into()),
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
    use core::{CTerm, ITerm, RcITerm};
    use var::{Debruijn, Name, Named, Var};

    use super::*;

    fn parse(src: &str) -> RcITerm {
        Term::to_core(&src.parse().unwrap()).unwrap()
    }

    #[test]
    fn var() {
        assert_eq!(parse(r"x"), ITerm::from(Var::Free(Name::user("x"))).into());
    }

    #[test]
    fn ty() {
        assert_eq!(parse(r"Type"), ITerm::Type.into());
    }

    #[test]
    fn ann() {
        assert_eq!(
            parse(r"Type : Type"),
            ITerm::Ann(
                CTerm::from(ITerm::Type).into(),
                CTerm::from(ITerm::Type).into(),
            ).into(),
        );
    }

    #[test]
    fn ann_ann_left() {
        assert_eq!(
            parse(r"Type : Type : Type"),
            ITerm::Ann(
                CTerm::from(ITerm::Ann(
                    CTerm::from(ITerm::Type).into(),
                    CTerm::from(ITerm::Type).into(),
                ),)
                    .into(),
                CTerm::from(ITerm::Type).into(),
            ).into(),
        );
    }

    #[test]
    fn ann_ann_right() {
        assert_eq!(
            parse(r"Type : (Type : Type)"),
            ITerm::Ann(
                CTerm::from(ITerm::Type).into(),
                CTerm::from(ITerm::Ann(
                    CTerm::from(ITerm::Type).into(),
                    CTerm::from(ITerm::Type).into(),
                ),)
                    .into(),
            ).into(),
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
                    CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, CTerm::from(ITerm::Type).into()),
                        CTerm::from(ITerm::Type).into(),
                    ),)
                        .into(),
                ),
                ITerm::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
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
                    CTerm::Lam(
                        Named(y.clone(), ()),
                        CTerm::from(Var::Bound(Named(y, Debruijn(0)))).into(),
                    ).into(),
                ),
                ITerm::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : Type => \y : Type => x"),
            ITerm::Lam(
                Named(x.clone(), CTerm::from(ITerm::Type).into()),
                ITerm::Lam(
                    Named(y, CTerm::from(ITerm::Type).into()),
                    ITerm::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                ).into(),
            ).into(),
        );
    }

    #[test]
    fn arrow() {
        assert_eq!(
            parse(r"Type -> Type"),
            ITerm::Pi(
                Named(Name::Abstract, CTerm::from(ITerm::Type).into()),
                CTerm::from(ITerm::Type).into(),
            ).into(),
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
                    CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, CTerm::from(ITerm::Type).into()),
                        CTerm::from(ITerm::Type).into(),
                    ),)
                        .into(),
                ),
                CTerm::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
        );
    }

    #[test]
    fn pi_pi() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"[x : Type] -> [y : Type] -> x"),
            ITerm::Pi(
                Named(x.clone(), CTerm::from(ITerm::Type).into()),
                CTerm::from(ITerm::Pi(
                    Named(y, CTerm::from(ITerm::Type).into()),
                    CTerm::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                ),)
                    .into(),
            ).into(),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"[x : Type] -> x -> x"),
            ITerm::Pi(
                Named(x.clone(), CTerm::from(ITerm::Type).into()),
                CTerm::from(ITerm::Pi(
                    Named(
                        Name::Abstract,
                        CTerm::from(Var::Bound(Named(x.clone(), Debruijn(0)))).into(),
                    ),
                    CTerm::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                ),)
                    .into(),
            ).into(),
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
                    CTerm::from(ITerm::Pi(
                        Named(Name::Abstract, CTerm::from(ITerm::Type).into()),
                        CTerm::from(ITerm::Type).into(),
                    ),)
                        .into(),
                ),
                ITerm::Lam(
                    Named(y.clone(), CTerm::from(ITerm::Type).into()),
                    ITerm::App(
                        ITerm::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                        CTerm::from(Var::Bound(Named(y, Debruijn(0)))).into(),
                    ).into(),
                ).into(),
            ).into(),
        );
    }

    #[test]
    fn id() {
        let x = Name::user("x");
        let a = Name::user("a");

        assert_eq!(
            parse(r"\a : Type => \x : a => x"),
            ITerm::Lam(
                Named(a.clone(), CTerm::from(ITerm::Type).into()),
                ITerm::Lam(
                    Named(
                        x.clone(),
                        CTerm::from(Var::Bound(Named(a, Debruijn(0)))).into(),
                    ),
                    ITerm::from(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            ).into(),
        );
    }

    #[test]
    fn id_ty() {
        let a = Name::user("a");

        assert_eq!(
            parse(r"[a : Type] -> a -> a"),
            ITerm::Pi(
                Named(a.clone(), CTerm::from(ITerm::Type).into()),
                CTerm::from(ITerm::Pi(
                    Named(
                        Name::Abstract,
                        CTerm::from(Var::Bound(Named(a.clone(), Debruijn(0)))).into(),
                    ),
                    CTerm::from(Var::Bound(Named(a, Debruijn(1)))).into(),
                ),)
                    .into(),
            ).into(),
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
