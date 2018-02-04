use super::*;

fn parse(src: &str) -> RcTerm {
    RcTerm::from_concrete(&src.parse().unwrap())
}

mod alpha_eq {
    use super::*;

    #[test]
    fn var() {
        assert_eq!(parse(r"x"), parse(r"x"));
    }

    #[test]
    #[should_panic]
    fn var_diff() {
        assert_eq!(parse(r"x"), parse(r"y"));
    }

    #[test]
    fn ty() {
        assert_eq!(parse(r"Type"), parse(r"Type"));
    }

    #[test]
    fn lam() {
        assert_eq!(parse(r"\x : Type => x"), parse(r"\a : Type => a"));
    }

    #[test]
    fn pi() {
        assert_eq!(parse(r"(x : Type) -> x"), parse(r"(a : Type) -> a"));
    }

    #[test]
    fn lam_app() {
        assert_eq!(
            parse(r"\x : Type -> Type => x Type"),
            parse(r"\a : Type -> Type => a Type")
        );
    }

    #[test]
    fn pi_app() {
        assert_eq!(
            parse(r"(x : Type -> Type) -> x Type"),
            parse(r"(a : Type -> Type) -> a Type")
        );
    }

    #[test]
    fn lam_lam_app() {
        assert_eq!(
            parse(r"\x : Type -> Type => \y : Type => x y"),
            parse(r"\a : Type -> Type => \b : Type => a b"),
        );
    }

    #[test]
    fn pi_pi_app() {
        assert_eq!(
            parse(r"(x : Type -> Type) -> (y : Type) -> x y"),
            parse(r"(a : Type -> Type) -> (b : Type) -> a b"),
        );
    }
}

mod from_concrete {
    use super::*;

    #[test]
    fn parse_prelude() {
        Module::from_concrete(&include_str!("../../../prelude.lp").parse().unwrap());
    }

    #[test]
    fn var() {
        assert_eq!(parse(r"x"), Term::from(Var::Free(Name::user("x"))).into());
    }

    #[test]
    fn var_kebab_case() {
        assert_eq!(
            parse(r"or-elim"),
            Term::from(Var::Free(Name::user("or-elim"))).into(),
        );
    }

    #[test]
    fn ty() {
        assert_eq!(parse(r"Type"), Term::Universe.into());
    }

    #[test]
    fn ann() {
        assert_eq!(
            parse(r"Type : Type"),
            Term::Ann(
                Term::from(Term::Universe).into(),
                Term::from(Term::Universe).into(),
            ).into(),
        );
    }

    #[test]
    fn ann_ann_left() {
        assert_eq!(
            parse(r"Type : Type : Type"),
            Term::Ann(
                Term::from(Term::Ann(
                    Term::from(Term::Universe).into(),
                    Term::from(Term::Universe).into(),
                )).into(),
                Term::from(Term::Universe).into(),
            ).into(),
        );
    }

    #[test]
    fn ann_ann_right() {
        assert_eq!(
            parse(r"Type : (Type : Type)"),
            Term::Ann(
                Term::from(Term::Universe).into(),
                Term::from(Term::Ann(
                    Term::from(Term::Universe).into(),
                    Term::from(Term::Universe).into(),
                )).into(),
            ).into(),
        );
    }

    #[test]
    fn ann_ann_ann() {
        assert_eq!(
            parse(r"(Type : Type) : (Type : Type)"),
            Term::Ann(
                Term::from(Term::Ann(
                    Term::from(Term::Universe).into(),
                    Term::from(Term::Universe).into(),
                )).into(),
                Term::from(Term::Ann(
                    Term::from(Term::Universe).into(),
                    Term::from(Term::Universe).into(),
                )).into(),
            ).into(),
        );
    }

    #[test]
    fn lam_ann() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"\x : Type -> Type => x"),
            Term::Lam(
                Named(
                    x.clone(),
                    Some(
                        Term::from(Term::Pi(
                            Named(Name::Abstract, Term::from(Term::Universe).into()),
                            Term::from(Term::Universe).into(),
                        )).into()
                    ),
                ),
                Term::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
        );
    }

    #[test]
    fn lam() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : (\y => y) => x"),
            Term::Lam(
                Named(
                    x.clone(),
                    Some(
                        Term::Lam(
                            Named(y.clone(), None),
                            Term::from(Var::Bound(Named(y, Debruijn(0)))).into(),
                        ).into()
                    ),
                ),
                Term::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
        );
    }

    #[test]
    fn lam_multi() {
        assert_eq!(
            parse(r"\y (x : Type) z => x"),
            parse(r"\y => \x : Type => \z => x"),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : Type => \y : Type => x"),
            Term::Lam(
                Named(x.clone(), Some(Term::from(Term::Universe).into())),
                Term::Lam(
                    Named(y, Some(Term::from(Term::Universe).into())),
                    Term::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                ).into(),
            ).into(),
        );
    }

    #[test]
    fn arrow() {
        assert_eq!(
            parse(r"Type -> Type"),
            Term::Pi(
                Named(Name::Abstract, Term::from(Term::Universe).into()),
                Term::from(Term::Universe).into(),
            ).into(),
        );
    }

    #[test]
    fn pi() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"(x : Type -> Type) -> x"),
            Term::Pi(
                Named(
                    x.clone(),
                    Term::from(Term::Pi(
                        Named(Name::Abstract, Term::from(Term::Universe).into()),
                        Term::from(Term::Universe).into(),
                    )).into(),
                ),
                Term::from(Var::Bound(Named(x, Debruijn(0)))).into(),
            ).into(),
        );
    }

    #[test]
    fn pi_pi() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"(x : Type) -> (y : Type) -> x"),
            Term::Pi(
                Named(x.clone(), Term::from(Term::Universe).into()),
                Term::from(Term::Pi(
                    Named(y, Term::from(Term::Universe).into()),
                    Term::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                )).into(),
            ).into(),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = Name::user("x");

        assert_eq!(
            parse(r"(x : Type) -> x -> x"),
            Term::Pi(
                Named(x.clone(), Term::from(Term::Universe).into()),
                Term::from(Term::Pi(
                    Named(
                        Name::Abstract,
                        Term::from(Var::Bound(Named(x.clone(), Debruijn(0)))).into(),
                    ),
                    Term::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                )).into(),
            ).into(),
        );
    }

    #[test]
    fn lam_app() {
        let x = Name::user("x");
        let y = Name::user("y");

        assert_eq!(
            parse(r"\x : (Type -> Type) => \y : Type => x y"),
            Term::Lam(
                Named(
                    x.clone(),
                    Some(
                        Term::from(Term::Pi(
                            Named(Name::Abstract, Term::from(Term::Universe).into()),
                            Term::from(Term::Universe).into(),
                        )).into(),
                    ),
                ),
                Term::Lam(
                    Named(y.clone(), Some(Term::from(Term::Universe).into())),
                    Term::App(
                        Term::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                        Term::from(Var::Bound(Named(y, Debruijn(0)))).into(),
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
            Term::Lam(
                Named(a.clone(), Some(Term::from(Term::Universe).into())),
                Term::Lam(
                    Named(
                        x.clone(),
                        Some(Term::from(Var::Bound(Named(a, Debruijn(0)))).into()),
                    ),
                    Term::from(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            ).into(),
        );
    }

    #[test]
    fn id_ty() {
        let a = Name::user("a");

        assert_eq!(
            parse(r"(a : Type) -> a -> a"),
            Term::Pi(
                Named(a.clone(), Term::from(Term::Universe).into()),
                Term::from(Term::Pi(
                    Named(
                        Name::Abstract,
                        Term::from(Var::Bound(Named(a.clone(), Debruijn(0)))).into(),
                    ),
                    Term::from(Var::Bound(Named(a, Debruijn(1)))).into(),
                )).into(),
            ).into(),
        );
    }

    #[test]
    fn id_ty_arr() {
        assert_eq!(
            parse(r"(a : Type) -> a -> a"),
            parse(r"(a : Type) -> (x : a) -> a"),
        )
    }
}
