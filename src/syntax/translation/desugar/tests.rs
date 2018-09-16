use codespan::{CodeMap, FileName};
use goldenfile::Mint;

use std::io::Write;
use syntax::parse;

use super::*;

fn golden(filename: &str, literal: &str) {
    let path = "src/syntax/translation/desugar/goldenfiles";

    let mut mint = Mint::new(path);
    let mut file = mint.new_goldenfile(filename).unwrap();

    let term = parse(literal);

    write!(file, "{:#?}", term).unwrap();
}

fn parse(src: &str) -> raw::RcTerm {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let (concrete_term, _import_paths, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.desugar(&DesugarEnv::new(HashMap::new()))
}

mod term {
    use syntax::raw::{RcTerm, Term};
    use syntax::Level;

    use super::*;

    fn var(x: &FreeVar<String>) -> RcTerm {
        RcTerm::from(Term::Var(
            ByteSpan::default(),
            Var::Free(x.clone()),
            LevelShift(0),
        ))
    }

    fn u0() -> RcTerm {
        RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)))
    }

    #[test]
    fn free_var() {
        match *parse(r"or-elim").inner {
            raw::Term::Var(_, Var::Free(ref free_var), LevelShift(0)) => {
                assert_eq!(free_var.pretty_name, Some("or-elim".to_owned()));
            },
            ref term => panic!("unexpected term: {}", term),
        }
    }

    #[test]
    fn ty() {
        golden("ty", r"Type");
    }

    #[test]
    fn ty_level() {
        golden("ty_level", r"Type^2");
    }

    #[test]
    fn ann() {
        golden("ann", r"Type : Type");
    }

    #[test]
    fn ann_ann_left() {
        golden("ann_ann_left", r"Type : Type : Type");
    }

    #[test]
    fn ann_ann_right() {
        golden("ann_ann_right", r"Type : (Type : Type)");
    }

    #[test]
    fn ann_ann_ann() {
        golden("ann_ann_ann", r"(Type : Type) : (Type : Type)");
    }

    #[test]
    fn lam_ann() {
        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(r"\x : Type -> Type => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    var(&x),
                ),
            )),
        );
    }

    #[test]
    fn lam() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let hole = || RcTerm::from(Term::Hole(ByteSpan::default()));

        assert_term_eq!(
            parse(r"\x : (\y => y) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Lam(
                            ByteSpan::default(),
                            Scope::new((Binder(y.clone()), Embed(hole())), var(&y)),
                        )))
                    ),
                    var(&x),
                ),
            )),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(r"\(x y : Type) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var(&x)),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn arrow() {
        assert_term_eq!(
            parse(r"Type -> Type"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
            )),
        );
    }

    #[test]
    fn pi() {
        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(r"(x : Type -> Type) -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    var(&x),
                ),
            )),
        );
    }

    #[test]
    fn pi_pi() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(r"(x y : Type) -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var(&x)),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(r"(x : Type) -> x -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var(&x))), var(&x)),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn lam_app() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(r"\(x : Type -> Type) (y : Type) => x y"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new(
                            (Binder(y.clone()), Embed(u0())),
                            RcTerm::from(Term::App(var(&x), var(&y))),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id() {
        let x = FreeVar::fresh_named("x");
        let a = FreeVar::fresh_named("a");

        assert_term_eq!(
            parse(r"\(a : Type) (x : a) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(x.clone()), Embed(var(&a))), var(&x)),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id_ty() {
        let a = FreeVar::fresh_named("a");

        assert_term_eq!(
            parse(r"(a : Type) -> a -> a"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var(&a))), var(&a)),
                    )),
                ),
            )),
        );
    }

    mod sugar {
        use super::*;

        #[test]
        fn lam_args() {
            assert_term_eq!(
                parse(r"\x (y : Type) z => x"),
                parse(r"\x => \y : Type => \z => x"),
            );
        }

        #[test]
        fn lam_args_multi() {
            assert_term_eq!(
                parse(r"\(x : Type) (y : Type) z => x"),
                parse(r"\(x y : Type) z => x"),
            );
        }

        #[test]
        fn pi_args() {
            assert_term_eq!(
                parse(r"(a : Type) -> (x y z : a) -> x"),
                parse(r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> x"),
            );
        }

        #[test]
        fn pi_args_multi() {
            assert_term_eq!(
                parse(r"(a : Type) (x y z : a) (w : x) -> x"),
                parse(r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> (w : x) -> x"),
            );
        }

        #[test]
        fn arrow() {
            assert_term_eq!(
                parse(r"(a : Type) -> a -> a"),
                parse(r"(a : Type) -> (x : a) -> a"),
            )
        }
    }
}
