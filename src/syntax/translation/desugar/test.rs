use codespan::{CodeMap, FileName};
use goldenfile::Mint;

use library;
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

    let (concrete_term, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.desugar(&Env::new())
}

mod module {
    use codespan_reporting;
    use codespan_reporting::termcolor::{ColorChoice, StandardStream};

    use super::*;

    #[test]
    fn parse_prelude() {
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), library::PRELUDE.into());
        let writer = StandardStream::stdout(ColorChoice::Always);

        let (concrete_module, errors) = parse::module(&filemap);
        if !errors.is_empty() {
            for error in errors {
                codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic())
                    .unwrap();
            }
            panic!("parse error!")
        }

        concrete_module.desugar(&Env::new());
    }
}

mod term {
    use syntax::raw::{RcTerm, Term};
    use syntax::Level;

    use super::*;

    #[test]
    fn var() {
        golden("var", r"x");
    }

    #[test]
    fn var_kebab_case() {
        golden("var_kebab_case", r"or-elim");
    }

    #[test]
    fn ty() {
        golden("ty", r"Type");
    }

    #[test]
    fn ty_level() {
        golden("ty_level", r"Type 2");
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
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

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
                    RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone()))),
                ),
            )),
        );
    }

    #[test]
    fn lam() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_y = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(y.clone())));
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
                            Scope::new((Binder(y.clone()), Embed(hole())), var_y()),
                        )))
                    ),
                    var_x(),
                ),
            )),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse(r"\(x y : Type) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn arrow() {
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

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
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

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
                    var_x(),
                ),
            )),
        );
    }

    #[test]
    fn pi_pi() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse(r"(x y : Type) -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn pi_arrow() {
        let x = FreeVar::fresh_named("x");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse(r"(x : Type) -> x -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var_x())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn lam_app() {
        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_y = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(y.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

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
                            RcTerm::from(Term::App(var_x(), var_y())),
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
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_a = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(a.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse(r"\(a : Type) (x : a) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(x.clone()), Embed(var_a())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id_ty() {
        let a = FreeVar::fresh_named("a");
        let var_a = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(a.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse(r"(a : Type) -> a -> a"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var_a())), var_a()),
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
                parse(r"(a : Type) (x y z : a) (w : I8) -> x"),
                parse(r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> (w : I8) -> x"),
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
