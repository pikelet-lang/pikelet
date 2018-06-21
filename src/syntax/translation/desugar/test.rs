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

fn parse(src: &str) -> raw::Term {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let (concrete_term, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.desugar()
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

        concrete_module.desugar();
    }
}

mod term {
    use super::*;

    use syntax::raw::Term;
    use syntax::Level;

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
        assert_term_eq!(
            parse(r"\x : Type -> Type => x"),
            Rc::new(Term::Lam(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            Scope::new(
                                (
                                    FreeVar::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(FreeVar::user("x")),)),
                ),
            )),
        );
    }

    #[test]
    fn lam() {
        golden("lam", r"\x : (\y => y) => x");
    }

    #[test]
    fn lam_lam_ann() {
        golden("lam_lam_ann", r"\(x y : Type) => x");
    }

    #[test]
    fn arrow() {
        assert_term_eq!(
            parse(r"Type -> Type"),
            Rc::new(Term::Pi(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("_"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                ),
            )),
        );
    }

    #[test]
    fn pi() {
        assert_term_eq!(
            parse(r"(x : Type -> Type) -> x"),
            Rc::new(Term::Pi(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            Scope::new(
                                (
                                    FreeVar::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(FreeVar::user("x")),)),
                ),
            )),
        );
    }

    #[test]
    fn pi_pi() {
        golden("pi_pi", r"(x y : Type) -> x");
    }

    #[test]
    fn pi_arrow() {
        assert_term_eq!(
            parse(r"(x : Type) -> x -> x"),
            Rc::new(Term::Pi(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("x"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Pi(
                        Ignore::default(),
                        Scope::new(
                            (
                                FreeVar::user("_"),
                                Embed(Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(FreeVar::user("x")),
                                ))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(FreeVar::user("x")),)),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn lam_app() {
        assert_term_eq!(
            parse(r"\(x : Type -> Type) (y : Type) => x y"),
            Rc::new(Term::Lam(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            Scope::new(
                                (
                                    FreeVar::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Lam(
                        Ignore::default(),
                        Scope::new(
                            (
                                FreeVar::user("y"),
                                Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                            ),
                            Rc::new(Term::App(
                                Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(FreeVar::user("x")),
                                )),
                                Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(FreeVar::user("y")),
                                )),
                            )),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id() {
        golden("id", r"\(a : Type) (x : a) => x");
    }

    #[test]
    fn id_ty() {
        assert_term_eq!(
            parse(r"(a : Type) -> a -> a"),
            Rc::new(Term::Pi(
                Ignore::default(),
                Scope::new(
                    (
                        FreeVar::user("a"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Pi(
                        Ignore::default(),
                        Scope::new(
                            (
                                FreeVar::user("_"),
                                Embed(Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(FreeVar::user("a")),
                                ))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(FreeVar::user("a")),)),
                        ),
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
