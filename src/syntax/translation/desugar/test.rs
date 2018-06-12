use codespan::{CodeMap, FileName};

use library;
use syntax::parse;

use super::*;

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
        assert_term_eq!(
            parse(r"x"),
            Term::Var(Ignore::default(), Var::Free(Name::user("x"))),
        );
    }

    #[test]
    fn var_kebab_case() {
        assert_term_eq!(
            parse(r"or-elim"),
            Term::Var(Ignore::default(), Var::Free(Name::user("or-elim"))),
        );
    }

    #[test]
    fn ty() {
        assert_term_eq!(parse(r"Type"), Term::Universe(Ignore::default(), Level(0)),);
    }

    #[test]
    fn ty_level() {
        assert_term_eq!(
            parse(r"Type 2"),
            Term::Universe(Ignore::default(), Level(2)),
        );
    }

    #[test]
    fn ann() {
        assert_term_eq!(
            parse(r"Type : Type"),
            Term::Ann(
                Ignore::default(),
                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                Rc::new(Term::Universe(Ignore::default(), Level(0))),
            ),
        );
    }

    #[test]
    fn ann_ann_left() {
        assert_term_eq!(
            parse(r"Type : Type : Type"),
            Term::Ann(
                Ignore::default(),
                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                Rc::new(Term::Ann(
                    Ignore::default(),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                )),
            ),
        );
    }

    #[test]
    fn ann_ann_right() {
        assert_term_eq!(
            parse(r"Type : (Type : Type)"),
            Term::Ann(
                Ignore::default(),
                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                Rc::new(Term::Ann(
                    Ignore::default(),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                )),
            ),
        );
    }

    #[test]
    fn ann_ann_ann() {
        assert_term_eq!(
            parse(r"(Type : Type) : (Type : Type)"),
            Rc::new(Term::Ann(
                Ignore::default(),
                Rc::new(Term::Ann(
                    Ignore::default(),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                )),
                Rc::new(Term::Ann(
                    Ignore::default(),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                    Rc::new(Term::Universe(Ignore::default(), Level(0))),
                )),
            )),
        );
    }

    #[test]
    fn lam_ann() {
        assert_term_eq!(
            parse(r"\x : Type -> Type => x"),
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                ),
            )),
        );
    }

    #[test]
    fn lam() {
        assert_term_eq!(
            parse(r"\x : (\y => y) => x"),
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Lam(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("y"),
                                    Embed(Rc::new(Term::Hole(Ignore::default()))),
                                ),
                                Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("y")),)),
                            ),
                        )),),
                    ),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                )
            )),
        );
    }

    #[test]
    fn lam_lam_ann() {
        assert_term_eq!(
            parse(r"\(x y : Type) => x"),
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Lam(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("y"),
                                Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                        ),
                    )),
                )
            )),
        );
    }

    #[test]
    fn arrow() {
        assert_term_eq!(
            parse(r"Type -> Type"),
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("_"),
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
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                ),
            )),
        );
    }

    #[test]
    fn pi_pi() {
        assert_term_eq!(
            parse(r"(x y : Type) -> x"),
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Pi(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("y"),
                                Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn pi_arrow() {
        assert_term_eq!(
            parse(r"(x : Type) -> x -> x"),
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Pi(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("_"),
                                Embed(Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("x")),
                                ))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
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
                nameless::bind(
                    (
                        Name::user("x"),
                        Embed(Rc::new(Term::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("_"),
                                    Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(Term::Universe(Ignore::default(), Level(0))),
                            ),
                        ))),
                    ),
                    Rc::new(Term::Lam(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("y"),
                                Embed(Rc::new(Term::Universe(Ignore::default(), Level(0),))),
                            ),
                            Rc::new(Term::App(
                                Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                                Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("y")),)),
                            )),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id() {
        let a = Name::user("a");

        assert_term_eq!(
            parse(r"\(a : Type) (x : a) => x"),
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind(
                    (
                        a.clone(),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Lam(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("x"),
                                Embed(Rc::new(Term::Var(Ignore::default(), Var::Free(a),))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id_ty() {
        assert_term_eq!(
            parse(r"(a : Type) -> a -> a"),
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind(
                    (
                        Name::user("a"),
                        Embed(Rc::new(Term::Universe(Ignore::default(), Level(0)))),
                    ),
                    Rc::new(Term::Pi(
                        Ignore::default(),
                        nameless::bind(
                            (
                                Name::user("_"),
                                Embed(Rc::new(Term::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("a")),
                                ))),
                            ),
                            Rc::new(Term::Var(Ignore::default(), Var::Free(Name::user("a")),)),
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
