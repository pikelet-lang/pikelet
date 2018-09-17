use codespan::{CodeMap, FileName};
use goldenfile::Mint;

use std::io::Write;
use syntax::parse;

use super::*;

fn golden(filename: &str, literal: &str) {
    let path = "src/syntax/translation/desugar/goldenfiles";

    let mut mint = Mint::new(path);
    let mut file = mint.new_goldenfile(filename).unwrap();
    let env = DesugarEnv::new(HashMap::new());

    let term = parse(&env, literal);

    write!(file, "{:#?}", term).unwrap();
}

fn parse(env: &DesugarEnv, src: &str) -> raw::RcTerm {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let (concrete_term, _import_paths, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.desugar(env)
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
        let env = DesugarEnv::new(HashMap::new());

        match *parse(&env, r"or-elim").inner {
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(&env, r"\x : Type -> Type => x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let hole = || RcTerm::from(Term::Hole(ByteSpan::default()));

        assert_term_eq!(
            parse(&env, r"\x : (\y => y) => x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(&env, r"\(x y : Type) => x"),
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
        let env = DesugarEnv::new(HashMap::new());

        assert_term_eq!(
            parse(&env, r"Type -> Type"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
            )),
        );
    }

    #[test]
    fn pi() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(&env, r"(x : Type -> Type) -> x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(&env, r"(x y : Type) -> x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse(&env, r"(x : Type) -> x -> x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");

        assert_term_eq!(
            parse(&env, r"\(x : Type -> Type) (y : Type) => x y"),
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
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let a = FreeVar::fresh_named("a");

        assert_term_eq!(
            parse(&env, r"\(a : Type) (x : a) => x"),
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
        let env = DesugarEnv::new(HashMap::new());

        let a = FreeVar::fresh_named("a");

        assert_term_eq!(
            parse(&env, r"(a : Type) -> a -> a"),
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
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse(&env, r"\x (y : Type) z => x"),
                parse(&env, r"\x => \y : Type => \z => x"),
            );
        }

        #[test]
        fn lam_args_multi() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse(&env, r"\(x : Type) (y : Type) z => x"),
                parse(&env, r"\(x y : Type) z => x"),
            );
        }

        #[test]
        fn pi_args() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse(&env, r"(a : Type) -> (x y z : a) -> x"),
                parse(&env, r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> x"),
            );
        }

        #[test]
        fn pi_args_multi() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse(&env, r"(a : Type) (x y z : a) (w : x) -> x"),
                parse(
                    &env,
                    r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> (w : x) -> x"
                ),
            );
        }

        #[test]
        fn arrow() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse(&env, r"(a : Type) -> a -> a"),
                parse(&env, r"(a : Type) -> (x : a) -> a"),
            )
        }

        #[test]
        fn if_then_else() {
            let env = DesugarEnv::new(hashmap!{
                "true".to_owned() => FreeVar::fresh_named("true"),
                "false".to_owned() => FreeVar::fresh_named("false"),
            });

            assert_term_eq!(
                parse(&env, r#"if true then "true" else "false""#),
                parse(&env, r#"case true of { true => "true"; false => "false" }"#),
            )
        }
    }
}
