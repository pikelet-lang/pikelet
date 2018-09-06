use moniker::{FreeVar, Nest};

use super::*;

fn span() -> ByteSpan {
    ByteSpan::default()
}

fn index() -> ByteIndex {
    ByteIndex::default()
}

mod module {
    use super::*;

    #[test]
    fn shadow_keyword() {
        let var_else1 = FreeVar::fresh_named("else");
        let var_else2 = FreeVar::fresh_named("else");

        let core_module = core::Module {
            items: vec![
                core::Item::Declaration {
                    label: Label("else".to_owned()),
                    binder: Binder(var_else1.clone()),
                    term: core::RcTerm::from(core::Term::universe(1)),
                },
                core::Item::Definition {
                    label: Label("else".to_owned()),
                    binder: Binder(var_else1.clone()),
                    term: core::RcTerm::from(core::Term::universe(0)),
                },
                // This shouldn't happen, but let's test what happens anyway!
                core::Item::Declaration {
                    label: Label("else".to_owned()),
                    binder: Binder(var_else2.clone()),
                    term: core::RcTerm::from(core::Term::universe(1)),
                },
                core::Item::Definition {
                    label: Label("else".to_owned()),
                    binder: Binder(var_else2.clone()),
                    term: core::RcTerm::from(core::Term::universe(0)),
                },
            ],
        };

        let concrete_module = concrete::Module::Valid {
            items: vec![
                concrete::Item::Declaration {
                    name: (index(), "else1".to_owned()),
                    ann: concrete::Term::Universe(span(), Some(1)),
                },
                concrete::Item::Definition {
                    name: (index(), "else1".to_owned()),
                    params: vec![],
                    return_ann: None,
                    body: concrete::Term::Universe(span(), None),
                },
                concrete::Item::Declaration {
                    name: (index(), "else2".to_owned()),
                    ann: concrete::Term::Universe(span(), Some(1)),
                },
                concrete::Item::Definition {
                    name: (index(), "else2".to_owned()),
                    params: vec![],
                    return_ann: None,
                    body: concrete::Term::Universe(span(), None),
                },
            ],
        };

        assert_eq!(core_module.resugar(&ResugarEnv::new()), concrete_module);
    }

    // TODO: moare tests
}

mod term {
    use super::*;

    #[test]
    fn ann() {
        let core_term = core::Term::Ann(
            core::RcTerm::from(core::Term::universe(0)),
            core::RcTerm::from(core::Term::universe(0)),
        );

        let concrete_term = concrete::Term::Ann(
            Box::new(concrete::Term::Universe(span(), None)),
            Box::new(concrete::Term::Universe(span(), None)),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn universe0() {
        let core_term = core::Term::universe(0);
        let concrete_term = concrete::Term::Universe(span(), None);

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn universe1() {
        let core_term = core::Term::universe(1);
        let concrete_term = concrete::Term::Universe(span(), Some(1));

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    // TODO: core::Term::Literal

    #[test]
    fn lit_bool_true() {
        let core_term = core::Term::Literal(core::Literal::Bool(true));
        let concrete_term = concrete::Term::Name(index(), "true".to_owned());

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn lit_bool_false() {
        let core_term = core::Term::Literal(core::Literal::Bool(false));
        let concrete_term = concrete::Term::Name(index(), "false".to_owned());

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn lit_string() {
        let core_term = core::Term::Literal(core::Literal::String("hello".to_owned()));
        let concrete_term =
            concrete::Term::Literal(concrete::Literal::String(span(), "hello".to_owned()));

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn var() {
        let free_var = FreeVar::fresh_named("x");
        let mut env = ResugarEnv::new();
        env.on_item(&Label("x".to_owned()), &Binder(free_var.clone()));

        let core_term = core::Term::Var(Var::Free(free_var));
        let concrete_term = concrete::Term::Name(index(), "x".to_owned());

        assert_eq!(core_term.resugar(&env), concrete_term);
    }

    #[test]
    fn var_shadow_keyword() {
        let free_var = FreeVar::fresh_named("if");
        let mut env = ResugarEnv::new();
        env.on_item(&Label("if".to_owned()), &Binder(free_var.clone()));

        let core_term = core::Term::Var(Var::Free(free_var));
        let concrete_term = concrete::Term::Name(index(), "if1".to_owned());

        assert_eq!(core_term.resugar(&env), concrete_term);
    }

    #[test]
    fn extern_() {
        let core_term = core::Term::Extern(
            "type".to_owned(),
            core::RcTerm::from(core::Term::universe(0)),
        );

        let concrete_term = concrete::Term::Extern(
            span(),
            span(),
            "type".to_owned(),
            Box::new(concrete::Term::Universe(span(), None)),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    // TODO: core::Term::Pi

    #[test]
    fn arrow() {
        let core_term = core::RcTerm::from(core::Term::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(core::RcTerm::from(core::RcTerm::from(
                    core::Term::universe(0),
                ))),
            ),
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(0))),
        )));

        let concrete_term = concrete::Term::Arrow(
            Box::new(concrete::Term::Universe(span(), None)),
            Box::new(concrete::Term::Universe(span(), None)),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn arrow_parens() {
        let core_term = core::Term::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(core::RcTerm::from(core::Term::Pi(Scope::new(
                    (
                        Binder(FreeVar::fresh_unnamed()),
                        Embed(core::RcTerm::from(core::RcTerm::from(
                            core::Term::universe(0),
                        ))),
                    ),
                    core::RcTerm::from(core::RcTerm::from(core::Term::universe(0))),
                )))),
            ),
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
        ));

        let concrete_term = concrete::Term::Arrow(
            Box::new(concrete::Term::Parens(
                span(),
                Box::new(concrete::Term::Arrow(
                    Box::new(concrete::Term::Universe(span(), None)),
                    Box::new(concrete::Term::Universe(span(), None)),
                )),
            )),
            Box::new(concrete::Term::Universe(span(), Some(1))),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    // TODO: core::Term::Lam
    // TODO: core::Term::App

    #[test]
    fn if_else() {
        let core_term = core::Term::If(
            core::RcTerm::from(core::Term::Literal(core::Literal::Bool(false))),
            core::RcTerm::from(core::Term::Literal(core::Literal::String(
                "hello".to_owned(),
            ))),
            core::RcTerm::from(core::Term::Literal(core::Literal::String("bye".to_owned()))),
        );
        let concrete_term = concrete::Term::If(
            index(),
            Box::new(concrete::Term::Name(index(), "false".to_owned())),
            Box::new(concrete::Term::Literal(concrete::Literal::String(
                span(),
                "hello".to_owned(),
            ))),
            Box::new(concrete::Term::Literal(concrete::Literal::String(
                span(),
                "bye".to_owned(),
            ))),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn record_ty_empty() {
        let core_term = core::Term::RecordType(Scope::new(Nest::new(vec![]), ()));
        let concrete_term = concrete::Term::RecordType(span(), vec![]);

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn record_ty() {
        let mut env = ResugarEnv::new();
        env.on_item(
            &Label("String".to_owned()),
            &Binder(FreeVar::fresh_named("String")),
        );

        let var_string = FreeVar::fresh_named("String");
        let var_x = FreeVar::fresh_named("x");

        let core_term = core::Term::RecordType(Scope::new(
            Nest::new(vec![
                (
                    Label("String".to_owned()),
                    Binder(var_string.clone()),
                    Embed(core::RcTerm::from(core::RcTerm::from(
                        core::Term::universe(0),
                    ))),
                ),
                (
                    Label("x".to_owned()),
                    Binder(var_x.clone()),
                    Embed(core::RcTerm::from(core::RcTerm::from(core::Term::Var(
                        Var::Free(var_string),
                    )))),
                ),
            ]),
            (),
        ));
        let concrete_term = concrete::Term::RecordType(
            span(),
            vec![
                concrete::RecordTypeField {
                    label: (index(), "String".to_owned()),
                    binder: Some((index(), "String1".to_owned())),
                    ann: concrete::Term::Universe(span(), None),
                },
                concrete::RecordTypeField {
                    label: (index(), "x".to_owned()),
                    binder: None,
                    ann: concrete::Term::Name(index(), "String1".to_owned()),
                },
            ],
        );

        assert_eq!(core_term.resugar(&env), concrete_term);
    }

    // TODO: core::Term::Record

    #[test]
    fn record_empty() {
        let core_term = core::Term::Record(Scope::new(Nest::new(vec![]), ()));
        let concrete_term = concrete::Term::Record(span(), vec![]);

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn proj_atomic() {
        let core_term = core::Term::Proj(
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(0))),
            Label("hello".to_owned()),
        );

        let concrete_term = concrete::Term::Proj(
            Box::new(concrete::Term::Universe(span(), None)),
            index(),
            "hello".to_owned(),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    #[test]
    fn proj_app() {
        let core_term = core::Term::Proj(
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
            Label("hello".to_owned()),
        );

        let concrete_term = concrete::Term::Proj(
            Box::new(concrete::Term::Parens(
                span(),
                Box::new(concrete::Term::Universe(span(), Some(1))),
            )),
            index(),
            "hello".to_owned(),
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }

    // TODO: core::Term::Case

    #[test]
    fn array() {
        let core_term = core::Term::Array(vec![
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
            core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
        ]);

        let concrete_term = concrete::Term::Array(
            span(),
            vec![
                concrete::Term::Universe(span(), Some(1)),
                concrete::Term::Universe(span(), Some(1)),
            ],
        );

        assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
    }
}
