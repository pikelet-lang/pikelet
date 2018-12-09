use codespan::{ByteIndex, ByteSpan};
use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};
use pretty_assertions::assert_eq;

use pikelet_concrete::resugar::{Resugar, ResugarEnv};
use pikelet_concrete::syntax::concrete;
use pikelet_core::syntax::{core, Label, LevelShift, Literal};

fn span() -> ByteSpan {
    ByteSpan::default()
}

fn index() -> ByteIndex {
    ByteIndex::default()
}

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
    let core_term = core::Term::Literal(Literal::Bool(true));
    let concrete_term = concrete::Term::Name(span(), "true".to_owned(), None);

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn lit_bool_false() {
    let core_term = core::Term::Literal(Literal::Bool(false));
    let concrete_term = concrete::Term::Name(span(), "false".to_owned(), None);

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn lit_string() {
    let core_term = core::Term::Literal(Literal::String("hello".to_owned()));
    let concrete_term =
        concrete::Term::Literal(concrete::Literal::String(span(), "hello".to_owned()));

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn var() {
    let free_var = FreeVar::fresh_named("x");
    let mut env = ResugarEnv::new();
    env.on_item(&Label("x".to_owned()), &Binder(free_var.clone()));

    let core_term = core::Term::var(Var::Free(free_var), 0);
    let concrete_term = concrete::Term::Name(span(), "x".to_owned(), None);

    assert_eq!(core_term.resugar(&env), concrete_term);
}

#[test]
fn var_shadow_keyword() {
    let free_var = FreeVar::fresh_named("if");
    let mut env = ResugarEnv::new();
    env.on_item(&Label("if".to_owned()), &Binder(free_var.clone()));

    let core_term = core::Term::var(Var::Free(free_var), 0);
    let concrete_term = concrete::Term::Name(span(), "if1".to_owned(), None);

    assert_eq!(core_term.resugar(&env), concrete_term);
}

#[test]
fn import() {
    let core_term = core::Term::Import("type".to_owned());
    let concrete_term = concrete::Term::Import(span(), span(), "type".to_owned());

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

// TODO: core::Term::Pi

#[test]
fn arrow() {
    let core_term = core::RcTerm::from(core::Term::FunType(Scope::new(
        (
            Binder(FreeVar::fresh_unnamed()),
            Embed(core::RcTerm::from(core::RcTerm::from(
                core::Term::universe(0),
            ))),
        ),
        core::RcTerm::from(core::RcTerm::from(core::Term::universe(0))),
    )));

    let concrete_term = concrete::Term::FunArrow(
        Box::new(concrete::Term::Universe(span(), None)),
        Box::new(concrete::Term::Universe(span(), None)),
    );

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn arrow_parens() {
    let core_term = core::Term::FunType(Scope::new(
        (
            Binder(FreeVar::fresh_unnamed()),
            Embed(core::RcTerm::from(core::Term::FunType(Scope::new(
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

    let concrete_term = concrete::Term::FunArrow(
        Box::new(concrete::Term::Parens(
            span(),
            Box::new(concrete::Term::FunArrow(
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
fn let_shadow_keyword() {
    let var_else1 = FreeVar::fresh_named("else");
    let var_else2 = FreeVar::fresh_named("else");

    let core_module = core::Term::Let(Scope::new(
        Nest::new(vec![
            (
                Binder(var_else1.clone()),
                Embed(core::RcTerm::from(core::Term::universe(0))),
            ),
            (
                Binder(var_else2.clone()),
                Embed(core::RcTerm::from(core::Term::universe(0))),
            ),
        ]),
        core::RcTerm::from(core::Term::RecordIntro(vec![])),
    ));

    let concrete_module = concrete::Term::Let(
        index(),
        vec![
            concrete::Item::Definition {
                name: (index(), "else1".to_owned()),
                params: vec![],
                return_ann: None,
                body: concrete::Term::Universe(span(), None),
            },
            concrete::Item::Definition {
                name: (index(), "else2".to_owned()),
                params: vec![],
                return_ann: None,
                body: concrete::Term::Universe(span(), None),
            },
        ],
        Box::new(concrete::Term::RecordIntro(span(), vec![])),
    );

    assert_eq!(core_module.resugar(&ResugarEnv::new()), concrete_module);
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
                Embed(core::RcTerm::from(core::RcTerm::from(core::Term::var(
                    Var::Free(var_string),
                    0,
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
                ann: concrete::Term::Name(span(), "String1".to_owned(), None),
            },
        ],
    );

    assert_eq!(core_term.resugar(&env), concrete_term);
}

// TODO: core::Term::Record

#[test]
fn record_empty() {
    let core_term = core::Term::RecordIntro(vec![]);
    let concrete_term = concrete::Term::RecordIntro(span(), vec![]);

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn record_proj_atomic() {
    let core_term = core::Term::RecordProj(
        core::RcTerm::from(core::RcTerm::from(core::Term::universe(0))),
        Label("hello".to_owned()),
        LevelShift(0),
    );

    let concrete_term = concrete::Term::RecordProj(
        span(),
        Box::new(concrete::Term::Universe(span(), None)),
        index(),
        "hello".to_owned(),
        None,
    );

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

#[test]
fn record_proj_fun_app() {
    let core_term = core::Term::RecordProj(
        core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
        Label("hello".to_owned()),
        LevelShift(0),
    );

    let concrete_term = concrete::Term::RecordProj(
        span(),
        Box::new(concrete::Term::Parens(
            span(),
            Box::new(concrete::Term::Universe(span(), Some(1))),
        )),
        index(),
        "hello".to_owned(),
        None,
    );

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}

// TODO: core::Term::Case

#[test]
fn array_intro() {
    let core_term = core::Term::ArrayIntro(vec![
        core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
        core::RcTerm::from(core::RcTerm::from(core::Term::universe(1))),
    ]);

    let concrete_term = concrete::Term::ArrayIntro(
        span(),
        vec![
            concrete::Term::Universe(span(), Some(1)),
            concrete::Term::Universe(span(), Some(1)),
        ],
    );

    assert_eq!(core_term.resugar(&ResugarEnv::new()), concrete_term);
}
