use codespan::{ByteIndex, ByteOffset, ByteSpan};
use nameless::{self, Embed, GenId, Ignore, Name, Var};
use std::rc::Rc;

use syntax::concrete;
use syntax::core;

/// Translate something to the corresponding core representation
pub trait Desugar<T> {
    fn desugar(&self) -> T;
}

/// Convert a sugary pi type from something like:
///
/// ```text
/// (a b : t1) -> t3
/// ```
///
/// To a bunch of nested pi types like:
///
/// ```text
/// (a : t1) -> (b : t1) -> t3
/// ```
fn desugar_pi(
    params: &[(Vec<(ByteIndex, String)>, concrete::Term)],
    body: &concrete::Term,
) -> core::RawTerm {
    let mut term = body.desugar();

    for &(ref names, ref ann) in params.iter().rev() {
        let ann = Rc::new(ann.desugar());
        for &(start, ref name) in names.iter().rev() {
            // This could be wrong... :/
            term = core::RawTerm::Pi(
                Ignore(ByteSpan::new(start, term.span().end())),
                nameless::bind(
                    (Name::user(name.clone()), Embed(ann.clone())),
                    Rc::new(term),
                ),
            );
        }
    }

    term
}

/// Convert a sugary lambda from something like:
///
/// ```text
/// \(a b : t1) c (d : t2) => t3
/// ```
///
/// To a bunch of nested lambdas like:
///
/// ```text
/// \(a : t1) => \(b : t1) => \c => \(d : t2) => t3
/// ```
fn desugar_lam(
    params: &[(Vec<(ByteIndex, String)>, Option<Box<concrete::Term>>)],
    body: &concrete::Term,
) -> core::RawTerm {
    let mut term = body.desugar();

    for &(ref names, ref ann) in params.iter().rev() {
        for &(start, ref name) in names.iter().rev() {
            let name = Name::user(name.clone());
            let ann = match *ann {
                None => Rc::new(core::RawTerm::Hole(Ignore::default())),
                Some(ref ann) => Rc::new(ann.desugar()),
            };

            term = core::RawTerm::Lam(
                Ignore(ByteSpan::new(start, term.span().end())),
                nameless::bind((name, Embed(ann)), Rc::new(term)),
            );
        }
    }

    term
}

fn desugar_app(fn_expr: &concrete::Term, args: &[concrete::Term]) -> core::RawTerm {
    let mut term = fn_expr.desugar();

    for arg in args.iter() {
        term = core::RawTerm::App(Rc::new(term), Rc::new(arg.desugar()))
    }

    term
}

fn desugar_record_ty(
    span: ByteSpan,
    fields: &[(ByteIndex, String, Box<concrete::Term>)],
) -> core::RawTerm {
    let mut term = core::RawTerm::EmptyRecordType(Ignore(ByteSpan::new(span.end(), span.end())));

    for &(start, ref label, ref ann) in fields.iter().rev() {
        term = core::RawTerm::RecordType(
            Ignore(ByteSpan::new(start, term.span().end())),
            core::Label(label.clone()),
            Rc::new(ann.desugar()),
            Rc::new(term),
        );
    }

    term
}

fn desugar_record(
    span: ByteSpan,
    fields: &[(ByteIndex, String, Box<concrete::Term>)],
) -> core::RawTerm {
    let mut term = core::RawTerm::EmptyRecord(Ignore(ByteSpan::new(span.end(), span.end())));

    for &(start, ref label, ref value) in fields.iter().rev() {
        term = core::RawTerm::Record(
            Ignore(ByteSpan::new(start, term.span().end())),
            core::Label(label.clone()),
            Rc::new(value.desugar()),
            Rc::new(term),
        );
    }

    term
}

impl Desugar<core::RawModule> for concrete::Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    fn desugar(&self) -> core::RawModule {
        match *self {
            concrete::Module::Valid {
                ref name,
                ref declarations,
            } => {
                // The type claims that we have encountered so far! We'll use these when
                // we encounter their corresponding definitions later as type annotations
                let mut prev_claim = None;
                // The definitions, desugared from the concrete syntax
                let mut definitions = Vec::<core::RawDefinition>::new();

                for declaration in declarations {
                    match *declaration {
                        concrete::Declaration::Import { .. } => {
                            unimplemented!("import declarations")
                        },
                        concrete::Declaration::Claim {
                            name: (_, ref name),
                            ref ann,
                            ..
                        } => match prev_claim.take() {
                            Some((name, ann)) => {
                                let term = Rc::new(core::RawTerm::Hole(Ignore::default()));
                                definitions.push(core::RawDefinition { name, term, ann });
                            },
                            None => prev_claim = Some((name.clone(), Rc::new(ann.desugar()))),
                        },
                        concrete::Declaration::Definition {
                            ref name,
                            ref params,
                            ref body,
                            ref wheres,
                            ..
                        } => {
                            let default_span = Ignore::default();

                            if !wheres.is_empty() {
                                unimplemented!("where clauses");
                            }

                            match prev_claim.take() {
                                None => definitions.push(core::RawDefinition {
                                    name: name.clone(),
                                    ann: Rc::new(core::RawTerm::Hole(default_span)),
                                    term: Rc::new(desugar_lam(params, body)),
                                }),
                                Some((claim_name, ann)) => {
                                    if claim_name == *name {
                                        definitions.push(core::RawDefinition {
                                            name: name.clone(),
                                            ann,
                                            term: Rc::new(desugar_lam(params, body)),
                                        });
                                    } else {
                                        definitions.push(core::RawDefinition {
                                            name: claim_name.clone(),
                                            ann,
                                            term: Rc::new(core::RawTerm::Hole(default_span)),
                                        });
                                        definitions.push(core::RawDefinition {
                                            name: name.clone(),
                                            ann: Rc::new(core::RawTerm::Hole(default_span)),
                                            term: Rc::new(desugar_lam(params, body)),
                                        });
                                    }
                                },
                            };
                        },
                        concrete::Declaration::Error(_) => unimplemented!("error recovery"),
                    }
                }

                core::RawModule {
                    name: name.1.clone(),
                    definitions,
                }
            },
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<core::RawTerm> for concrete::Term {
    /// Convert a term in the concrete syntax into a core term
    fn desugar(&self) -> core::RawTerm {
        let span = Ignore(self.span());
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = Rc::new(expr.desugar());
                let ty = Rc::new(ty.desugar());

                core::RawTerm::Ann(span, expr, ty)
            },
            concrete::Term::Universe(_, level) => {
                core::RawTerm::Universe(span, core::Level(level.unwrap_or(0)))
            },
            concrete::Term::Literal(_, ref lit) => {
                let c = match *lit {
                    concrete::Literal::String(ref value) => {
                        core::RawConstant::String(value.clone())
                    },
                    concrete::Literal::Char(value) => core::RawConstant::Char(value),
                    concrete::Literal::Int(value) => core::RawConstant::Int(value),
                    concrete::Literal::Float(value) => core::RawConstant::Float(value),
                };

                core::RawTerm::Constant(span, c)
            },
            concrete::Term::Hole(_) => core::RawTerm::Hole(span),
            concrete::Term::Var(_, ref x) => {
                core::RawTerm::Var(span, Var::Free(Name::user(x.clone())))
            },
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(params, body),
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = Name::from(GenId::fresh());
                let ann = Rc::new(ann.desugar());
                let body = Rc::new(body.desugar());

                core::RawTerm::Pi(span, nameless::bind((name, Embed(ann)), body))
            },
            concrete::Term::App(ref fn_expr, ref args) => desugar_app(fn_expr, args),
            concrete::Term::Let(_, ref _declarations, ref _body) => unimplemented!("let bindings"),
            concrete::Term::If(start, ref cond, ref if_true, ref if_false) => core::RawTerm::If(
                Ignore(start),
                Rc::new(cond.desugar()),
                Rc::new(if_true.desugar()),
                Rc::new(if_false.desugar()),
            ),
            concrete::Term::RecordType(span, ref fields) => desugar_record_ty(span, fields),
            concrete::Term::Record(span, ref fields) => desugar_record(span, fields),
            concrete::Term::Proj(ref tm, label_start, ref label) => {
                let label_span = Ignore(ByteSpan::from_offset(
                    label_start,
                    ByteOffset::from_str(label),
                ));

                core::RawTerm::Proj(
                    span,
                    Rc::new(tm.desugar()),
                    label_span,
                    core::Label(label.clone()),
                )
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}

#[cfg(test)]
mod desugar {
    use codespan::{CodeMap, FileName};

    use library;
    use syntax::parse;

    use super::*;

    fn parse(src: &str) -> core::RawTerm {
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

        use syntax::core::{Level, RawTerm};

        #[test]
        fn var() {
            assert_term_eq!(
                parse(r"x"),
                RawTerm::Var(Ignore::default(), Var::Free(Name::user("x"))),
            );
        }

        #[test]
        fn var_kebab_case() {
            assert_term_eq!(
                parse(r"or-elim"),
                RawTerm::Var(Ignore::default(), Var::Free(Name::user("or-elim"))),
            );
        }

        #[test]
        fn ty() {
            assert_term_eq!(
                parse(r"Type"),
                RawTerm::Universe(Ignore::default(), Level(0)),
            );
        }

        #[test]
        fn ty_level() {
            assert_term_eq!(
                parse(r"Type 2"),
                RawTerm::Universe(Ignore::default(), Level(0).succ().succ()),
            );
        }

        #[test]
        fn ann() {
            assert_term_eq!(
                parse(r"Type : Type"),
                RawTerm::Ann(
                    Ignore::default(),
                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                ),
            );
        }

        #[test]
        fn ann_ann_left() {
            assert_term_eq!(
                parse(r"Type : Type : Type"),
                RawTerm::Ann(
                    Ignore::default(),
                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    Rc::new(RawTerm::Ann(
                        Ignore::default(),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    )),
                ),
            );
        }

        #[test]
        fn ann_ann_right() {
            assert_term_eq!(
                parse(r"Type : (Type : Type)"),
                RawTerm::Ann(
                    Ignore::default(),
                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    Rc::new(RawTerm::Ann(
                        Ignore::default(),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    )),
                ),
            );
        }

        #[test]
        fn ann_ann_ann() {
            assert_term_eq!(
                parse(r"(Type : Type) : (Type : Type)"),
                Rc::new(RawTerm::Ann(
                    Ignore::default(),
                    Rc::new(RawTerm::Ann(
                        Ignore::default(),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    )),
                    Rc::new(RawTerm::Ann(
                        Ignore::default(),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    )),
                )),
            );
        }

        #[test]
        fn lam_ann() {
            assert_term_eq!(
                parse(r"\x : Type -> Type => x"),
                Rc::new(RawTerm::Lam(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Pi(
                                Ignore::default(),
                                nameless::bind(
                                    (
                                        Name::user("_"),
                                        Embed(Rc::new(RawTerm::Universe(
                                            Ignore::default(),
                                            Level(0)
                                        ))),
                                    ),
                                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                                ),
                            ))),
                        ),
                        Rc::new(RawTerm::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                    ),
                )),
            );
        }

        #[test]
        fn lam() {
            assert_term_eq!(
                parse(r"\x : (\y => y) => x"),
                Rc::new(RawTerm::Lam(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Lam(
                                Ignore::default(),
                                nameless::bind(
                                    (
                                        Name::user("y"),
                                        Embed(Rc::new(RawTerm::Hole(Ignore::default()))),
                                    ),
                                    Rc::new(RawTerm::Var(
                                        Ignore::default(),
                                        Var::Free(Name::user("y")),
                                    )),
                                ),
                            )),),
                        ),
                        Rc::new(RawTerm::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                    )
                )),
            );
        }

        #[test]
        fn lam_lam_ann() {
            assert_term_eq!(
                parse(r"\(x y : Type) => x"),
                Rc::new(RawTerm::Lam(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Lam(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("y"),
                                    Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(RawTerm::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("x")),
                                )),
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
                Rc::new(RawTerm::Pi(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("_"),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                    ),
                )),
            );
        }

        #[test]
        fn pi() {
            assert_term_eq!(
                parse(r"(x : Type -> Type) -> x"),
                Rc::new(RawTerm::Pi(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Pi(
                                Ignore::default(),
                                nameless::bind(
                                    (
                                        Name::user("_"),
                                        Embed(Rc::new(RawTerm::Universe(
                                            Ignore::default(),
                                            Level(0),
                                        ))),
                                    ),
                                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                                ),
                            ))),
                        ),
                        Rc::new(RawTerm::Var(Ignore::default(), Var::Free(Name::user("x")),)),
                    ),
                )),
            );
        }

        #[test]
        fn pi_pi() {
            assert_term_eq!(
                parse(r"(x y : Type) -> x"),
                Rc::new(RawTerm::Pi(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("y"),
                                    Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(RawTerm::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("x")),
                                )),
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
                Rc::new(RawTerm::Pi(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("_"),
                                    Embed(Rc::new(RawTerm::Var(
                                        Ignore::default(),
                                        Var::Free(Name::user("x")),
                                    ))),
                                ),
                                Rc::new(RawTerm::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("x")),
                                )),
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
                Rc::new(RawTerm::Lam(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("x"),
                            Embed(Rc::new(RawTerm::Pi(
                                Ignore::default(),
                                nameless::bind(
                                    (
                                        Name::user("_"),
                                        Embed(Rc::new(RawTerm::Universe(
                                            Ignore::default(),
                                            Level(0),
                                        ))),
                                    ),
                                    Rc::new(RawTerm::Universe(Ignore::default(), Level(0))),
                                ),
                            ))),
                        ),
                        Rc::new(RawTerm::Lam(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("y"),
                                    Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0),))),
                                ),
                                Rc::new(RawTerm::App(
                                    Rc::new(RawTerm::Var(
                                        Ignore::default(),
                                        Var::Free(Name::user("x")),
                                    )),
                                    Rc::new(RawTerm::Var(
                                        Ignore::default(),
                                        Var::Free(Name::user("y")),
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
            let a = Name::user("a");

            assert_term_eq!(
                parse(r"\(a : Type) (x : a) => x"),
                Rc::new(RawTerm::Lam(
                    Ignore::default(),
                    nameless::bind(
                        (
                            a.clone(),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Lam(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("x"),
                                    Embed(Rc::new(RawTerm::Var(Ignore::default(), Var::Free(a),))),
                                ),
                                Rc::new(RawTerm::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("x")),
                                )),
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
                Rc::new(RawTerm::Pi(
                    Ignore::default(),
                    nameless::bind(
                        (
                            Name::user("a"),
                            Embed(Rc::new(RawTerm::Universe(Ignore::default(), Level(0)))),
                        ),
                        Rc::new(RawTerm::Pi(
                            Ignore::default(),
                            nameless::bind(
                                (
                                    Name::user("_"),
                                    Embed(Rc::new(RawTerm::Var(
                                        Ignore::default(),
                                        Var::Free(Name::user("a")),
                                    ))),
                                ),
                                Rc::new(RawTerm::Var(
                                    Ignore::default(),
                                    Var::Free(Name::user("a")),
                                )),
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
}
