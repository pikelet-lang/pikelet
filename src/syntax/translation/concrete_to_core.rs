use codespan::ByteSpan;
use nameless::{Debruijn, FreeName, LocallyNameless, Named, Scope, Var};

use syntax::concrete;
use syntax::core;

/// Translate something to the corresponding core representation
pub trait ToCore<T> {
    fn to_core(&self) -> T;
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
fn pi_to_core(
    param_names: &[(ByteSpan, String)],
    ann: &concrete::Term,
    body: &concrete::Term,
) -> core::RcRawTerm {
    let ann = ann.to_core();
    let mut term = body.to_core();

    for &(span, ref name) in param_names.iter().rev() {
        // This could be wrong... :/
        term = core::RawTerm::Pi(
            core::SourceMeta {
                span: span.to(term.span()),
            },
            Scope::bind(
                Named::new(core::Name::user(name.clone()), ann.clone()),
                term,
            ),
        ).into();
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
fn lam_to_core(
    params: &[(Vec<(ByteSpan, String)>, Option<Box<concrete::Term>>)],
    body: &concrete::Term,
) -> core::RcRawTerm {
    let mut term = body.to_core();

    for &(ref names, ref ann) in params.iter().rev() {
        for &(span, ref name) in names.iter().rev() {
            let name = core::Name::user(name.clone());
            let meta = core::SourceMeta {
                span: span.to(term.span()),
            };
            let ann = match *ann {
                None => core::RawTerm::Hole(core::SourceMeta::default()).into(),
                Some(ref ann) => ann.to_core(),
            };
            term = core::RawTerm::Lam(meta, Scope::bind(Named::new(name, ann), term)).into();
        }
    }

    term
}

impl ToCore<core::RawModule> for concrete::Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    fn to_core(&self) -> core::RawModule {
        use std::collections::BTreeMap;
        use std::collections::btree_map::Entry;

        match *self {
            concrete::Module::Valid {
                ref name,
                ref declarations,
            } => {
                // The type claims that we have encountered so far! We'll use these when
                // we encounter their corresponding definitions later as type annotations
                let mut claims = BTreeMap::new();
                // The definitions, desugared from the concrete syntax
                let mut definitions = Vec::<core::RawDefinition>::new();

                for declaration in declarations {
                    match *declaration {
                        concrete::Declaration::Import { .. } => {
                            unimplemented!("import declarations")
                        },
                        // We've enountered a claim! Let's try to add it to the claims
                        // that we've seen so far...
                        concrete::Declaration::Claim {
                            name: (_, ref name),
                            ref ann,
                            ..
                        } => {
                            match claims.entry(name) {
                                // Oh no! We've already seen a claim for this name!
                                Entry::Occupied(_) => panic!(), // FIXME: Better error
                                // This name does not yet have a claim associated with it
                                Entry::Vacant(mut entry) => {
                                    let mut ann = ann.to_core();

                                    for (level, definition) in definitions.iter().rev().enumerate()
                                    {
                                        let name = core::Name::user(definition.name.clone());
                                        ann.close_at(Debruijn(level as u32), &name);
                                    }

                                    entry.insert(ann)
                                },
                            };
                        },
                        // We've encountered a definition. Let's desugar it!
                        concrete::Declaration::Definition {
                            name: (_, ref name),
                            ref params,
                            ref body,
                            ..
                        } => {
                            let name = name.clone();
                            let mut term = lam_to_core(params, body);
                            let ann = claims.remove(&name);

                            for (level, definition) in definitions.iter().rev().enumerate() {
                                let name = core::Name::user(definition.name.clone());
                                term.close_at(Debruijn(level as u32), &name);
                            }

                            definitions.push(core::RawDefinition { name, term, ann });
                        },
                        concrete::Declaration::Error(_) => unimplemented!("error recovery"),
                    }
                }

                // FIXME: Better error
                assert!(claims.is_empty());

                core::RawModule {
                    name: name.1.clone(),
                    definitions,
                }
            },
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl ToCore<core::RcRawTerm> for concrete::Term {
    /// Convert a term in the concrete syntax into a core term
    fn to_core(&self) -> core::RcRawTerm {
        let meta = core::SourceMeta { span: self.span() };
        match *self {
            concrete::Term::Parens(_, ref term) => term.to_core(),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = expr.to_core().into();
                let ty = ty.to_core().into();

                core::RawTerm::Ann(meta, expr, ty).into()
            },
            concrete::Term::Universe(_, level) => {
                core::RawTerm::Universe(meta, core::Level(level.unwrap_or(0))).into()
            },
            concrete::Term::Hole(_) => core::RawTerm::Hole(meta).into(),
            concrete::Term::Var(_, ref x) => {
                let var = Var::Free(core::Name::user(x.clone()));

                core::RawTerm::Var(meta, var).into()
            },
            concrete::Term::Pi(_, (ref names, ref ann), ref body) => pi_to_core(names, ann, body),
            concrete::Term::Lam(_, ref params, ref body) => lam_to_core(params, body),
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = core::Name::fresh();
                let ann = ann.to_core();
                let body = body.to_core();

                core::RawTerm::Pi(meta, Scope::bind(Named::new(name, ann), body)).into()
            },
            concrete::Term::App(ref fn_expr, ref arg) => {
                let fn_expr = fn_expr.to_core();
                let arg = arg.to_core();

                core::RawTerm::App(meta, fn_expr, arg).into()
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}

#[cfg(test)]
mod to_core {
    use codespan::{CodeMap, FileName};

    use library;
    use syntax::parse;

    use super::*;

    fn parse(src: &str) -> core::RcRawTerm {
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

        let (concrete_term, errors) = parse::term(&filemap);
        assert!(errors.is_empty());

        concrete_term.to_core()
    }

    mod module {
        use super::*;

        #[test]
        fn parse_prelude() {
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), library::PRELUDE.into());

            let (concrete_module, errors) = parse::module(&filemap);
            assert!(errors.is_empty());

            concrete_module.to_core();
        }
    }

    mod term {
        use super::*;

        use syntax::core::{Level, Name, RawTerm, SourceMeta};

        #[test]
        fn var() {
            assert_eq!(
                parse(r"x"),
                RawTerm::Var(SourceMeta::default(), Var::Free(Name::user("x"))).into()
            );
        }

        #[test]
        fn var_kebab_case() {
            assert_eq!(
                parse(r"or-elim"),
                RawTerm::Var(SourceMeta::default(), Var::Free(Name::user("or-elim"))).into(),
            );
        }

        #[test]
        fn ty() {
            assert_eq!(
                parse(r"Type"),
                RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
            );
        }

        #[test]
        fn ty_level() {
            assert_eq!(
                parse(r"Type 2"),
                RawTerm::Universe(SourceMeta::default(), Level::ZERO.succ().succ()).into()
            );
        }

        #[test]
        fn ann() {
            assert_eq!(
                parse(r"Type : Type"),
                RawTerm::Ann(
                    SourceMeta::default(),
                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                ).into(),
            );
        }

        #[test]
        fn ann_ann_left() {
            assert_eq!(
                parse(r"Type : Type : Type"),
                RawTerm::Ann(
                    SourceMeta::default(),
                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                    RawTerm::Ann(
                        SourceMeta::default(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                    ).into(),
                ).into(),
            );
        }

        #[test]
        fn ann_ann_right() {
            assert_eq!(
                parse(r"Type : (Type : Type)"),
                RawTerm::Ann(
                    SourceMeta::default(),
                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                    RawTerm::Ann(
                        SourceMeta::default(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                    ).into(),
                ).into(),
            );
        }

        #[test]
        fn ann_ann_ann() {
            assert_eq!(
                parse(r"(Type : Type) : (Type : Type)"),
                RawTerm::Ann(
                    SourceMeta::default(),
                    RawTerm::Ann(
                        SourceMeta::default(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                    ).into(),
                    RawTerm::Ann(
                        SourceMeta::default(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                    ).into(),
                ).into(),
            );
        }

        #[test]
        fn lam_ann() {
            let x = Name::user("x");

            assert_eq!(
                parse(r"\x : Type -> Type => x"),
                RawTerm::Lam(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Pi(
                                SourceMeta::default(),
                                Scope::bind(
                                    Named::new(
                                        Name::user("_"),
                                        RawTerm::Universe(SourceMeta::default(), Level::ZERO)
                                            .into()
                                    ),
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                                )
                            ).into()
                        ),
                        RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn lam() {
            let x = Name::user("x");
            let y = Name::user("y");

            assert_eq!(
                parse(r"\x : (\y => y) => x"),
                RawTerm::Lam(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Lam(
                                SourceMeta::default(),
                                Scope::bind(
                                    Named::new(
                                        y.clone(),
                                        RawTerm::Hole(SourceMeta::default()).into()
                                    ),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(y)).into(),
                                )
                            ).into(),
                        ),
                        RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn lam_lam_ann() {
            let x = Name::user("x");
            let y = Name::user("y");

            assert_eq!(
                parse(r"\(x y : Type) => x"),
                RawTerm::Lam(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Lam(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    y,
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                                ),
                                RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn arrow() {
            assert_eq!(
                parse(r"Type -> Type"),
                RawTerm::Pi(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            Name::user("_"),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn pi() {
            let x = Name::user("x");

            assert_eq!(
                parse(r"(x : Type -> Type) -> x"),
                RawTerm::Pi(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Pi(
                                SourceMeta::default(),
                                Scope::bind(
                                    Named::new(
                                        Name::user("_"),
                                        RawTerm::Universe(SourceMeta::default(), Level::ZERO)
                                            .into()
                                    ),
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                                )
                            ).into(),
                        ),
                        RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn pi_pi() {
            let x = Name::user("x");
            let y = Name::user("y");

            assert_eq!(
                parse(r"(x y : Type) -> x"),
                RawTerm::Pi(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Pi(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    y,
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                                ),
                                RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn pi_arrow() {
            let x = Name::user("x");

            assert_eq!(
                parse(r"(x : Type) -> x -> x"),
                RawTerm::Pi(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Pi(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    Name::user("_"),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(x.clone()))
                                        .into()
                                ),
                                RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn lam_app() {
            let x = Name::user("x");
            let y = Name::user("y");

            assert_eq!(
                parse(r"\(x : Type -> Type) (y : Type) => x y"),
                RawTerm::Lam(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            x.clone(),
                            RawTerm::Pi(
                                SourceMeta::default(),
                                Scope::bind(
                                    Named::new(
                                        Name::user("_"),
                                        RawTerm::Universe(SourceMeta::default(), Level::ZERO)
                                            .into()
                                    ),
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into(),
                                )
                            ).into(),
                        ),
                        RawTerm::Lam(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    y.clone(),
                                    RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                                ),
                                RawTerm::App(
                                    SourceMeta::default(),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(y)).into(),
                                ).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn id() {
            let x = Name::user("x");
            let a = Name::user("a");

            assert_eq!(
                parse(r"\(a : Type) (x : a) => x"),
                RawTerm::Lam(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            a.clone(),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Lam(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    x.clone(),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(a)).into()
                                ),
                                RawTerm::Var(SourceMeta::default(), Var::Free(x)).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        #[test]
        fn id_ty() {
            let a = Name::user("a");

            assert_eq!(
                parse(r"(a : Type) -> a -> a"),
                RawTerm::Pi(
                    SourceMeta::default(),
                    Scope::bind(
                        Named::new(
                            a.clone(),
                            RawTerm::Universe(SourceMeta::default(), Level::ZERO).into()
                        ),
                        RawTerm::Pi(
                            SourceMeta::default(),
                            Scope::bind(
                                Named::new(
                                    Name::user("_"),
                                    RawTerm::Var(SourceMeta::default(), Var::Free(a.clone()))
                                        .into()
                                ),
                                RawTerm::Var(SourceMeta::default(), Var::Free(a)).into(),
                            )
                        ).into(),
                    )
                ).into(),
            );
        }

        mod sugar {
            use super::*;

            #[test]
            fn lam_args() {
                assert_eq!(
                    parse(r"\x (y : Type) z => x"),
                    parse(r"\x => \y : Type => \z => x"),
                );
            }

            #[test]
            fn lam_args_multi() {
                assert_eq!(
                    parse(r"\(x : Type) (y : Type) z => x"),
                    parse(r"\(x y : Type) z => x"),
                );
            }

            #[test]
            fn pi_args() {
                assert_eq!(
                    parse(r"(a : Type) -> (x y z : a) -> x"),
                    parse(r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> x"),
                );
            }

            #[test]
            fn arrow() {
                assert_eq!(
                    parse(r"(a : Type) -> a -> a"),
                    parse(r"(a : Type) -> (x : a) -> a"),
                )
            }
        }
    }
}
