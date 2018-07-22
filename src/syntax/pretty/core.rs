//! Pretty printing for the core syntax

use moniker::{Binder, Var};
use pretty::Doc;
use std::iter;

use syntax::core::{Head, Literal, Neutral, Term, Value};
use syntax::raw;
use syntax::{Label, Level};

use super::{parens, sexpr, StaticDoc, ToDoc};

fn pretty_ann(expr: &impl ToDoc, ty: &impl ToDoc) -> StaticDoc {
    sexpr(
        "ann",
        expr.to_doc().append(Doc::space()).append(ty.to_doc()),
    )
}

fn pretty_universe(level: Level) -> StaticDoc {
    sexpr("Type", Doc::as_string(&level))
}

fn pretty_var(var: &Var<String>) -> StaticDoc {
    sexpr("var", Doc::text(format!("{:#}", var)))
}

fn pretty_lam(name: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "λ",
        Doc::group(parens(
            Doc::as_string(name)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        )).append(Doc::space())
            .append(body.to_doc()),
    )
}

fn pretty_pi(name: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "Π",
        Doc::group(parens(
            Doc::as_string(name)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        )).append(Doc::space())
            .append(body.to_doc()),
    )
}

fn pretty_app<'a, As, A>(expr: StaticDoc, args: As) -> StaticDoc
where
    As: 'a + IntoIterator<Item = &'a A>,
    A: 'a + ToDoc,
{
    sexpr(
        "app",
        expr.append(Doc::space()).append(Doc::intersperse(
            args.into_iter().map(A::to_doc),
            Doc::space(),
        )),
    )
}

fn pretty_if(cond: &impl ToDoc, if_true: &impl ToDoc, if_false: &impl ToDoc) -> StaticDoc {
    sexpr(
        "if",
        cond.to_doc()
            .append(Doc::space())
            .append(if_true.to_doc())
            .append(Doc::space())
            .append(if_false.to_doc()),
    )
}

fn pretty_record_ty(inner: StaticDoc) -> StaticDoc {
    sexpr("Record", inner)
}

fn pretty_record(inner: StaticDoc) -> StaticDoc {
    sexpr("record", inner)
}

fn pretty_empty_record_ty() -> StaticDoc {
    pretty_record_ty(Doc::text("()"))
}

fn pretty_empty_record() -> StaticDoc {
    pretty_record(Doc::text("()"))
}

fn pretty_proj(expr: &impl ToDoc, label: &Label<String>) -> StaticDoc {
    sexpr(
        "proj",
        expr.to_doc()
            .append(Doc::space())
            .append(Doc::as_string(&label.0)),
    )
}

impl ToDoc for raw::Literal {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Literal::String(ref value) => Doc::text(format!("{:?}", value)),
            raw::Literal::Char(value) => Doc::text(format!("{:?}", value)),
            raw::Literal::Int(value) => Doc::as_string(&value),
            raw::Literal::Float(value) => Doc::as_string(&value),
        }
    }
}

impl ToDoc for Literal {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Literal::Bool(true) => Doc::text("true"),
            Literal::Bool(false) => Doc::text("false"),
            Literal::String(ref value) => Doc::text(format!("{:?}", value)),
            Literal::Char(value) => Doc::text(format!("{:?}", value)),
            Literal::U8(value) => Doc::as_string(&value),
            Literal::U16(value) => Doc::as_string(&value),
            Literal::U32(value) => Doc::as_string(&value),
            Literal::U64(value) => Doc::as_string(&value),
            Literal::I8(value) => Doc::as_string(&value),
            Literal::I16(value) => Doc::as_string(&value),
            Literal::I32(value) => Doc::as_string(&value),
            Literal::I64(value) => Doc::as_string(&value),
            Literal::F32(value) => Doc::as_string(&value),
            Literal::F64(value) => Doc::as_string(&value),
        }
    }
}

impl ToDoc for raw::Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Term::Ann(_, ref expr, ref ty) => pretty_ann(&expr.inner, &ty.inner),
            raw::Term::Universe(_, level) => pretty_universe(level),
            raw::Term::Hole(_) => parens(Doc::text("hole")),
            raw::Term::Literal(_, ref lit) => lit.to_doc(),
            raw::Term::Var(_, ref var) => pretty_var(var),
            raw::Term::Lam(_, ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            raw::Term::Pi(_, ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            raw::Term::App(ref expr, ref arg) => pretty_app(expr.to_doc(), iter::once(&arg.inner)),
            raw::Term::If(_, ref cond, ref if_true, ref if_false) => {
                pretty_if(&cond.inner, &if_true.inner, &if_false.inner)
            },
            raw::Term::RecordType(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        raw::Term::RecordType(_, ref next_scope) => scope = next_scope,
                        raw::Term::RecordTypeEmpty(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            raw::Term::RecordTypeEmpty(_) => pretty_empty_record_ty(),
            raw::Term::Record(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        raw::Term::Record(_, ref next_scope) => scope = next_scope,
                        raw::Term::RecordEmpty(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            raw::Term::RecordEmpty(_) => pretty_empty_record(),
            raw::Term::Array(_, ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            raw::Term::Proj(_, ref expr, _, ref label) => pretty_proj(&expr.inner, label),
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Term::Ann(ref expr, ref ty) => pretty_ann(&expr.inner, &ty.inner),
            Term::Universe(level) => pretty_universe(level),
            Term::Literal(ref lit) => lit.to_doc(),
            Term::Var(ref var) => pretty_var(var),
            Term::Lam(ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Term::Pi(ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Term::App(ref expr, ref arg) => pretty_app(expr.to_doc(), iter::once(&arg.inner)),
            Term::If(ref cond, ref if_true, ref if_false) => {
                pretty_if(&cond.inner, &if_true.inner, &if_false.inner)
            },
            Term::RecordType(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Term::RecordType(ref next_scope) => scope = next_scope,
                        Term::RecordTypeEmpty => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Term::RecordTypeEmpty => pretty_empty_record_ty(),
            Term::Record(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Term::Record(ref next_scope) => scope = next_scope,
                        Term::RecordEmpty => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            Term::RecordEmpty => pretty_empty_record(),
            Term::Array(ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            Term::Proj(ref expr, ref label) => pretty_proj(&expr.inner, label),
        }
    }
}

impl ToDoc for Value {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Value::Universe(level) => pretty_universe(level),
            Value::Literal(ref lit) => lit.to_doc(),
            Value::Lam(ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Value::Pi(ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Value::RecordType(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Value::RecordType(ref next_scope) => scope = next_scope,
                        Value::RecordTypeEmpty => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Value::RecordTypeEmpty => pretty_empty_record_ty(),
            Value::Record(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Value::Record(ref next_scope) => scope = next_scope,
                        Value::RecordEmpty => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            Value::RecordEmpty => pretty_empty_record(),
            Value::Array(ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            Value::Neutral(ref n) => n.to_doc(),
        }
    }
}

impl ToDoc for Neutral {
    fn to_doc(&self) -> StaticDoc {
        let (head, spine) = match *self {
            Neutral::App(ref head, ref spine) => (head.to_doc(), spine),
            Neutral::If(ref cond, ref if_true, ref if_false, ref spine) => (
                pretty_if(&cond.inner, &if_true.inner, &if_false.inner),
                spine,
            ),
            Neutral::Proj(ref expr, ref label, ref spine) => {
                (pretty_proj(&expr.inner, label), spine)
            },
        };

        pretty_app(head, spine.iter().map(|arg| &arg.inner))
    }
}

impl ToDoc for Head {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Head::Var(ref var) => pretty_var(var),
        }
    }
}
