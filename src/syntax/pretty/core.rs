//! Pretty printing for the core syntax

use nameless::{Name, Var};
use pretty::Doc;
use std::iter;

use syntax::core::{Definition, Head, Literal, Module, Neutral, Term, Value};
use syntax::raw;
use syntax::{Label, Level};

use super::{parens, sexpr, StaticDoc, ToDoc};

fn pretty_ann<E: ToDoc, T: ToDoc>(expr: &E, ty: &T) -> StaticDoc {
    sexpr(
        "ann",
        expr.to_doc().append(Doc::space()).append(ty.to_doc()),
    )
}

fn pretty_universe(level: Level) -> StaticDoc {
    sexpr("Type", Doc::as_string(level))
}

fn pretty_var(var: &Var) -> StaticDoc {
    sexpr("var", Doc::text(format!("{:#}", var)))
}

fn pretty_lam<A: ToDoc, B: ToDoc>(name: &Name, ann: &A, body: &B) -> StaticDoc {
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

fn pretty_pi<A: ToDoc, B: ToDoc>(name: &Name, ann: &A, body: &B) -> StaticDoc {
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

fn pretty_if<C: ToDoc, T: ToDoc, F: ToDoc>(cond: &C, if_true: &T, if_false: &F) -> StaticDoc {
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

fn pretty_proj<E: ToDoc>(expr: &E, label: &Label) -> StaticDoc {
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
            raw::Literal::Int(value) => Doc::as_string(value),
            raw::Literal::Float(value) => Doc::as_string(value),
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
            Literal::U8(value) => Doc::as_string(value),
            Literal::U16(value) => Doc::as_string(value),
            Literal::U32(value) => Doc::as_string(value),
            Literal::U64(value) => Doc::as_string(value),
            Literal::I8(value) => Doc::as_string(value),
            Literal::I16(value) => Doc::as_string(value),
            Literal::I32(value) => Doc::as_string(value),
            Literal::I64(value) => Doc::as_string(value),
            Literal::F32(value) => Doc::as_string(value),
            Literal::F64(value) => Doc::as_string(value),
        }
    }
}

impl ToDoc for raw::Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Term::Ann(_, ref expr, ref ty) => pretty_ann(expr, ty),
            raw::Term::Universe(_, level) => pretty_universe(level),
            raw::Term::Hole(_) => parens(Doc::text("hole")),
            raw::Term::Literal(_, ref lit) => lit.to_doc(),
            raw::Term::Var(_, ref var) => pretty_var(var),
            raw::Term::Lam(_, ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            raw::Term::Pi(_, ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            raw::Term::App(ref expr, ref arg) => pretty_app(expr.to_doc(), iter::once(arg)),
            raw::Term::If(_, ref cond, ref if_true, ref if_false) => {
                pretty_if(cond, if_true, if_false)
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
                .append(Doc::text("]")),
            raw::Term::Proj(_, ref expr, _, ref label) => pretty_proj(expr, label),
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Term::Ann(_, ref expr, ref ty) => pretty_ann(expr, ty),
            Term::Universe(_, level) => pretty_universe(level),
            Term::Literal(_, ref lit) => lit.to_doc(),
            Term::Var(_, ref var) => pretty_var(var),
            Term::Lam(_, ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Term::Pi(_, ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Term::App(ref expr, ref arg) => pretty_app(expr.to_doc(), iter::once(arg)),
            Term::If(_, ref cond, ref if_true, ref if_false) => pretty_if(cond, if_true, if_false),
            Term::RecordType(_, ref scope) => {
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
                        Term::RecordType(_, ref next_scope) => scope = next_scope,
                        Term::RecordTypeEmpty(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Term::RecordTypeEmpty(_) => pretty_empty_record_ty(),
            Term::Record(_, ref scope) => {
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
                        Term::Record(_, ref next_scope) => scope = next_scope,
                        Term::RecordEmpty(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            Term::RecordEmpty(_) => pretty_empty_record(),
            Term::Array(_, ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append(Doc::text("]")),
            Term::Proj(_, ref expr, _, ref label) => pretty_proj(expr, label),
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
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Value::Pi(ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
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
                .append(Doc::text("]")),
            Value::Neutral(ref n) => n.to_doc(),
        }
    }
}

impl ToDoc for Neutral {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Neutral::App(ref head, ref spine) => pretty_app(head.to_doc(), spine),
            Neutral::If(ref cond, ref if_true, ref if_false, ref spine) => {
                pretty_app(pretty_if(cond, if_true, if_false), spine)
            },
            Neutral::Proj(ref expr, ref label, ref spine) => {
                pretty_app(pretty_proj(expr, label), spine)
            },
        }
    }
}

impl ToDoc for Head {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Head::Var(ref var) => pretty_var(var),
        }
    }
}

impl ToDoc for raw::Definition {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "define",
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(self.ann.to_doc())
                .append(Doc::space())
                .append(self.term.to_doc()),
        )
    }
}

impl ToDoc for raw::Module {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "module",
            Doc::intersperse(
                self.definitions
                    .iter()
                    .map(|definition| definition.to_doc()),
                Doc::newline().append(Doc::newline()),
            ),
        )
    }
}

impl ToDoc for Definition {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "define",
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(self.ann.to_doc())
                .append(Doc::space())
                .append(self.term.to_doc()),
        )
    }
}

impl ToDoc for Module {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "module",
            Doc::intersperse(
                self.definitions
                    .iter()
                    .map(|definition| definition.to_doc()),
                Doc::newline().append(Doc::newline()),
            ),
        )
    }
}
