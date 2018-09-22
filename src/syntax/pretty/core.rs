//! Pretty printing for the core syntax

use moniker::{Binder, Embed, Var};
use pretty::Doc;
use std::iter;

use syntax::core::{Head, Literal, Neutral, Pattern, Term, Value};
use syntax::raw;
use syntax::{FloatFormat, IntFormat, Label, Level, LevelShift};

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

fn pretty_binder(binder: &Binder<String>) -> StaticDoc {
    sexpr("binder", Doc::text(format!("{:#}", binder)))
}

fn pretty_var(var: &Var<String>, shift: LevelShift) -> StaticDoc {
    sexpr("var", Doc::text(format!("{:#}^{}", var, shift)))
}

fn pretty_extern(name: &str, ty: &impl ToDoc) -> StaticDoc {
    sexpr(
        "extern",
        Doc::text(format!("{:?}", name))
            .append(Doc::space())
            .append(ty.to_doc()),
    )
}

fn pretty_lam(binder: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "λ",
        Doc::group(parens(
            pretty_binder(binder)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        )).append(Doc::space())
        .append(body.to_doc()),
    )
}

fn pretty_let_ann(
    binder: &Binder<String>,
    ann: &impl ToDoc,
    bind: &impl ToDoc,
    body: &impl ToDoc,
) -> StaticDoc {
    sexpr(
        "let",
        Doc::group(parens(
            pretty_binder(binder)
                .append(Doc::space())
                .append(ann.to_doc().group())
                .append(Doc::space())
                .append(bind.to_doc().group()),
        )).append(Doc::space())
        .append(body.to_doc()),
    )
}

fn pretty_let(binder: &Binder<String>, bind: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "let",
        Doc::group(parens(
            pretty_binder(binder)
                .append(Doc::space())
                .append(bind.to_doc().group()),
        )).append(Doc::space())
        .append(body.to_doc()),
    )
}

fn pretty_pi(binder: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "Π",
        Doc::group(parens(
            pretty_binder(binder)
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

fn pretty_record_ty(inner: StaticDoc) -> StaticDoc {
    sexpr("Record", inner)
}

fn pretty_record(inner: StaticDoc) -> StaticDoc {
    sexpr("record", inner)
}

fn pretty_case<'a, Cs, P, T>(head: &impl ToDoc, clauses: Cs) -> StaticDoc
where
    Cs: 'a + IntoIterator<Item = (&'a P, &'a T)>,
    P: 'a + ToDoc,
    T: 'a + ToDoc,
{
    sexpr(
        "case",
        head.to_doc().append(Doc::space()).append(Doc::intersperse(
            clauses.into_iter().map(|(pattern, body)| {
                parens(pattern.to_doc().append(Doc::space()).append(body.to_doc()))
            }),
            Doc::space(),
        )),
    )
}

fn pretty_proj(expr: &impl ToDoc, label: &Label) -> StaticDoc {
    sexpr(
        "proj",
        expr.to_doc()
            .append(Doc::space())
            .append(Doc::as_string(label)),
    )
}

impl ToDoc for raw::Literal {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Literal::String(_, ref value) => Doc::text(format!("{:?}", value)),
            raw::Literal::Char(_, value) => Doc::text(format!("{:?}", value)),
            raw::Literal::Int(_, value, IntFormat::Bin) => Doc::text(format!("0b{:b}", value)),
            raw::Literal::Int(_, value, IntFormat::Oct) => Doc::text(format!("0o{:o}", value)),
            raw::Literal::Int(_, value, IntFormat::Dec) => Doc::text(format!("{}", value)),
            raw::Literal::Int(_, value, IntFormat::Hex) => Doc::text(format!("0x{:x}", value)),
            raw::Literal::Float(_, value, FloatFormat::Dec) => Doc::text(format!("{}", value)),
        }
    }
}

impl ToDoc for raw::Pattern {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Pattern::Ann(ref pattern, Embed(ref ty)) => pretty_ann(&pattern.inner, &ty.inner),
            raw::Pattern::Binder(_, ref binder) => pretty_binder(binder),
            raw::Pattern::Var(_, Embed(ref var), shift) => pretty_var(var, shift),
            raw::Pattern::Literal(ref literal) => literal.to_doc(),
        }
    }
}

impl ToDoc for raw::Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            raw::Term::Ann(ref expr, ref ty) => pretty_ann(&expr.inner, &ty.inner),
            raw::Term::Universe(_, level) => pretty_universe(level),
            raw::Term::Hole(_) => parens(Doc::text("hole")),
            raw::Term::Literal(ref literal) => literal.to_doc(),
            raw::Term::Var(_, ref var, shift) => pretty_var(var, shift),
            raw::Term::Extern(_, _, ref name, ref ty) => pretty_extern(name, &ty.inner),
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
            raw::Term::App(ref head, ref arg) => pretty_app(head.to_doc(), iter::once(&arg.inner)),
            raw::Term::RecordType(_, ref scope) => pretty_record_ty(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref ann))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(ann.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            raw::Term::Record(_, ref scope) => pretty_record(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref term))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(term.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            raw::Term::Proj(_, ref expr, _, ref label) => pretty_proj(&expr.inner, label),
            raw::Term::Case(_, ref head, ref clauses) => pretty_case(
                &head.inner,
                clauses
                    .iter()
                    .map(|clause| (&clause.unsafe_pattern.inner, &clause.unsafe_body.inner)),
            ),
            raw::Term::Array(_, ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                )).append("]"),
            raw::Term::Let(_, ref scope) => pretty_let_ann(
                &scope.unsafe_pattern.0,
                &((scope.unsafe_pattern.1).0).0.inner,
                &((scope.unsafe_pattern.1).0).1.inner,
                &scope.unsafe_body.inner,
            ),
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
            Literal::U8(value, _) => Doc::as_string(&value),
            Literal::U16(value, _) => Doc::as_string(&value),
            Literal::U32(value, _) => Doc::as_string(&value),
            Literal::U64(value, _) => Doc::as_string(&value),
            Literal::S8(value, _) => Doc::as_string(&value),
            Literal::S16(value, _) => Doc::as_string(&value),
            Literal::S32(value, _) => Doc::as_string(&value),
            Literal::S64(value, _) => Doc::as_string(&value),
            Literal::F32(value, _) => Doc::as_string(&value),
            Literal::F64(value, _) => Doc::as_string(&value),
        }
    }
}

impl ToDoc for Pattern {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => pretty_ann(&pattern.inner, &ty.inner),
            Pattern::Binder(ref binder) => pretty_binder(binder),
            Pattern::Var(Embed(ref var), shift) => pretty_var(var, shift),
            Pattern::Literal(ref literal) => literal.to_doc(),
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Term::Ann(ref expr, ref ty) => pretty_ann(&expr.inner, &ty.inner),
            Term::Universe(level) => pretty_universe(level),
            Term::Literal(ref literal) => literal.to_doc(),
            Term::Var(ref var, shift) => pretty_var(var, shift),
            Term::Extern(ref name, ref ty) => pretty_extern(name, &ty.inner),
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
            Term::Let(ref scope) => pretty_let(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Term::App(ref head, ref arg) => pretty_app(head.to_doc(), iter::once(&arg.inner)),
            Term::RecordType(ref scope) => pretty_record_ty(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref ann))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(ann.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            Term::Record(ref scope) => pretty_record(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref term))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(term.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            Term::Proj(ref expr, ref label) => pretty_proj(&expr.inner, label),
            Term::Case(ref head, ref clauses) => pretty_case(
                &head.inner,
                clauses
                    .iter()
                    .map(|clause| (&clause.unsafe_pattern.inner, &clause.unsafe_body.inner)),
            ),
            Term::Array(ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                )).append("]"),
        }
    }
}

impl ToDoc for Value {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Value::Universe(level) => pretty_universe(level),
            Value::Literal(ref literal) => literal.to_doc(),
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
            Value::RecordType(ref scope) => pretty_record_ty(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref ann))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(ann.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            Value::Record(ref scope) => pretty_record(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref term))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(term.to_doc())
                                .append(Doc::newline()),
                        )
                    },
                ),
            )),
            Value::Array(ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                )).append("]"),
            Value::Neutral(ref neutral, ref spine) if spine.is_empty() => neutral.to_doc(),
            Value::Neutral(ref neutral, ref spine) => {
                pretty_app(neutral.to_doc(), spine.iter().map(|arg| &arg.inner))
            },
        }
    }
}

impl ToDoc for Neutral {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Neutral::Head(ref head) => head.to_doc(),
            Neutral::Proj(ref expr, ref label) => pretty_proj(&expr.inner, label),
            Neutral::Case(ref head, ref clauses) => pretty_case(
                &head.inner,
                clauses
                    .iter()
                    .map(|clause| (&clause.unsafe_pattern.inner, &clause.unsafe_body.inner)),
            ),
        }
    }
}

impl ToDoc for Head {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Head::Var(ref var, shift) => pretty_var(var, shift),
            Head::Extern(ref name, ref ty) => pretty_extern(name, &ty.inner),
        }
    }
}
