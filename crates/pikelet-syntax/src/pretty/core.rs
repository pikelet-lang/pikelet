//! Pretty printing for the core syntax

use super::pretty::Doc;
use moniker::{Binder, Embed, Var};
use std::iter;

use core::{Literal, Pattern, Term};
use raw;
use {FloatFormat, IntFormat, Label, Level, LevelShift};

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

fn pretty_import(name: &str) -> StaticDoc {
    sexpr("import", Doc::text(format!("{:?}", name)))
}

fn pretty_fun_intro(binder: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "λ",
        Doc::group(parens(
            pretty_binder(binder)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        ))
        .append(Doc::space())
        .append(body.to_doc()),
    )
}

fn pretty_let(binders: StaticDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr("let", binders.append(Doc::space()).append(body.to_doc()))
}

fn pretty_fun_ty(binder: &Binder<String>, ann: &impl ToDoc, body: &impl ToDoc) -> StaticDoc {
    sexpr(
        "Π",
        Doc::group(parens(
            pretty_binder(binder)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        ))
        .append(Doc::space())
        .append(body.to_doc()),
    )
}

fn pretty_fun_app<'a, As, A>(expr: StaticDoc, args: As) -> StaticDoc
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

fn pretty_record_intro(inner: StaticDoc) -> StaticDoc {
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

fn pretty_record_proj(expr: &impl ToDoc, label: &Label, shift: LevelShift) -> StaticDoc {
    sexpr(
        "proj",
        expr.to_doc()
            .append(Doc::space())
            .append(Doc::text(format!("{:#}^{}", label, shift))),
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
            raw::Term::Import(_, _, ref name) => pretty_import(name),
            raw::Term::FunIntro(_, ref scope) => pretty_fun_intro(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            raw::Term::FunType(_, ref scope) => pretty_fun_ty(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            raw::Term::FunApp(ref head, ref arg) => {
                pretty_fun_app(head.to_doc(), iter::once(&arg.inner))
            },
            raw::Term::Let(_, ref scope) => pretty_let(
                Doc::concat(scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref binder, Embed((ref ann, ref term)))| {
                        parens(
                            pretty_binder(binder)
                                .append(Doc::space())
                                .append(ann.to_doc().group())
                                .append(Doc::space())
                                .append(term.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                )),
                &scope.unsafe_body.inner,
            ),
            raw::Term::RecordType(_, ref scope) => pretty_record_ty(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref ann))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(ann.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                ),
            )),
            raw::Term::RecordIntro(_, ref scope) => pretty_record_intro(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref term))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(term.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                ),
            )),
            raw::Term::RecordProj(_, ref expr, _, ref label, shift) => {
                pretty_record_proj(&expr.inner, label, shift)
            },
            raw::Term::Case(_, ref head, ref clauses) => pretty_case(
                &head.inner,
                clauses
                    .iter()
                    .map(|clause| (&clause.unsafe_pattern.inner, &clause.unsafe_body.inner)),
            ),
            raw::Term::ArrayIntro(_, ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
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
            Term::Import(ref name) => pretty_import(name),
            Term::FunIntro(ref scope) => pretty_fun_intro(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Term::FunType(ref scope) => pretty_fun_ty(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0.inner,
                &scope.unsafe_body.inner,
            ),
            Term::Let(ref scope) => pretty_let(
                Doc::concat(scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref binder, Embed((ref ann, ref term)))| {
                        parens(
                            pretty_binder(binder)
                                .append(Doc::space())
                                .append(ann.to_doc().group())
                                .append(Doc::space())
                                .append(term.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                )),
                &scope.unsafe_body.inner,
            ),
            Term::FunApp(ref head, ref arg) => {
                pretty_fun_app(head.to_doc(), iter::once(&arg.inner))
            },
            Term::RecordType(ref scope) => pretty_record_ty(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref ann))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(ann.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                ),
            )),
            Term::RecordIntro(ref scope) => pretty_record_intro(Doc::concat(
                scope.unsafe_pattern.unsafe_patterns.iter().map(
                    |&(ref label, _, Embed(ref term))| {
                        parens(
                            Doc::as_string(label)
                                .append(Doc::space())
                                .append(term.to_doc().group()),
                        )
                        .append(Doc::newline())
                    },
                ),
            )),
            Term::RecordProj(ref expr, ref label, shift) => {
                pretty_record_proj(&expr.inner, label, shift)
            },
            Term::Case(ref head, ref clauses) => pretty_case(
                &head.inner,
                clauses
                    .iter()
                    .map(|clause| (&clause.unsafe_pattern.inner, &clause.unsafe_body.inner)),
            ),
            Term::ArrayIntro(ref elems) => Doc::text("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
        }
    }
}
