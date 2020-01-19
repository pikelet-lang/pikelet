//! Pretty prints the core language to a textual form.
use pretty::{DocAllocator, DocBuilder};

use crate::core::{Constant, Term};

pub mod prec {
    pub const TERM: u8 = 0;
    pub const EXPR: u8 = TERM + 1;
    pub const ARROW: u8 = EXPR + 1;
    pub const APP: u8 = ARROW + 1;
    pub const ATOMIC: u8 = APP + 1;
}

pub fn pretty_term<'term, D>(alloc: &'term D, term: &'term Term) -> DocBuilder<'term, D>
where
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    pretty_term_prec(alloc, term, prec::TERM)
}

pub fn pretty_term_prec<'term, D>(
    alloc: &'term D,
    term: &'term Term,
    prec: u8,
) -> DocBuilder<'term, D>
where
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    match term {
        Term::Universe(level) => (alloc.nil())
            .append("Type")
            .append("^")
            .append(alloc.as_string(level.0)),
        Term::Global(name) => (alloc.nil())
            .append(alloc.text("global"))
            .append(alloc.space())
            .append(alloc.text(name)),
        Term::Local(index) => (alloc.nil())
            .append(alloc.text("local"))
            .append(alloc.space())
            .append(alloc.as_string(index.0)),
        Term::Ann(term, r#type) => pretty_paren(
            alloc,
            prec > prec::TERM,
            (alloc.nil())
                .append(pretty_term_prec(alloc, term, prec::EXPR))
                .append(alloc.space())
                .append(":")
                .append(
                    (alloc.space())
                        .append(pretty_term_prec(alloc, r#type, prec::TERM))
                        .group()
                        .nest(4),
                ),
        ),
        Term::Constant(constant) => pretty_constant(alloc, constant),
        Term::Sequence(term_entries) => (alloc.nil())
            .append("[")
            .group()
            .append(
                alloc.intersperse(
                    term_entries
                        .iter()
                        .map(|term| pretty_term_prec(alloc, term, prec::TERM).group().nest(4)),
                    alloc.text(",").append(alloc.space()),
                ),
            )
            .append("]"),
        Term::RecordType(ty_entries) => (alloc.nil())
            .append("Record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(alloc.concat(ty_entries.iter().map(|(name, r#type)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(alloc.text(name))
                    .append(":")
                    .append(
                        (alloc.space())
                            .append(pretty_term_prec(alloc, r#type, prec::TERM))
                            .append(",")
                            .group()
                            .nest(4),
                    )
                    .nest(4)
                    .group()
            })))
            .append("}"),
        Term::RecordTerm(term_entries) => (alloc.nil())
            .append("record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(alloc.concat(term_entries.iter().map(|(name, term)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(alloc.text(name))
                    .append("=")
                    .append(
                        (alloc.space())
                            .append(pretty_term_prec(alloc, term, prec::TERM))
                            .append(",")
                            .group()
                            .nest(4),
                    )
                    .nest(4)
                    .group()
            })))
            .append("}"),
        Term::RecordElim(head, name) => (alloc.nil())
            .append(pretty_term_prec(alloc, head, prec::ATOMIC))
            .append(".")
            .append(alloc.text(name)),
        Term::FunctionType(param_type, body_type) => pretty_paren(
            alloc,
            prec > prec::ARROW,
            (alloc.nil())
                .append(pretty_term_prec(alloc, param_type, prec::APP))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(pretty_term_prec(alloc, body_type, prec::ARROW)),
        ),
        Term::FunctionTerm(_, body) => pretty_paren(
            alloc,
            prec > prec::EXPR,
            (alloc.nil())
                .append("fun")
                .append(alloc.space())
                .append("_")
                .append(alloc.space())
                .append("=>")
                .group()
                .append(alloc.space())
                .append(pretty_term_prec(alloc, body, prec::EXPR).nest(4)),
        ),
        Term::FunctionElim(head, argument) => pretty_paren(
            alloc,
            prec > prec::APP,
            pretty_term_prec(alloc, head, prec::APP).append(
                (alloc.space())
                    .append(pretty_term_prec(alloc, argument, prec::ARROW))
                    .group()
                    .nest(4),
            ),
        ),
        Term::Lift(term, shift) => (alloc.nil())
            .append(pretty_term_prec(alloc, term, prec::ATOMIC))
            .append("^")
            .append(alloc.as_string(shift.0)),
        Term::Error => alloc.text("!"),
    }
}

pub fn pretty_constant<'term, D>(alloc: &'term D, constant: &'term Constant) -> DocBuilder<'term, D>
where
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    match constant {
        Constant::U8(value) => alloc.text(format!("{}", value)),
        Constant::U16(value) => alloc.text(format!("{}", value)),
        Constant::U32(value) => alloc.text(format!("{}", value)),
        Constant::U64(value) => alloc.text(format!("{}", value)),
        Constant::S8(value) => alloc.text(format!("{}", value)),
        Constant::S16(value) => alloc.text(format!("{}", value)),
        Constant::S32(value) => alloc.text(format!("{}", value)),
        Constant::S64(value) => alloc.text(format!("{}", value)),
        Constant::F32(value) => alloc.text(format!("{}", value)),
        Constant::F64(value) => alloc.text(format!("{}", value)),
        Constant::Char(value) => alloc.text(format!("{:?}", value)),
        Constant::String(value) => alloc.text(format!("{:?}", value)),
    }
}

fn pretty_paren<'term, D>(
    alloc: &'term D,
    b: bool,
    doc: DocBuilder<'term, D>,
) -> DocBuilder<'term, D>
where
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    if b {
        alloc.text("(").append(doc).append(")")
    } else {
        doc
    }
}
