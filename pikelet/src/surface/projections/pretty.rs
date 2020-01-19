//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::surface::{Literal, Term};

pub mod prec {
    pub const TERM: u8 = 0;
    pub const EXPR: u8 = TERM + 1;
    pub const ARROW: u8 = EXPR + 1;
    pub const APP: u8 = ARROW + 1;
    pub const ATOMIC: u8 = APP + 1;
}

pub fn pretty_term<'term, D, S>(alloc: &'term D, term: &'term Term<S>) -> DocBuilder<'term, D>
where
    S: 'term + AsRef<str>,
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    pretty_term_prec(alloc, term, prec::TERM)
}

pub fn pretty_term_prec<'term, D, S>(
    alloc: &'term D,
    term: &'term Term<S>,
    prec: u8,
) -> DocBuilder<'term, D>
where
    S: 'term + AsRef<str>,
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    match term {
        Term::Name(_, name) => alloc.text(name.as_ref()),
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
        Term::Literal(_, literal) => pretty_literal(alloc, literal),
        Term::Sequence(_, term_entries) => (alloc.nil())
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
        Term::RecordType(_, ty_entries) => (alloc.nil())
            .append("Record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(alloc.concat(ty_entries.iter().map(|(name, r#type)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(alloc.text(name.as_ref()))
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
        Term::RecordTerm(_, term_entries) => (alloc.nil())
            .append("record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(alloc.concat(term_entries.iter().map(|(name, term)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(alloc.text(name.as_ref()))
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
        Term::RecordElim(_, head, name) => (alloc.nil())
            .append(pretty_term_prec(alloc, head, prec::ATOMIC))
            .append(".")
            .append(name.as_ref()),
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
        Term::FunctionTerm(_, param_names, body) => pretty_paren(
            alloc,
            prec > prec::EXPR,
            (alloc.nil())
                .append("fun")
                .append(alloc.space())
                .append(alloc.intersperse(param_names.iter().map(S::as_ref), alloc.space()))
                .append(alloc.space())
                .append("=>")
                .group()
                .append(
                    (alloc.nil())
                        .append(alloc.space())
                        .append(pretty_term_prec(alloc, body, prec::EXPR).group().nest(4)),
                ),
        ),
        Term::FunctionElim(head, arguments) => pretty_paren(
            alloc,
            prec > prec::APP,
            pretty_term_prec(alloc, head, prec::APP).append(
                (alloc.nil())
                    .append(alloc.concat(arguments.iter().map(|argument| {
                        alloc
                            .space()
                            .append(pretty_term_prec(alloc, argument, prec::ARROW))
                    })))
                    .group()
                    .nest(4),
            ),
        ),
        Term::Lift(_, term, shift) => (alloc.nil())
            .append(pretty_term_prec(alloc, term, prec::ATOMIC))
            .append("^")
            .append(shift.to_string()),
        Term::Error(_) => alloc.text("!"),
    }
}

pub fn pretty_literal<'term, D, S>(
    alloc: &'term D,
    literal: &'term Literal<S>,
) -> DocBuilder<'term, D>
where
    S: 'term + AsRef<str>,
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    match literal {
        Literal::Char(text) | Literal::String(text) | Literal::Number(text) => {
            alloc.text(text.as_ref())
        }
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
