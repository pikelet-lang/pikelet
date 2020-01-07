//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::surface::{Literal, Term};

pub fn pretty_term<'term, D, S>(alloc: &'term D, term: &'term Term<S>) -> DocBuilder<'term, D>
where
    S: 'term + AsRef<str>,
    D: DocAllocator<'term>,
    D::Doc: Clone,
{
    match term {
        Term::Name(_, name) => alloc.text(name.as_ref()),
        Term::Ann(term, r#type) => (alloc.nil())
            .append(pretty_term(alloc, term))
            .append(alloc.space())
            .append(":")
            .append(
                (alloc.space())
                    .append(pretty_term(alloc, r#type))
                    .group()
                    .nest(4),
            ),
        Term::Literal(_, literal) => pretty_literal(alloc, literal),
        Term::Sequence(_, term_entries) => (alloc.nil())
            .append("[")
            .group()
            .append(
                alloc.intersperse(
                    term_entries
                        .iter()
                        .map(|term| pretty_term(alloc, term).group().nest(4)),
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
                            .append(pretty_term(alloc, r#type))
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
                            .append(pretty_term(alloc, term))
                            .append(",")
                            .group()
                            .nest(4),
                    )
                    .nest(4)
                    .group()
            })))
            .append("}"),
        Term::RecordElim(_, head, name) => (alloc.nil())
            .append(pretty_term(alloc, head))
            .append(".")
            .append(name.as_ref()),
        Term::FunctionType(param_type, body_type) => (alloc.nil())
            .append(pretty_term(alloc, param_type))
            .append(alloc.space())
            .append("->")
            .append(alloc.space())
            .append(pretty_term(alloc, body_type)),
        Term::FunctionElim(head, arguments) => pretty_term(alloc, head).append(
            (alloc.nil())
                .append(
                    alloc.concat(
                        arguments
                            .iter()
                            .map(|argument| alloc.space().append(pretty_term(alloc, argument))),
                    ),
                )
                .group()
                .nest(4),
        ),
        Term::Lift(_, term, shift) => (alloc.nil())
            .append(pretty_term(alloc, term))
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
