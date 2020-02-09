//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::surface::{Literal, Term};

/// The precedence of a term.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Term = 0,
    Expr,
    Arrow,
    App,
    Atomic,
}

pub fn pretty_term<'a, D, S>(alloc: &'a D, term: &'a Term<S>) -> DocBuilder<'a, D>
where
    S: 'a + AsRef<str>,
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    pretty_term_prec(alloc, term, Prec::Term)
}

pub fn pretty_term_prec<'a, D, S>(alloc: &'a D, term: &'a Term<S>, prec: Prec) -> DocBuilder<'a, D>
where
    S: 'a + AsRef<str>,
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match term {
        Term::Name(_, name) => alloc.text(name.as_ref()),
        Term::Ann(term, r#type) => pretty_paren(
            alloc,
            prec > Prec::Term,
            (alloc.nil())
                .append(pretty_term_prec(alloc, term, Prec::Expr))
                .append(alloc.space())
                .append(":")
                .append(
                    (alloc.space())
                        .append(pretty_term_prec(alloc, r#type, Prec::Term))
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
                        .map(|term| pretty_term_prec(alloc, term, Prec::Term).group().nest(4)),
                    alloc.text(",").append(alloc.space()),
                ),
            )
            .append("]"),
        Term::RecordType(_, ty_entries) => (alloc.nil())
            .append("Record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(
                alloc.concat(ty_entries.iter().map(|(_, entry_name, entry_type)| {
                    (alloc.nil())
                        .append(alloc.hardline())
                        .append(alloc.text(entry_name.as_ref()))
                        .append(":")
                        .append(
                            (alloc.space())
                                .append(pretty_term_prec(alloc, entry_type, Prec::Term))
                                .append(",")
                                .group()
                                .nest(4),
                        )
                        .nest(4)
                        .group()
                })),
            )
            .append("}"),
        Term::RecordTerm(_, term_entries) => (alloc.nil())
            .append("record")
            .append(alloc.space())
            .append("{")
            .group()
            .append(
                alloc.concat(term_entries.iter().map(|(_, entry_name, entry_term)| {
                    (alloc.nil())
                        .append(alloc.hardline())
                        .append(alloc.text(entry_name.as_ref()))
                        .append("=")
                        .append(
                            (alloc.space())
                                .append(pretty_term_prec(alloc, entry_term, Prec::Term))
                                .append(",")
                                .group()
                                .nest(4),
                        )
                        .nest(4)
                        .group()
                })),
            )
            .append("}"),
        Term::RecordElim(_, head, name) => (alloc.nil())
            .append(pretty_term_prec(alloc, head, Prec::Atomic))
            .append(".")
            .append(name.as_ref()),
        Term::FunctionType(param_type, body_type) => pretty_paren(
            alloc,
            prec > Prec::Arrow,
            (alloc.nil())
                .append(pretty_term_prec(alloc, param_type, Prec::App))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(pretty_term_prec(alloc, body_type, Prec::Arrow)),
        ),
        Term::FunctionTerm(_, param_names, body) => pretty_paren(
            alloc,
            prec > Prec::Expr,
            (alloc.nil())
                .append("fun")
                .append(alloc.space())
                .append(
                    alloc.intersperse(
                        param_names
                            .iter()
                            .map(|(_, param_name)| param_name.as_ref()),
                        alloc.space(),
                    ),
                )
                .append(alloc.space())
                .append("=>")
                .group()
                .append(
                    (alloc.nil())
                        .append(alloc.space())
                        .append(pretty_term_prec(alloc, body, Prec::Expr).group().nest(4)),
                ),
        ),
        Term::FunctionElim(head, arguments) => pretty_paren(
            alloc,
            prec > Prec::App,
            pretty_term_prec(alloc, head, Prec::App).append(
                (alloc.nil())
                    .append(alloc.concat(arguments.iter().map(|argument| {
                        alloc
                            .space()
                            .append(pretty_term_prec(alloc, argument, Prec::Arrow))
                    })))
                    .group()
                    .nest(4),
            ),
        ),
        Term::Lift(_, term, shift) => (alloc.nil())
            .append(pretty_term_prec(alloc, term, Prec::Atomic))
            .append("^")
            .append(shift.to_string()),
        Term::Error(_) => alloc.text("!"),
    }
}

pub fn pretty_literal<'a, D, S>(alloc: &'a D, literal: &'a Literal<S>) -> DocBuilder<'a, D>
where
    S: 'a + AsRef<str>,
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match literal {
        Literal::Char(text) | Literal::String(text) | Literal::Number(text) => {
            alloc.text(text.as_ref())
        }
    }
}

fn pretty_paren<'a, D>(alloc: &'a D, b: bool, doc: DocBuilder<'a, D>) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    if b {
        alloc.text("(").append(doc).append(")")
    } else {
        doc
    }
}
