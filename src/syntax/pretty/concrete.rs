//! Pretty printing for the concrete syntax

use pretty::Doc;

use syntax::concrete::{Declaration, Exposing, LamParams, Literal, Module, PiParams, Term};

use super::{StaticDoc, ToDoc};

const INDENT_WIDTH: usize = 4;

impl ToDoc for Module {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Module::Valid {
                ref name,
                ref declarations,
            } => Doc::group(
                Doc::text("module")
                    .append(Doc::space())
                    .append(Doc::as_string(&name.1))
                    .append(Doc::text(";")),
            ).append(Doc::newline())
                .append(Doc::newline())
                .append(Doc::intersperse(
                    declarations.iter().map(|declaration| declaration.to_doc()),
                    Doc::newline().append(Doc::newline()),
                )),
            Module::Error(_) => Doc::text("<error>"),
        }
    }
}

impl ToDoc for Declaration {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Declaration::Import {
                ref name,
                ref rename,
                ref exposing,
                ..
            } => Doc::text("module")
                .append(Doc::space())
                .append(Doc::as_string(&name.1))
                .append(rename.as_ref().map_or(Doc::nil(), |&(_, ref rename)| {
                    Doc::space()
                        .append(Doc::text("as"))
                        .append(Doc::space())
                        .append(Doc::as_string(rename))
                }))
                .append(exposing.as_ref().map_or(Doc::nil(), |exposing| {
                    Doc::space().append(exposing.to_doc())
                })),
            Declaration::Claim {
                ref name, ref ann, ..
            } => Doc::as_string(&name.1)
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ann.to_doc()),
            Declaration::Definition { ref wheres, .. } if !wheres.is_empty() => {
                unimplemented!("where clauses")
            },
            Declaration::Definition {
                ref name,
                ref params,
                ref body,
                ref wheres,
                ..
            } => Doc::as_string(name)
                .append(Doc::space())
                .append(pretty_lam_params(params))
                .append(Doc::text("="))
                .append(Doc::space())
                .append(body.to_doc().nest(INDENT_WIDTH))
                .append(if wheres.is_empty() {
                    Doc::nil()
                } else {
                    // FIXME: Indentation
                    Doc::newline()
                        .append(Doc::text("where"))
                        .append(Doc::space())
                        .append(Doc::text("{"))
                        .append(Doc::newline())
                        .append(Doc::intersperse(
                            wheres.iter().map(|w| w.to_doc()),
                            Doc::newline(),
                        ))
                        .append(Doc::newline())
                        .append(Doc::text("}"))
                }),
            Declaration::Error(_) => Doc::text("<error>"),
        }.append(Doc::text(";"))
    }
}

impl ToDoc for Exposing {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Exposing::All(_) => Doc::text("(..)"),
            Exposing::Exact(_, ref imports) => Doc::intersperse(
                imports.iter().map(|&((_, ref name), ref rename)| {
                    Doc::as_string(name).append(rename.as_ref().map_or(
                        Doc::nil(),
                        |&(_, ref rename)| {
                            Doc::space()
                                .append(Doc::text("as"))
                                .append(Doc::space())
                                .append(Doc::as_string(rename))
                        },
                    ))
                }),
                Doc::text(",").append(Doc::space()),
            ),
            Exposing::Error(_) => Doc::text("<error>"),
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Term::Parens(_, ref term) => {
                Doc::text("(").append(term.to_doc()).append(Doc::text(")"))
            },
            Term::Ann(ref term, ref ty) => term.to_doc()
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ty.to_doc()),
            Term::Universe(_, level) => {
                Doc::text("Type").append(level.map_or(Doc::nil(), |level| {
                    Doc::space().append(Doc::as_string(level))
                }))
            },
            Term::Literal(_, Literal::String(ref value)) => Doc::text(format!("{:?}", value)),
            Term::Literal(_, Literal::Char(value)) => Doc::text(format!("{:?}", value)),
            Term::Literal(_, Literal::Int(value)) => Doc::as_string(value),
            Term::Literal(_, Literal::Float(value)) => Doc::as_string(value),
            Term::Hole(_) => Doc::text("_"),
            Term::Var(_, ref name) => Doc::as_string(name),
            Term::Lam(_, ref params, ref body) => Doc::text("\\")
                .append(pretty_lam_params(params))
                .append(Doc::space())
                .append(Doc::text("=>"))
                .append(Doc::space())
                .append(body.to_doc()),
            Term::Pi(_, ref params, ref body) => pretty_pi_params(params)
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc()),
            Term::Arrow(ref ann, ref body) => ann.to_doc()
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc()),
            Term::App(ref fn_term, ref args) => fn_term.to_doc().append(Doc::space()).append(
                Doc::intersperse(args.iter().map(|arg| arg.to_doc()), Doc::space()),
            ),
            Term::Let(_, ref decls, ref body) => {
                Doc::text("let")
                    .append(Doc::space())
                    .append(Doc::intersperse(
                        // FIXME: Indentation
                        decls.iter().map(|decl| decl.to_doc()),
                        Doc::newline(),
                    ))
                    .append(Doc::space())
                    .append(Doc::text("in"))
                    .append(Doc::space())
                    .append(body.to_doc())
            },
            Term::If(_, ref cond, ref if_true, ref if_false) => Doc::text("if")
                .append(Doc::space())
                .append(cond.to_doc())
                .append(Doc::space())
                .append(Doc::text("then"))
                .append(Doc::space())
                .append(if_true.to_doc())
                .append(Doc::space())
                .append(Doc::text("else"))
                .append(Doc::space())
                .append(if_false.to_doc()),
            Term::RecordType(_, ref fields) if fields.is_empty() => Doc::text("Record {}"),
            Term::Record(_, ref fields) if fields.is_empty() => Doc::text("record {}"),
            Term::RecordType(_, ref fields) => Doc::text("Record {")
                .append(Doc::space())
                .append(
                    Doc::intersperse(
                        fields.iter().map(|&(_, ref name, ref ann)| {
                            Doc::as_string(name)
                                .append(Doc::space())
                                .append(Doc::text(":"))
                                .append(Doc::space())
                                .append(ann.to_doc())
                        }),
                        Doc::text(",").append(Doc::space()),
                    ).nest(INDENT_WIDTH),
                )
                .append(Doc::space())
                .append(Doc::text("}")),
            Term::Record(_, ref fields) => Doc::text("record {")
                .append(Doc::space())
                .append(
                    Doc::intersperse(
                        fields.iter().map(|&(_, ref name, ref expr)| {
                            Doc::as_string(name)
                                .append(Doc::space())
                                .append(Doc::text("="))
                                .append(Doc::space())
                                .append(expr.to_doc())
                        }),
                        Doc::text(",").append(Doc::space()),
                    ).nest(INDENT_WIDTH),
                )
                .append(Doc::space())
                .append(Doc::text("}")),
            Term::Proj(ref expr, _, ref label) => expr.to_doc()
                .append(Doc::text("."))
                .append(Doc::as_string(label)),
            Term::Error(_) => Doc::text("<error>"),
        }
    }
}

fn pretty_lam_params(params: &LamParams) -> StaticDoc {
    Doc::intersperse(
        params.iter().map(|&(ref names, ref ann)| match *ann {
            None if names.len() == 1 => Doc::as_string(&names[0].1),
            None => unreachable!(), // FIXME - shouldn't be possible in AST
            Some(ref ann) => Doc::text("(")
                .append(Doc::intersperse(
                    names.iter().map(|name| Doc::as_string(&name.1)),
                    Doc::space(),
                ))
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ann.to_doc())
                .append(Doc::text(")")),
        }),
        Doc::space(),
    )
}

fn pretty_pi_params(params: &PiParams) -> StaticDoc {
    Doc::intersperse(
        params.iter().map(|&(ref names, ref ann)| {
            Doc::text("(")
                .append(Doc::intersperse(
                    names.iter().map(|name| Doc::as_string(&name.1)),
                    Doc::space(),
                ))
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ann.to_doc())
                .append(Doc::text(")"))
        }),
        Doc::space(),
    )
}
