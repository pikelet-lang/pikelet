use pretty::Doc;

use syntax::concrete::{Declaration, Exposing, LamParams, Module, PiParams, Term};

use super::{Options, StaticDoc, ToDoc};

impl ToDoc for Module {
    fn to_doc(&self, options: Options) -> StaticDoc {
        Doc::group(
            Doc::text("module")
                .append(Doc::space())
                .append(Doc::as_string(&self.name.1))
                .append(Doc::text(";")),
        ).append(Doc::newline())
            .append(Doc::newline())
            .append(Doc::intersperse(
                self.declarations
                    .iter()
                    .map(|declaration| declaration.to_doc(options)),
                Doc::newline().append(Doc::newline()),
            ))
    }
}

impl ToDoc for Declaration {
    fn to_doc(&self, options: Options) -> StaticDoc {
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
                    Doc::space().append(exposing.to_doc(options))
                })),
            Declaration::Claim {
                ref name, ref ann, ..
            } => Doc::as_string(&name.1)
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ann.to_doc(options)),
            Declaration::Definition {
                ref name,
                ref params,
                ref body,
                ..
            } => Doc::as_string(&name.1)
                .append(Doc::space())
                .append(pretty_lam_params(options, params))
                .append(Doc::text("="))
                .append(Doc::space())
                .append(body.to_doc(options).nest(options.indent_width as usize)),
        }.append(Doc::text(";"))
    }
}

impl ToDoc for Exposing {
    fn to_doc(&self, _: Options) -> StaticDoc {
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
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self, options: Options) -> StaticDoc {
        match *self {
            Term::Parens(_, ref term) => Doc::text("(")
                .append(term.to_doc(options))
                .append(Doc::text(")")),
            Term::Ann(ref term, ref ty) => term.to_doc(options)
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ty.to_doc(options)),
            Term::Universe(_, level) => {
                Doc::text("Type").append(level.map_or(Doc::nil(), |level| {
                    Doc::space().append(Doc::as_string(level))
                }))
            },
            Term::Var(_, ref name) => Doc::as_string(name),
            Term::Lam(_, ref params, ref body) => Doc::text("\\")
                .append(pretty_lam_params(options, params))
                .append(Doc::space())
                .append(Doc::text("=>"))
                .append(Doc::space())
                .append(body.to_doc(options)),
            Term::Pi(_, ref params, ref body) => Doc::text("(")
                .append(pretty_pi_params(options, params))
                .append(Doc::text(")"))
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc(options)),
            Term::Arrow(ref ann, ref body) => ann.to_doc(options)
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc(options)),
            Term::App(ref fn_term, ref arg) => fn_term
                .to_doc(options)
                .append(Doc::space())
                .append(arg.to_doc(options)),
        }
    }
}

fn pretty_lam_params(options: Options, params: &LamParams) -> StaticDoc {
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
                .append(ann.to_doc(options))
                .append(Doc::text(")")),
        }),
        Doc::space(),
    )
}

fn pretty_pi_params(options: Options, &(ref names, ref ann): &PiParams) -> StaticDoc {
    Doc::text("(")
        .append(Doc::intersperse(
            names.iter().map(|name| Doc::as_string(&name.1)),
            Doc::space(),
        ))
        .append(Doc::space())
        .append(Doc::text(":"))
        .append(Doc::space())
        .append(ann.to_doc(options))
        .append(Doc::text(")"))
}
