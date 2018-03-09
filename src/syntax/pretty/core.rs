//! Pretty printing for the core syntax

use pretty::Doc;

use syntax::core::{Definition, Module};
use syntax::core::{Binder, Context, Level, Name, RawTerm, RcRawTerm, RcValue, Value};
use syntax::var::{Debruijn, Var};

use super::{parens_if, Options, Prec, StaticDoc, ToDoc};

pub fn pretty_ann<E: ToDoc, T: ToDoc>(options: Options, expr: &E, ty: &T) -> StaticDoc {
    parens_if(
        Prec::ANN < options.prec,
        Doc::group(
            expr.to_doc(options.with_prec(Prec::LAM))
                .append(Doc::space())
                .append(Doc::text(":")),
        ).append(Doc::group(
            Doc::space()
                .append(ty.to_doc(options.with_prec(Prec::ANN)))
                .nest(options.indent_width as usize),
        )),
    )
}

pub fn pretty_universe(options: Options, level: Level) -> StaticDoc {
    if level == Level(0) {
        Doc::text("Type")
    } else {
        parens_if(
            Prec::APP < options.prec,
            Doc::text(format!("Type {}", level)),
        )
    }
}

pub fn pretty_var(options: Options, var: &Var<Name, Debruijn>) -> StaticDoc {
    match options.debug_indices {
        true => Doc::text(format!("{:#}", var)),
        false => Doc::as_string(var),
    }
}

pub fn pretty_name(options: Options, name: &Name) -> StaticDoc {
    // FIXME: pretty names
    match options.debug_indices {
        true => Doc::text(format!("{:#}", name)),
        false => Doc::as_string(name),
    }
}

pub fn pretty_lam<A: ToDoc, B: ToDoc>(
    options: Options,
    name: &Name,
    ann: Option<&A>,
    body: &B,
) -> StaticDoc {
    parens_if(
        Prec::LAM < options.prec,
        Doc::group(
            Doc::text(r"\")
                .append(Doc::as_string(name))
                .append(match ann.as_ref() {
                    Some(ann) => Doc::space()
                        .append(Doc::text(":"))
                        .append(Doc::space())
                        .append(ann.to_doc(options.with_prec(Prec::PI)).group()),
                    None => Doc::nil(),
                })
                .append(Doc::space())
                .append(Doc::text("=>")),
        ).append(Doc::group(
            Doc::space()
                .append(body.to_doc(options.with_prec(Prec::NO_WRAP)))
                .nest(options.indent_width as usize),
        )),
    )
}

pub fn pretty_pi<A: ToDoc, B: ToDoc>(
    options: Options,
    name: &Name,
    ann: &A,
    body: &B,
) -> StaticDoc {
    parens_if(
        Prec::PI < options.prec,
        Doc::group(
            Doc::text("(")
                .append(Doc::as_string(name))
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(ann.to_doc(options.with_prec(Prec::PI)))
                .append(Doc::text(")"))
                .append(Doc::space())
                .append(Doc::text("->")),
        ).append(Doc::group(
            Doc::space()
                .append(body.to_doc(options.with_prec(Prec::NO_WRAP)))
                .nest(options.indent_width as usize),
        )),
    )
}

pub fn pretty_app<F: ToDoc, A: ToDoc>(options: Options, fn_term: &F, arg_term: &A) -> StaticDoc {
    parens_if(
        Prec::APP < options.prec,
        Doc::nil()
            .append(fn_term.to_doc(options.with_prec(Prec::APP)))
            .append(Doc::space())
            .append(arg_term.to_doc(options.with_prec(Prec::APP))),
    )
}

impl ToDoc for RawTerm {
    fn to_doc(&self, options: Options) -> StaticDoc {
        match *self {
            RawTerm::Ann(_, ref expr, ref ty) => pretty_ann(options, expr, ty),
            RawTerm::Universe(_, level) => pretty_universe(options, level),
            RawTerm::Var(_, ref var) => pretty_var(options, var),
            RawTerm::Lam(_, ref lam) => pretty_lam(
                options,
                &lam.unsafe_binder.name,
                lam.unsafe_binder.inner.as_ref(),
                &lam.unsafe_body,
            ),
            RawTerm::Pi(_, ref pi) => pretty_pi(
                options,
                &pi.unsafe_binder.name,
                &pi.unsafe_binder.inner,
                &pi.unsafe_body,
            ),
            RawTerm::App(_, ref f, ref a) => pretty_app(options, f, a),
        }
    }
}

impl ToDoc for RcRawTerm {
    fn to_doc(&self, options: Options) -> StaticDoc {
        self.inner.to_doc(options)
    }
}

impl ToDoc for Value {
    fn to_doc(&self, options: Options) -> StaticDoc {
        match *self {
            Value::Universe(level) => pretty_universe(options, level),
            Value::Lam(ref lam) => pretty_lam(
                options,
                &lam.unsafe_binder.name,
                lam.unsafe_binder.inner.as_ref(),
                &lam.unsafe_body,
            ),
            Value::Pi(ref pi) => pretty_pi(
                options,
                &pi.unsafe_binder.name,
                &pi.unsafe_binder.inner,
                &pi.unsafe_body,
            ),
            Value::Var(ref var) => pretty_var(options, var),
            Value::App(ref fn_term, ref arg_term) => pretty_app(options, fn_term, arg_term),
        }
    }
}

impl ToDoc for RcValue {
    fn to_doc(&self, options: Options) -> StaticDoc {
        self.inner.to_doc(options)
    }
}

impl ToDoc for Context {
    fn to_doc(&self, options: Options) -> StaticDoc {
        Doc::text("[")
            .append(Doc::intersperse(
                self.binders
                    .iter()
                    .map(|&(ref name, ref binder)| match *binder {
                        Binder::Lam { ref ann } => Doc::group(
                            Doc::text(r"\").append(pretty_name(options, name)).append(
                                match ann.as_ref() {
                                    Some(ann) => Doc::space()
                                        .append(Doc::text(":"))
                                        .append(Doc::space())
                                        .append(ann.to_doc(options.with_prec(Prec::PI)).group()),
                                    None => Doc::nil(),
                                },
                            ),
                        ),
                        Binder::Pi { ref ann } => Doc::group(
                            Doc::text("(")
                                .append(pretty_name(options, name))
                                .append(Doc::space())
                                .append(Doc::text(":"))
                                .append(Doc::space())
                                .append(ann.to_doc(options.with_prec(Prec::PI)))
                                .append(Doc::text(")")),
                        ),
                        Binder::Let { ref ann, ref value } => Doc::group(
                            Doc::text("let")
                                .append(Doc::space())
                                .append(pretty_name(options, name))
                                .append(Doc::space())
                                .append(Doc::text(":"))
                                .append(Doc::space())
                                .append(ann.to_doc(options.with_prec(Prec::PI)))
                                .append(Doc::space())
                                .append(Doc::text("="))
                                .append(Doc::space())
                                .append(value.to_doc(options.with_prec(Prec::PI))),
                        ),
                    }),
                Doc::text(",").append(Doc::space()),
            ))
            .append(Doc::text("]"))
    }
}

impl ToDoc for Definition {
    fn to_doc(&self, options: Options) -> StaticDoc {
        match self.ann {
            None => Doc::nil(),
            Some(ref ann) => Doc::group(
                Doc::as_string(&self.name)
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(Doc::space())
                    .append(ann.to_doc(options.with_prec(Prec::NO_WRAP)))
                    .append(Doc::text(";")),
            ).append(Doc::newline()),
        }.append(Doc::group(
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::space())
                .append(self.term.to_doc(options.with_prec(Prec::NO_WRAP)))
                .append(Doc::text(";")),
        ))
    }
}

impl ToDoc for Module {
    fn to_doc(&self, options: Options) -> StaticDoc {
        Doc::group(
            Doc::text("module")
                .append(Doc::space())
                .append(Doc::as_string(&self.name))
                .append(Doc::text(";")),
        ).append(Doc::newline())
            .append(Doc::newline())
            .append(Doc::intersperse(
                self.definitions
                    .iter()
                    .map(|definition| definition.to_doc(options.with_prec(Prec::NO_WRAP))),
                Doc::newline().append(Doc::newline()),
            ))
    }
}
