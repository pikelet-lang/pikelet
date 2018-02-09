//! Pretty printing utilities

extern crate pretty;

use syntax::core::{Definition, Module};
use syntax::core::{Binder, Context, Level, Name, RcTerm, RcValue, Term, Value};
use syntax::var::Var;

use self::pretty::{BoxDoc, Doc};

/// Configurable parameters for controlling the pretty printer
#[derive(Copy, Clone)]
pub struct Options {
    pub indent_width: u8,
    pub debug_indices: bool,
    pub prec: Prec,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            indent_width: 4,
            debug_indices: false,
            prec: Prec::NO_WRAP,
        }
    }
}

impl Options {
    /// Set the number of spaces to indent by
    pub fn with_indent_width(self, indent_width: u8) -> Options {
        Options {
            indent_width,
            ..self
        }
    }

    /// Set whether the Debruijn indices should be displayed
    pub fn with_debug_indices(self, debug_indices: bool) -> Options {
        Options {
            debug_indices,
            ..self
        }
    }

    /// Set the current precedence of the pretty printer
    pub fn with_prec(self, prec: Prec) -> Options {
        Options { prec, ..self }
    }
}

/// The precedence of the pretty printer
///
/// This is used to reconstruct the parentheses needed to reconstruct a valid
/// syntax tree
#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Prec(i8);

impl Prec {
    pub const NO_WRAP: Prec = Prec(-1);
    pub const ANN: Prec = Prec(0);
    pub const LAM: Prec = Prec(1);
    pub const PI: Prec = Prec(2);
    pub const APP: Prec = Prec(10);
}

pub type StaticDoc = Doc<'static, BoxDoc<'static>>;

/// Convert a datatype to a pretty-printable document
pub trait ToDoc {
    fn to_doc(&self, context: Options) -> StaticDoc;
}

fn parens_if(should_wrap: bool, inner: StaticDoc) -> StaticDoc {
    match should_wrap {
        false => inner,
        true => Doc::nil()
            .append(Doc::text("("))
            .append(inner)
            .append(Doc::text(")")),
    }
}

pub fn pretty_ann<E: ToDoc, T: ToDoc>(context: Options, expr: &E, ty: &T) -> StaticDoc {
    parens_if(
        Prec::ANN < context.prec,
        Doc::group(
            expr.to_doc(context.with_prec(Prec::LAM))
                .append(Doc::space())
                .append(Doc::text(":")),
        ).append(Doc::group(
            Doc::space()
                .append(ty.to_doc(context.with_prec(Prec::ANN)))
                .nest(context.indent_width as usize),
        )),
    )
}

pub fn pretty_universe(context: Options, level: Level) -> StaticDoc {
    if level == Level(0) {
        Doc::text("Type")
    } else {
        parens_if(
            Prec::APP < context.prec,
            Doc::text(format!("Type {}", level)),
        )
    }
}

pub fn pretty_var(context: Options, var: &Var<Name>) -> StaticDoc {
    match context.debug_indices {
        true => Doc::text(format!("{:#}", var)),
        false => Doc::as_string(var),
    }
}

pub fn pretty_name(context: Options, name: &Name) -> StaticDoc {
    // FIXME: pretty names
    match context.debug_indices {
        true => Doc::text(format!("{:#}", name)),
        false => Doc::as_string(name),
    }
}

pub fn pretty_lam<A: ToDoc, B: ToDoc>(
    context: Options,
    name: &Name,
    ann: Option<&A>,
    body: &B,
) -> StaticDoc {
    parens_if(
        Prec::LAM < context.prec,
        Doc::group(
            Doc::text(r"\")
                .append(Doc::as_string(name))
                .append(match ann.as_ref() {
                    Some(ann) => Doc::space()
                        .append(Doc::text(":"))
                        .append(Doc::space())
                        .append(ann.to_doc(context.with_prec(Prec::PI)).group()),
                    None => Doc::nil(),
                })
                .append(Doc::space())
                .append(Doc::text("=>")),
        ).append(Doc::group(
            Doc::space()
                .append(body.to_doc(context.with_prec(Prec::NO_WRAP)))
                .nest(context.indent_width as usize),
        )),
    )
}

pub fn pretty_pi<A: ToDoc, B: ToDoc>(
    context: Options,
    name: &Name,
    ann: &A,
    body: &B,
) -> StaticDoc {
    parens_if(
        Prec::PI < context.prec,
        // FIXME: print arrows
        // if body.is_closed() {
        //     Doc::group(
        //         ann.to_doc(context.with_prec(Prec::APP))
        //             .append(Doc::space())
        //             .append(Doc::text("->")),
        //     ).append(Doc::group(
        //         Doc::space()
        //             .append(body.to_doc(context.with_prec(Prec::NO_WRAP)))
        //             .nest(context.indent_width as usize),
        //     ))
        // } else {
            Doc::group(
                Doc::text("(")
                    .append(Doc::as_string(name))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(Doc::space())
                    .append(ann.to_doc(context.with_prec(Prec::PI)))
                    .append(Doc::text(")"))
                    .append(Doc::space())
                    .append(Doc::text("->")),
            ).append(Doc::group(
                Doc::space()
                    .append(body.to_doc(context.with_prec(Prec::NO_WRAP)))
                    .nest(context.indent_width as usize),
            ))
        // },
    )
}

pub fn pretty_app<F: ToDoc, A: ToDoc>(context: Options, fn_term: &F, arg_term: &A) -> StaticDoc {
    parens_if(
        Prec::APP < context.prec,
        Doc::nil()
            .append(fn_term.to_doc(context.with_prec(Prec::APP)))
            .append(Doc::space())
            .append(arg_term.to_doc(context.with_prec(Prec::APP))),
    )
}

impl ToDoc for Term {
    fn to_doc(&self, context: Options) -> StaticDoc {
        match *self {
            Term::Ann(ref expr, ref ty) => pretty_ann(context, expr, ty),
            Term::Universe(level) => pretty_universe(context, level),
            Term::Var(ref var) => pretty_var(context, var),
            Term::Lam(ref lam) => pretty_lam(
                context,
                &lam.unsafe_param.name,
                lam.unsafe_param.inner.as_ref(),
                &lam.unsafe_body,
            ),
            Term::Pi(ref pi) => pretty_pi(
                context,
                &pi.unsafe_param.name,
                &pi.unsafe_param.inner,
                &pi.unsafe_body,
            ),
            Term::App(ref f, ref a) => pretty_app(context, f, a),
        }
    }
}

impl ToDoc for RcTerm {
    fn to_doc(&self, context: Options) -> StaticDoc {
        self.inner.to_doc(context)
    }
}

impl ToDoc for Value {
    fn to_doc(&self, context: Options) -> StaticDoc {
        match *self {
            Value::Universe(level) => pretty_universe(context, level),
            Value::Lam(ref lam) => pretty_lam(
                context,
                &lam.unsafe_param.name,
                lam.unsafe_param.inner.as_ref(),
                &lam.unsafe_body,
            ),
            Value::Pi(ref pi) => pretty_pi(
                context,
                &pi.unsafe_param.name,
                &pi.unsafe_param.inner,
                &pi.unsafe_body,
            ),
            Value::Var(ref var) => pretty_var(context, var),
            Value::App(ref fn_term, ref arg_term) => pretty_app(context, fn_term, arg_term),
        }
    }
}

impl ToDoc for RcValue {
    fn to_doc(&self, context: Options) -> StaticDoc {
        self.inner.to_doc(context)
    }
}

impl ToDoc for Context {
    fn to_doc(&self, context: Options) -> StaticDoc {
        Doc::text("[")
            .append(Doc::intersperse(
                self.binders
                    .iter()
                    .map(|&(ref name, ref binder)| match *binder {
                        Binder::Lam(ref ann) => Doc::group(
                            Doc::text(r"\").append(pretty_name(context, name)).append(
                                match ann.as_ref() {
                                    Some(ann) => Doc::space()
                                        .append(Doc::text(":"))
                                        .append(Doc::space())
                                        .append(ann.to_doc(context.with_prec(Prec::PI)).group()),
                                    None => Doc::nil(),
                                },
                            ),
                        ),
                        Binder::Pi(ref ann) => Doc::group(
                            Doc::text("(")
                                .append(pretty_name(context, name))
                                .append(Doc::space())
                                .append(Doc::text(":"))
                                .append(Doc::space())
                                .append(ann.to_doc(context.with_prec(Prec::PI)))
                                .append(Doc::text(")")),
                        ),
                        Binder::Let(ref ann, ref value) => Doc::group(
                            Doc::text("let")
                                .append(Doc::space())
                                .append(pretty_name(context, name))
                                .append(Doc::space())
                                .append(Doc::text(":"))
                                .append(Doc::space())
                                .append(ann.to_doc(context.with_prec(Prec::PI)))
                                .append(Doc::space())
                                .append(Doc::text("="))
                                .append(Doc::space())
                                .append(value.to_doc(context.with_prec(Prec::PI))),
                        ),
                    }),
                Doc::text(",").append(Doc::space()),
            ))
            .append(Doc::text("]"))
    }
}

impl ToDoc for Definition {
    fn to_doc(&self, context: Options) -> StaticDoc {
        match self.ann {
            None => Doc::nil(),
            Some(ref ann) => Doc::group(
                Doc::as_string(&self.name)
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(Doc::space())
                    .append(ann.to_doc(context.with_prec(Prec::NO_WRAP)))
                    .append(Doc::text(";")),
            ).append(Doc::newline()),
        }.append(Doc::group(
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::space())
                .append(self.term.to_doc(context.with_prec(Prec::NO_WRAP)))
                .append(Doc::text(";")),
        ))
    }
}

impl ToDoc for Module {
    fn to_doc(&self, context: Options) -> StaticDoc {
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
                    .map(|definition| definition.to_doc(context.with_prec(Prec::NO_WRAP))),
                Doc::newline().append(Doc::newline()),
            ))
    }
}
