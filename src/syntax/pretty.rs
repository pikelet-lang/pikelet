//! Pretty printing utilities

extern crate pretty;

use syntax::core::{Binder, Context, Name, RcTerm, RcValue, Term, Value};
use syntax::var::{Named, Var};

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

pub fn pretty_universe() -> StaticDoc {
    Doc::text("Type")
}

pub fn pretty_var(context: Options, var: &Var<Name>) -> StaticDoc {
    match context.debug_indices {
        true => Doc::text(format!("{:#}", var)),
        false => Doc::as_string(var),
    }
}

pub fn pretty_name(context: Options, name: &Name) -> StaticDoc {
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
        if name == &Name::Abstract {
            Doc::group(
                ann.to_doc(context.with_prec(Prec::APP))
                    .append(Doc::space())
                    .append(Doc::text("->")),
            ).append(Doc::group(
                Doc::space()
                    .append(body.to_doc(context.with_prec(Prec::NO_WRAP)))
                    .nest(context.indent_width as usize),
            ))
        } else {
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
        },
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
            Term::Universe => pretty_universe(),
            Term::Var(ref var) => pretty_var(context, var),
            Term::Lam(Named(ref n, ref a), ref b) => pretty_lam(context, n, a.as_ref(), b),
            Term::Pi(Named(ref n, ref a), ref b) => pretty_pi(context, n, a, b),
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
            Value::Universe => pretty_universe(),
            Value::Lam(Named(ref n, ref a), ref b) => pretty_lam(context, n, a.as_ref(), b),
            Value::Pi(Named(ref n, ref a), ref b) => pretty_pi(context, n, a, b),
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

pub fn pretty_binder(context: Options, name: &Name, binder: &Binder) -> StaticDoc {
    match *binder {
        Binder::Lam(ref ann) => Doc::group(
            Doc::text(r"\")
                .append(pretty_name(context, name))
                .append(match ann.as_ref() {
                    Some(ann) => Doc::space()
                        .append(Doc::text(":"))
                        .append(Doc::space())
                        .append(ann.to_doc(context.with_prec(Prec::PI)).group()),
                    None => Doc::nil(),
                }),
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
    }
}

impl ToDoc for Binder {
    fn to_doc(&self, context: Options) -> StaticDoc {
        pretty_binder(context, &Name::Abstract, self)
    }
}

impl ToDoc for Context {
    fn to_doc(&self, context: Options) -> StaticDoc {
        Doc::text("[")
            .append(Doc::intersperse(
                self.binders
                    .iter()
                    .map(|&Named(ref name, ref binder)| pretty_binder(context, name, binder)),
                Doc::text(",").append(Doc::space()),
            ))
            .append(Doc::text("]"))
    }
}
