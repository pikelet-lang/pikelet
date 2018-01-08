extern crate pretty;

use core::{CTerm, ITerm, Neutral, Value};
use var::{Name, Named};

use self::pretty::{BoxDoc, Doc};

/// Configurable parameters for controlling the pretty printer
#[derive(Copy, Clone)]
pub struct Context {
    pub indent_width: u8,
    pub debug_indices: bool,
    pub prec: Prec,
}

impl Default for Context {
    fn default() -> Context {
        Context {
            indent_width: 4,
            debug_indices: false,
            prec: Prec::NO_WRAP,
        }
    }
}

impl Context {
    /// Set the number of spaces to indent by
    pub fn with_indent_width(self, indent_width: u8) -> Context {
        Context {
            indent_width,
            ..self
        }
    }

    /// Set whether the Debruijn indices should be displayed
    pub fn with_debug_indices(self, debug_indices: bool) -> Context {
        Context {
            debug_indices,
            ..self
        }
    }

    /// Set the current precedence of the pretty printer
    pub fn with_prec(self, prec: Prec) -> Context {
        Context { prec, ..self }
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

/// Convert a datatype to a pretty-printable document
pub trait ToDoc {
    fn to_doc(&self, context: Context) -> Doc<BoxDoc>;
}

fn parens_if<'a>(should_wrap: bool, inner: Doc<'a, BoxDoc<'a>>) -> Doc<'a, BoxDoc<'a>> {
    match should_wrap {
        false => inner,
        true => Doc::nil()
            .append(Doc::text("("))
            .append(inner)
            .append(Doc::text(")")),
    }
}

pub fn pretty_ann<'a, E: ToDoc, T: ToDoc>(
    context: Context,
    expr: &'a E,
    ty: &'a T,
) -> Doc<'a, BoxDoc<'a>> {
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

pub fn pretty_ty<'a>() -> Doc<'a, BoxDoc<'a>> {
    Doc::text("Type")
}

pub fn pretty_lam<'a, A: ToDoc, B: ToDoc>(
    context: Context,
    name: &'a Name,
    ann: Option<&'a A>,
    body: &'a B,
) -> Doc<'a, BoxDoc<'a>> {
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

pub fn pretty_pi<'a, A: ToDoc, B: ToDoc>(
    context: Context,
    name: &'a Name,
    ann: &'a A,
    body: &'a B,
) -> Doc<'a, BoxDoc<'a>> {
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
                Doc::text("[")
                    .append(Doc::as_string(name))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(Doc::space())
                    .append(ann.to_doc(context.with_prec(Prec::PI)))
                    .append(Doc::text("]"))
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

pub fn pretty_app<'a, F: ToDoc, A: ToDoc>(
    context: Context,
    fn_term: &'a F,
    arg_term: &'a A,
) -> Doc<'a, BoxDoc<'a>> {
    parens_if(
        Prec::APP < context.prec,
        Doc::nil()
            .append(fn_term.to_doc(context.with_prec(Prec::APP)))
            .append(Doc::space())
            .append(arg_term.to_doc(context.with_prec(Prec::APP))),
    )
}

impl ToDoc for CTerm {
    fn to_doc(&self, context: Context) -> Doc<BoxDoc> {
        match *self {
            CTerm::Inf(ref iterm) => iterm.to_doc(context),
            CTerm::Lam(Named(ref name, ()), ref body) => {
                pretty_lam(context, name, None::<&ITerm>, &**body)
            }
        }
    }
}

impl ToDoc for ITerm {
    fn to_doc(&self, context: Context) -> Doc<BoxDoc> {
        match *self {
            ITerm::Ann(ref expr, ref ty) => pretty_ann(context, &**expr, &**ty),
            ITerm::Type => pretty_ty(),
            ITerm::Var(ref var) => Doc::as_string(var),
            ITerm::Lam(Named(ref n, ref a), ref b) => pretty_lam(context, n, Some(&**a), &**b),
            ITerm::Pi(Named(ref n, ref a), ref b) => pretty_pi(context, n, &**a, &**b),
            ITerm::App(ref f, ref a) => pretty_app(context, &**f, &**a),
        }
    }
}

impl ToDoc for Value {
    fn to_doc(&self, context: Context) -> Doc<BoxDoc> {
        match *self {
            Value::Type => pretty_ty(),
            Value::Lam(Named(ref n, ref a), ref b) => {
                pretty_lam(context, n, a.as_ref().map(|a| &**a), &**b)
            }
            Value::Pi(Named(ref n, ref a), ref b) => pretty_pi(context, n, &**a, &**b),
            Value::Neutral(ref svalue) => svalue.to_doc(context),
        }
    }
}

impl ToDoc for Neutral {
    fn to_doc(&self, context: Context) -> Doc<BoxDoc> {
        match *self {
            Neutral::Var(ref var) => match context.debug_indices {
                true => Doc::text(format!("{:#}", var)),
                false => Doc::as_string(var),
            },
            Neutral::App(ref fn_term, ref arg_term) => pretty_app(context, &**fn_term, &**arg_term),
        }
    }
}
