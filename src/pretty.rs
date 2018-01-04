extern crate pretty;

use core::{CTerm, ITerm, SValue, Value};
use var::{Name, Named};

use self::pretty::{BoxDoc, Doc};

pub const INDENT_WIDTH: usize = 4;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Prec(i8);

impl Prec {
    pub const NO_WRAP: Prec = Prec(-1);
    pub const ANN: Prec = Prec(0);
    pub const LAM: Prec = Prec(1);
    pub const PI: Prec = Prec(1);
    pub const APP: Prec = Prec(10);
}

pub trait ToDoc {
    fn to_doc(&self, prec: Prec) -> Doc<BoxDoc>;
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

fn pretty_ann<'a, E: ToDoc, T: ToDoc>(prec: Prec, expr: &'a E, ty: &'a T) -> Doc<'a, BoxDoc<'a>> {
    parens_if(
        Prec::ANN < prec,
        Doc::nil()
            .append(expr.to_doc(Prec::ANN))
            .append(Doc::space())
            .append(Doc::text(":"))
            .group()
            .append(Doc::space())
            .append(ty.to_doc(Prec::ANN).nest(INDENT_WIDTH)),
    )
}

fn pretty_ty<'a>() -> Doc<'a, BoxDoc<'a>> {
    Doc::text("Type")
}

fn pretty_lam<'a, A: ToDoc, B: ToDoc>(
    prec: Prec,
    name: &'a Name,
    ann: Option<&'a A>,
    body: &'a B,
) -> Doc<'a, BoxDoc<'a>> {
    parens_if(
        Prec::LAM < prec,
        Doc::nil()
            .append(
                Doc::text(r"\")
                    .append(Doc::as_string(name))
                    .append(match ann.as_ref() {
                        Some(ann) => Doc::space()
                            .append(Doc::text(":"))
                            .append(Doc::space())
                            .append(ann.to_doc(Prec::APP).group()),
                        None => Doc::nil(),
                    })
                    .append(Doc::space())
                    .append(Doc::text("=>"))
                    .group(),
            )
            .append(Doc::space())
            .append(body.to_doc(Prec::NO_WRAP).nest(INDENT_WIDTH)),
    )
}

fn pretty_pi<'a, A: ToDoc, B: ToDoc>(
    prec: Prec,
    name: &'a Name,
    ann: &'a A,
    body: &'a B,
) -> Doc<'a, BoxDoc<'a>> {
    parens_if(
        Prec::PI < prec,
        if name.0 == "_" {
            Doc::nil()
                .append(ann.to_doc(Prec::APP))
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc(Prec::NO_WRAP).nest(INDENT_WIDTH))
                .group()
        } else {
            Doc::nil()
                .append(
                    Doc::text("[")
                        .append(Doc::as_string(name))
                        .append(Doc::space())
                        .append(Doc::text(":"))
                        .append(Doc::space())
                        .append(ann.to_doc(Prec::APP))
                        .append(Doc::text("]"))
                        .group(),
                )
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body.to_doc(Prec::NO_WRAP).nest(INDENT_WIDTH))
                .group()
        },
    )
}

fn pretty_app<'a, F: ToDoc, A: ToDoc>(
    prec: Prec,
    fn_term: &'a F,
    arg_term: &'a A,
) -> Doc<'a, BoxDoc<'a>> {
    parens_if(
        Prec::APP < prec,
        Doc::nil()
            .append(fn_term.to_doc(Prec::APP))
            .append(Doc::space())
            .append(arg_term.to_doc(Prec::APP)),
    )
}

impl ToDoc for CTerm {
    fn to_doc(&self, prec: Prec) -> Doc<BoxDoc> {
        match *self {
            CTerm::Inf(ref iterm) => iterm.to_doc(prec),
            CTerm::Lam(Named(ref name, ()), ref body) => {
                pretty_lam(prec, name, None::<&ITerm>, &**body)
            }
        }
    }
}

impl ToDoc for ITerm {
    fn to_doc(&self, prec: Prec) -> Doc<BoxDoc> {
        match *self {
            ITerm::Ann(ref expr, ref ty) => pretty_ann(prec, &**expr, &**ty),
            ITerm::Type => pretty_ty(),
            ITerm::Var(ref var) => Doc::as_string(var),
            ITerm::Lam(Named(ref n, ref a), ref b) => pretty_lam(prec, n, Some(&**a), &**b),
            ITerm::Pi(Named(ref n, ref a), ref b) => pretty_pi(prec, n, &**a, &**b),
            ITerm::App(ref f, ref a) => pretty_app(prec, &**f, &**a),
        }
    }
}

impl ToDoc for Value {
    fn to_doc(&self, prec: Prec) -> Doc<BoxDoc> {
        match *self {
            Value::Type => pretty_ty(),
            Value::Lam(Named(ref n, ref a), ref b) => {
                pretty_lam(prec, n, a.as_ref().map(|a| &**a), &**b)
            }
            Value::Pi(Named(ref n, ref a), ref b) => pretty_pi(prec, n, &**a, &**b),
            Value::Stuck(ref svalue) => svalue.to_doc(prec),
        }
    }
}

impl ToDoc for SValue {
    fn to_doc(&self, prec: Prec) -> Doc<BoxDoc> {
        match *self {
            SValue::Var(ref var) => Doc::as_string(var),
            SValue::App(ref fn_term, ref arg_term) => pretty_app(prec, &**fn_term, &**arg_term),
        }
    }
}
