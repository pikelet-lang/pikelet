//! Pretty printing utilities

extern crate pretty;

use self::pretty::termcolor::ColorSpec;
use self::pretty::{BoxDoc, Doc};
use std::rc::Rc;

mod concrete;
mod core;

/// An effectively 'infinite' line length for when we don't have an explicit
/// width provided for pretty printing.
///
/// `pretty.rs` seems to bug-out and break on every line when using
/// `usize::MAX`, so we'll just use a really big number instead...
pub const FALLBACK_WIDTH: usize = 1_000_000;

pub type StaticDoc = Doc<'static, BoxDoc<'static, ColorSpec>, ColorSpec>;

/// Convert a datatype to a pretty-printable document
pub trait ToDoc {
    fn to_doc(&self) -> StaticDoc;
}

impl<'a, T: ToDoc> ToDoc for &'a T {
    fn to_doc(&self) -> StaticDoc {
        (*self).to_doc()
    }
}

impl<T: ToDoc> ToDoc for Rc<T> {
    fn to_doc(&self) -> StaticDoc {
        (**self).to_doc()
    }
}

fn parens(doc: StaticDoc) -> StaticDoc {
    Doc::text("(").append(doc.append(")").nest(1))
}

fn sexpr(name: &'static str, doc: StaticDoc) -> StaticDoc {
    parens(
        Doc::text(name)
            .append(Doc::space())
            .append(doc.nest(name.len())),
    )
}
