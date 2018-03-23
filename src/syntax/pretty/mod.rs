//! Pretty printing utilities

use pretty::{BoxDoc, Doc};
use std::rc::Rc;

mod concrete;
mod core;

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
    fn to_doc(&self, options: Options) -> StaticDoc;
}

impl<'a, T: ToDoc> ToDoc for &'a T {
    fn to_doc(&self, options: Options) -> StaticDoc {
        (*self).to_doc(options)
    }
}

impl<T: ToDoc> ToDoc for Rc<T> {
    fn to_doc(&self, options: Options) -> StaticDoc {
        (**self).to_doc(options)
    }
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
