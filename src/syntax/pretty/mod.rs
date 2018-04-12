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
}

impl Default for Options {
    fn default() -> Options {
        Options {
            indent_width: 4,
            debug_indices: false,
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
