//! Pretty printing utilities

use pretty::{BoxDoc, Doc};
use std::rc::Rc;

mod concrete;
mod core;

pub type StaticDoc = Doc<'static, BoxDoc<'static>>;

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
