//! A simple language.

#![allow(clippy::drop_copy)]
#![allow(clippy::drop_ref)]
#![allow(clippy::new_without_default)]
#![allow(clippy::while_let_on_iterator)]

pub mod lang;
pub mod pass;

mod literal;
pub mod reporting;
