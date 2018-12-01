//! The syntax of the language

extern crate codespan;
extern crate codespan_reporting;
#[macro_use]
extern crate failure;
extern crate im;
extern crate lalrpop_util;
#[macro_use]
extern crate moniker;
extern crate pikelet_core;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate pretty;
extern crate unicode_xid;

pub mod desugar;
pub mod elaborate;
pub mod parse;
pub mod resugar;
pub mod syntax;
