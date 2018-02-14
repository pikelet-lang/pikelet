#[macro_use]
extern crate failure;
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;
extern crate pretty;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate rpds;
extern crate source;
extern crate unicode_xid;

pub mod semantics;
pub mod syntax;

#[cfg(feature = "cli")]
extern crate isatty;
#[cfg(feature = "cli")]
extern crate rustyline;
#[cfg(feature = "cli")]
#[macro_use]
extern crate structopt;
#[cfg(feature = "cli")]
extern crate term_size;

#[cfg(feature = "cli")]
pub mod cli;
