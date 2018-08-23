//! The Pikelet Compiler
//!
//! # Compiler Architecture
//!
//! In order to create a separation of concerns, we break up our compiler into many
//! small stages, beginning with a source string, and ultimately ending up with
//! compiled machine code.
//!
//! Below is a rough flow chart showing how source strings are currently lexed,
//! parsed, desugared, and type checked/elaborated:
//!
//! ```bob
//!          .------------.
//!          |   String   |
//!          '------------'
//!                 |
//! - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Frontend        |
//!                 |
//!       syntax::parse::lexer
//!                 |
//!                 v
//!   .-----------------------------.
//!   | syntax::parse::lexer::Token |
//!   '-----------------------------'
//!                 |
//!      syntax::parse::grammar
//!                 |
//!                 v
//!     .------------------------.
//!     | syntax::concrete::Term |---------> Code formatter (TODO)
//!     '------------------------'
//!                 |
//!    syntax::translation::desugar
//!                 |
//!                 v
//!       .-------------------.
//!       | syntax::raw::Term |
//!       '-------------------'
//!                 |                        .---------------------.
//!     semantics::{check,infer} <---------- | syntax::core::Value |
//!                 |                        '---------------------'
//!                 v                                    ^
//!       .--------------------.                         |
//!       | syntax::core::Term | - semantics::normalize -'
//!       '--------------------'
//!                 |
//!                 |
//! - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Middle (TODO)   |
//!                 |
//!                 v
//!         A-Normal Form (ANF)
//!                 |
//!                 v
//!       Closure Conversion (CC)
//!                 |
//!                 v
//!    Static Single Assignment (SSA)
//!                 |
//!                 |
//! - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Backend (TODO)  |
//!                 |
//!                 v
//!              Codegen
//!                 |
//!                 *-------> Bytecode?
//!                 |
//!                 *-------> WASM?
//!                 |
//!                 *-------> Cranelift IR?
//!                 |
//!                 '-------> LLVM IR?
//! ```
//!
//! As you can see we have only built the front-end as of the time of writing. When
//! we begin to build out a [compiler back end](https://github.com/pikelet-lang/pikelet/issues/9),
//! more stages will be added after type checking and elaboration.
//!
//! ## Name binding
//!
//! Name binding is a surprisingly challenging thing to implement in type checkers
//! and compilers. We use the [`moniker` crate](https://github.com/brendanzab/moniker)
//! for this. Unfortunately this uses a quite slow method of name binding, and could
//! result in performance blowouts in the future. This is something to keep an eye on!
//!
//! ## Performance considerations
//!
//! As you can see from the diagram above, this architecture leads to an
//! easy-to-reason about pipeline. It does however result in the creation of lots of
//! intermediate allocations of heap-allocated tree data structures that will
//! ultimately be discarded. This is quite similar to the problem we face with
//! iterators:
//!
//! ```rust,ignore
//! // 'internal' iteration
//! vec![1, 2, 3].map(|x| x * x).filter(|x| x < 3)
//!
//! // 'external' iteration
//! vec![1, 2, 3].iter().map(|x| x * x).filter(|x| x < 3).collect()
//! ```
//!
//! The first example, which uses 'internal' iteration allocates a new collection
//! after each operation, resulting in three allocated collections. We can improve
//! the performance however by using 'external' iteration - ie. returning a series
//! of chained iterator adaptors, that only perform the allocation on the call to
//! `collect`. This emulates the 'fusion' that languages like Haskell perform to
//! reduce intermediate allocations.
//!
//! We could potentially get some fusion between the stages of our compiler by way
//! of the [visitor pattern](https://github.com/pikelet-lang/pikelet/issues/75).
//!
//! ## Support for interactive development
//!
//! It would be interesting to see how Pikelet could be implemented using an
//! [asynchronous, query-based architecture](https://github.com/pikelet-lang/pikelet/issues/103).
//! This will become more important as the demands of interactive development
//! and incremental compilation become more pressing. In this model we would
//! have to think of compilation as less a pure function from source code to
//! machine code, and more as interacting with a database.
//!
//! ### Resources
//!
//! - [Queries: demand-driven compilation (Rustc Book)](https://rust-lang-nursery.github.io/rustc-guide/query.html)
//! - [Anders Hejlsberg on Modern Compiler Construction (YouTube)](https://www.youtube.com/watch?v=wSdV1M7n4gQ)

extern crate codespan;
extern crate codespan_reporting;
#[macro_use]
extern crate failure;
#[cfg(test)]
extern crate goldenfile;
#[macro_use]
extern crate im;
extern crate lalrpop_util;
#[macro_use]
extern crate moniker;
extern crate pretty;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate unicode_xid;

mod library;
pub mod semantics;
pub mod syntax;

#[cfg(feature = "cli")]
extern crate linefeed;
#[cfg(feature = "cli")]
#[macro_use]
extern crate structopt;
#[cfg(feature = "cli")]
extern crate term_size;

#[cfg(feature = "cli")]
pub mod cli;

use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting::Diagnostic;

use syntax::core;

pub fn load_file(file: &FileMap) -> Result<core::Module, Vec<Diagnostic>> {
    use semantics::TcEnv;
    use syntax::translation::{Desugar, DesugarEnv};

    let (concrete_module, errors) = syntax::parse::module(&file);
    let mut diagnostics = errors
        .iter()
        .map(|err| err.to_diagnostic())
        .collect::<Vec<_>>();

    let raw_module = concrete_module.desugar(&DesugarEnv::new());
    semantics::check_module(&TcEnv::default(), &raw_module).map_err(|err| {
        diagnostics.push(err.to_diagnostic());
        diagnostics
    })
}

pub fn load_prelude(codemap: &mut CodeMap) -> core::Module {
    let file = codemap.add_filemap(
        FileName::real("library/prelude.pi"),
        String::from(library::PRELUDE),
    );

    load_file(&file).unwrap_or_else(|_diagnostics| {
        // for diagnostic in diagnostics {
        //     codespan_reporting::emit(codemap, &diagnostic);
        // }
        panic!("unexpected errors in prelude");
    })
}
