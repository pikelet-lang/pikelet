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
//!              .------------.
//!              |   String   |
//!              '------------'
//!                     |
//! - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Frontend            |
//!                     |
//!        pikelet_syntax::parse::lexer
//!                     |
//!                     v
//!   .-------------------------------------.
//!   | pikelet_syntax::parse::lexer::Token |
//!   '-------------------------------------'
//!                     |
//!       pikelet_syntax::parse::grammar
//!                     |
//!                     v
//!     .--------------------------------.
//!     | pikelet_syntax::concrete::Term |---------> Code formatter (TODO)
//!     '--------------------------------'
//!                     |
//!     pikelet_syntax::translation::desugar
//!                     |
//!                     v
//!       .---------------------------.
//!       | pikelet_syntax::raw::Term |
//!       '---------------------------'
//!                     |                                .-----------------------------.
//!      pikelet_elaborate::{check,infer} <------------- | pikelet_syntax::core::Value |
//!                     |                                '-----------------------------'
//!                     v                                                ^
//!       .----------------------------.                                 |
//!       | pikelet_syntax::core::Term | - pikelet_elaborate::normalize -'
//!       '----------------------------'
//!                     |
//!                     |
//! - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Middle (TODO)       |
//!                     |
//!                     v
//!             A-Normal Form (ANF)
//!                     |
//!                     v
//!           Closure Conversion (CC)
//!                     |
//!                     v
//!        Static Single Assignment (SSA)
//!                     |
//!                     |
//! - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Backend (TODO)      |
//!                     |
//!                     v
//!                  Codegen
//!                     |
//!                     *-------> Bytecode?
//!                     |
//!                     *-------> WASM?
//!                     |
//!                     *-------> Cranelift IR?
//!                     |
//!                     '-------> LLVM IR?
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
extern crate pikelet_elaborate;
extern crate pikelet_library;
extern crate pikelet_syntax;

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
use std::sync::Arc;

use pikelet_elaborate::{Context, Import};
use pikelet_syntax::translation::{Desugar, DesugarEnv, Resugar};
use pikelet_syntax::{core, raw};

/// An environment that keeps track of the state of a Pikelet program during
/// compilation or interactive sessions
#[derive(Debug, Clone)]
pub struct Driver {
    /// The base type checking context, containing the built-in definitions
    context: Context,
    /// The base desugar environment, using the definitions from the `context`
    desugar_env: DesugarEnv,
    /// A codemap that owns the source code for any terms that are currently loaded
    code_map: CodeMap,
}

impl Driver {
    /// Create a new Pikelet environment, containing only the built-in definitions
    pub fn new() -> Driver {
        let context = Context::default();
        let desugar_env = DesugarEnv::new(context.mappings());

        Driver {
            context,
            desugar_env,
            code_map: CodeMap::new(),
        }
    }

    /// Create a new Pikelet environment, with the prelude loaded as well
    pub fn with_prelude() -> Driver {
        let mut pikelet = Driver::new();

        let prim_path = "prim".to_owned();
        let prim_name = FileName::virtual_("prim");
        let prim_src = pikelet_library::PRIM.to_owned();
        let prim_file = pikelet.code_map.add_filemap(prim_name, prim_src);

        let prelude_path = "prelude".to_owned();
        let prelude_name = FileName::virtual_("prelude");
        let prelude_src = pikelet_library::PRELUDE.to_owned();
        let prelude_file = pikelet.code_map.add_filemap(prelude_name, prelude_src);

        pikelet.load_file(prim_path, prim_file).unwrap();
        pikelet.load_file(prelude_path, prelude_file).unwrap();

        pikelet
    }

    pub fn load_file(
        &mut self,
        internal_path: String,
        file_map: Arc<FileMap>,
    ) -> Result<(), Vec<Diagnostic>> {
        let (concrete_term, _import_paths, errors) = pikelet_syntax::parse::term(&file_map);
        if !errors.is_empty() {
            return Err(errors.iter().map(|error| error.to_diagnostic()).collect());
        }
        let raw_term = self.desugar(&concrete_term)?;
        let (term, ty) = self.infer_term(&raw_term)?;
        // FIXME: Check if import already exists
        self.context
            .insert_import(internal_path, Import::Term(term), ty);

        Ok(())
    }

    pub fn desugar<T>(&self, src: &impl Desugar<T>) -> Result<T, Vec<Diagnostic>> {
        src.desugar(&self.desugar_env)
            .map_err(|e| vec![e.to_diagnostic()])
    }

    pub fn infer_bind_term(
        &mut self,
        name: &str,
        raw_term: &raw::RcTerm,
    ) -> Result<(core::RcTerm, core::RcType), Vec<Diagnostic>> {
        let (term, inferred) = self.infer_term(&raw_term)?;

        let fv = self.desugar_env.on_binding(&name);
        self.context
            .insert_declaration(fv.clone(), inferred.clone());
        self.context.insert_definition(fv.clone(), term.clone());

        Ok((term, inferred))
    }

    pub fn infer_term(
        &self,
        raw_term: &raw::RcTerm,
    ) -> Result<(core::RcTerm, core::RcType), Vec<Diagnostic>> {
        pikelet_elaborate::infer_term(&self.context, &raw_term).map_err(|e| vec![e.to_diagnostic()])
    }

    pub fn nf_term(&self, term: &core::RcTerm) -> Result<core::RcValue, Vec<Diagnostic>> {
        pikelet_elaborate::nf_term(&self.context, term).map_err(|e| vec![e.to_diagnostic()])
    }

    pub fn resugar<T>(&self, src: &impl Resugar<T>) -> T {
        self.context.resugar(src)
    }
}

#[cfg(test)]
mod tests {
    use codespan_reporting::termcolor::{ColorChoice, StandardStream};

    use super::*;

    #[test]
    fn with_prelude() {
        let _pikelet = Driver::with_prelude();
    }

    #[test]
    fn prelude() {
        let mut pikelet = Driver::new();
        let writer = StandardStream::stdout(ColorChoice::Always);

        let prim_path = "prim".to_owned();
        let prim_name = FileName::virtual_("prim");
        let prim_src = pikelet_library::PRIM.to_owned();
        let prim_file = pikelet.code_map.add_filemap(prim_name, prim_src);

        let prelude_path = "prelude".to_owned();
        let prelude_name = FileName::virtual_("prelude");
        let prelude_src = pikelet_library::PRELUDE.to_owned();
        let prelude_file = pikelet.code_map.add_filemap(prelude_name, prelude_src);

        if let Err(diagnostics) = pikelet.load_file(prim_path, prim_file) {
            for diagnostic in diagnostics {
                codespan_reporting::emit(&mut writer.lock(), &pikelet.code_map, &diagnostic)
                    .unwrap();
            }
            panic!("load error!")
        }

        if let Err(diagnostics) = pikelet.load_file(prelude_path, prelude_file) {
            for diagnostic in diagnostics {
                codespan_reporting::emit(&mut writer.lock(), &pikelet.code_map, &diagnostic)
                    .unwrap();
            }
            panic!("load error!")
        }
    }
}
