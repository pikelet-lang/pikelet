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
//!                .------------.
//!                |   String   |
//!                '------------'
//!                       |
//! - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Frontend              |
//!                       |
//!        pikelet_concrete::parse::lexer
//!                       |
//!                       v
//!    .---------------------------------------.
//!    | pikelet_concrete::parse::lexer::Token |
//!    '---------------------------------------'
//!                       |
//!           pikelet_concrete::parse
//!                       |
//!                       v
//!  .------------------------------------------.
//!  | pikelet_concrete::syntax::concrete::Term |---------> Code formatter (TODO)
//!  '------------------------------------------'
//!                       |
//!           pikelet_concrete::desugar
//!                       |
//!                       v
//!    .-------------------------------------.
//!    | pikelet_concrete::syntax::raw::Term |
//!    '-------------------------------------'
//!                       |                                  .-------------------------------------.
//!   pikelet_concrete::elaborate::{check,infer} <---------- | pikelet_core::syntax::domain::Value |
//!                       |                                  '-------------------------------------'
//!                       v                                                    ^
//!     .----------------------------------.                                   |
//!     | pikelet_core::syntax::core::Term | -- pikelet_core::normalize -------'
//!     '----------------------------------'
//!                       |
//!                       |
//! - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Middle (TODO)         |
//!                       |
//!                       v
//!               A-Normal Form (ANF)
//!                       |
//!                       v
//!             Closure Conversion (CC)
//!                       |
//!                       v
//!          Static Single Assignment (SSA)
//!                       |
//!                       |
//! - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//! Backend (TODO)        |
//!                       |
//!                       v
//!                    Codegen
//!                       |
//!                       *-------> Bytecode?
//!                       |
//!                       *-------> WASM?
//!                       |
//!                       *-------> Cranelift IR?
//!                       |
//!                       '-------> LLVM IR?
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

use codespan::CodeMap;
pub use codespan::FileName;
pub use codespan_reporting::{termcolor, ColorArg, Diagnostic};
use std::io;

use pikelet_concrete::desugar::{Desugar, DesugarEnv};
use pikelet_concrete::elaborate::Context;
use pikelet_concrete::resugar::Resugar;
use pikelet_concrete::syntax::raw;
use pikelet_core::syntax::{core, domain, Import};

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

        pikelet
            .register_file(
                "prim".to_owned(),
                FileName::virtual_("prim"),
                pikelet_library::PRIM.to_owned(),
            )
            .unwrap();

        pikelet
            .register_file(
                "prelude".to_owned(),
                FileName::virtual_("prelude"),
                pikelet_library::PRELUDE.to_owned(),
            )
            .unwrap();

        pikelet
    }

    /// Add a binding to the driver's top-level environment
    pub fn add_binding(&mut self, name: &str, term: core::RcTerm, ann: domain::RcType) {
        let fv = self.desugar_env.on_binding(&name);
        self.context.insert_declaration(fv.clone(), ann.clone());
        self.context.insert_definition(fv.clone(), term.clone());
    }

    /// Register a file with the driver
    pub fn register_file(
        &mut self,
        path: String,
        name: FileName,
        src: String,
    ) -> Result<(), Vec<Diagnostic>> {
        let (term, ty) = self.infer_file(name, src)?;
        // FIXME: Check if import already exists
        self.context.insert_import(path, Import::Term(term), ty);

        Ok(())
    }

    /// Infer the type of a file
    pub fn infer_file(
        &mut self,
        name: FileName,
        src: String,
    ) -> Result<(core::RcTerm, domain::RcType), Vec<Diagnostic>> {
        let file_map = self.code_map.add_filemap(name, src);
        // TODO: follow import paths
        let (concrete_term, _import_paths, errors) = pikelet_concrete::parse::term(&file_map);
        if !errors.is_empty() {
            return Err(errors.iter().map(|error| error.to_diagnostic()).collect());
        }
        let raw_term = self.desugar(&concrete_term)?;
        self.infer_term(&raw_term)
    }

    /// Normalize the contents of a file
    pub fn normalize_file(
        &mut self,
        name: FileName,
        src: String,
    ) -> Result<domain::RcValue, Vec<Diagnostic>> {
        use pikelet_concrete::elaborate::InternalError;

        let (term, _) = self.infer_file(name, src)?;
        pikelet_core::nbe::nf_term(&self.context, &term)
            .map_err(|err| vec![InternalError::from(err).to_diagnostic()])
    }

    /// Infer the type of a term
    pub fn infer_term(
        &self,
        raw_term: &raw::RcTerm,
    ) -> Result<(core::RcTerm, domain::RcType), Vec<Diagnostic>> {
        pikelet_concrete::elaborate::infer_term(&self.context, &raw_term)
            .map_err(|err| vec![err.to_diagnostic()])
    }

    /// Normalize a term
    pub fn normalize_term(&self, term: &core::RcTerm) -> Result<domain::RcValue, Vec<Diagnostic>> {
        use pikelet_concrete::elaborate::InternalError;

        pikelet_core::nbe::nf_term(&self.context, term)
            .map_err(|err| vec![InternalError::from(err).to_diagnostic()])
    }

    /// Desugar a term
    pub fn desugar<T>(&self, src: &impl Desugar<T>) -> Result<T, Vec<Diagnostic>> {
        src.desugar(&self.desugar_env)
            .map_err(|e| vec![e.to_diagnostic()])
    }

    /// Resugar a term
    pub fn resugar<T>(&self, src: &impl Resugar<T>) -> T {
        self.context.resugar(src)
    }

    /// Emit the diagnostics using the given writer
    pub fn emit<'a>(
        &self,
        mut writer: impl termcolor::WriteColor,
        diagnostics: impl IntoIterator<Item = &'a Diagnostic>,
    ) -> io::Result<()> {
        for diagnostic in diagnostics {
            codespan_reporting::emit(&mut writer, &self.code_map, diagnostic)?;
        }
        Ok(())
    }
}
