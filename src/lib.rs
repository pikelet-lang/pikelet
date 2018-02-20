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
extern crate source_reporting;
extern crate unicode_xid;

mod library;
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

use source::{CodeMap, FileMap, FileName};
use source_reporting::Diagnostic;

use semantics::CheckedModule;

pub fn load_file(file: &FileMap) -> Result<CheckedModule, Vec<Diagnostic>> {
    use syntax::core::Module;
    use syntax::translation::FromConcrete;

    let mut diagnostics = Vec::new();

    let (module, errors) = syntax::parse::module(&file);
    diagnostics.extend(errors.iter().map(|err| err.to_diagnostic()));

    match module {
        None => Err(diagnostics),
        Some(module) => {
            let module = Module::from_concrete(&module);
            match semantics::check_module(&module) {
                Ok(module) => Ok(module),
                Err(err) => {
                    diagnostics.push(err.to_diagnostic());
                    Err(diagnostics)
                },
            }
        },
    }
}

pub fn load_prelude(codemap: &mut CodeMap) -> CheckedModule {
    let file = codemap.add_filemap(
        FileName::real("library/prelude.pi"),
        String::from(library::PRELUDE),
    );

    match load_file(&file) {
        Ok(module) => module,
        Err(diagnostics) => {
            for diagnostic in diagnostics {
                source_reporting::emit(codemap, &diagnostic);
            }
            panic!("unexpected parse errors in prelude");
        },
    }
}
