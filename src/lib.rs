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
extern crate rustyline;
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
    use syntax::translation::Desugar;

    let (module, errors) = syntax::parse::module(&file);
    let mut diagnostics = errors
        .iter()
        .map(|err| err.to_diagnostic())
        .collect::<Vec<_>>();

    semantics::check_module(&module.desugar()).map_err(|err| {
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
