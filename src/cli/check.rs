use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use failure::Error;
use std::path::PathBuf;

/// Options for the `check` subcommand
#[derive(Debug, StructOpt)]
pub struct Opts {
    /// Files to check
    #[structopt(name = "FILE", parse(from_os_str))]
    pub files: Vec<PathBuf>,
}

/// Run the `check` subcommand with the given options
pub fn run(color: ColorChoice, opts: Opts) -> Result<(), Error> {
    use codespan::CodeMap;
    use codespan_reporting;

    use semantics;
    use syntax::parse;
    use syntax::translation::ToCore;

    let mut codemap = CodeMap::new();
    let writer = StandardStream::stderr(color);

    let mut is_error = false;
    for path in opts.files {
        let file = codemap.add_filemap_from_disk(path)?;
        let (module, parse_errors) = parse::module(&file);

        let mut is_parse_error = false;
        for error in parse_errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic())?;
            is_error = true;
            is_parse_error = true;
        }
        if is_parse_error {
            continue;
        }

        match semantics::check_module(&module.to_core()) {
            Ok(_module) => {},
            Err(err) => {
                codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic())?;
                is_error = true;
            },
        }
    }
    if is_error {
        Err(format_err!("encountered an error!"))
    } else {
        Ok(())
    }
}
