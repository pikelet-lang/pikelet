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

    use semantics::{self, TcEnv};
    use syntax::parse;
    use syntax::translation::{Desugar, DesugarEnv};

    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());
    let writer = StandardStream::stderr(color);

    let mut is_error = false;
    for path in opts.files {
        let file = codemap.add_filemap_from_disk(path)?;
        let (concrete_term, _import_paths, parse_errors) = parse::term(&file);

        let mut is_parse_error = false;
        for error in parse_errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic())?;
            is_error = true;
            is_parse_error = true;
        }
        if is_parse_error {
            continue;
        }

        let raw_term = match concrete_term.desugar(&desugar_env) {
            Ok(raw_term) => raw_term,
            Err(err) => {
                codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic())?;
                is_error = true;
                continue;
            },
        };

        match semantics::infer_term(&tc_env, &raw_term) {
            Ok(_) => {},
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
