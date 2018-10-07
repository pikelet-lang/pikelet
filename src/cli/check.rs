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

    use Pikelet;

    let mut codemap = CodeMap::new();
    let mut pikelet = Pikelet::with_prelude();
    let writer = StandardStream::stderr(color);

    let mut is_error = false;

    for path in opts.files {
        // FIXME: allow for customization of internal path
        let internal_path = path.to_str().unwrap().to_owned();
        let file_map = codemap.add_filemap_from_disk(path)?;

        if let Err(diagnostics) = pikelet.load_file(internal_path, file_map) {
            for diagnostic in diagnostics {
                codespan_reporting::emit(&mut writer.lock(), &codemap, &diagnostic)?;
            }
            is_error = true;
            continue;
        }
    }

    if is_error {
        Err(format_err!("encountered an error!"))
    } else {
        Ok(())
    }
}
