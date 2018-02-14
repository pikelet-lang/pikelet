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
pub fn run(_check_opts: Opts) -> Result<(), Error> {
    unimplemented!()
}
