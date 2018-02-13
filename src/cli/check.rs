use failure::Error;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
pub struct Opts {
    /// Files to check
    #[structopt(name = "FILE", parse(from_os_str))]
    pub files: Vec<PathBuf>,
}

pub fn run(_check_opts: Opts) -> Result<(), Error> {
    unimplemented!()
}
