use std::path::PathBuf;

#[derive(Debug, StructOpt)]
pub struct CheckOpts {
    /// Files to check
    #[structopt(name = "FILE", parse(from_os_str))]
    pub files: Vec<PathBuf>,
}

pub fn run(_check_opts: CheckOpts) {
    unimplemented!()
}
