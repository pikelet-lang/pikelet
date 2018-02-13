use failure::Error;

pub mod check;
pub mod repl;

// TODO: test using https://github.com/killercup/assert_cli

#[derive(Debug, StructOpt)]
#[structopt(name = "lambdapi")]
pub struct Opts {
    #[structopt(subcommand)]
    pub command: Command,
}

#[derive(Debug, StructOpt)]
pub enum Command {
    /// Check the that the given files type check
    #[structopt(name = "check")]
    Check(check::Opts),

    /// A REPL for running expressions
    #[structopt(name = "repl")]
    Repl(repl::Opts),
}

pub fn run(opts: Opts) -> Result<(), Error> {
    match opts.command {
        Command::Check(check_opts) => check::run(check_opts),
        Command::Repl(repl_opts) => repl::run(repl_opts),
    }
}
