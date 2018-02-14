use failure::Error;
use std::str::FromStr;

pub mod check;
pub mod repl;

// TODO: test using https://github.com/killercup/assert_cli

#[derive(Debug, StructOpt)]
#[structopt(name = "lambdapi")]
pub struct Opts {
    /// Configure coloring of output
    #[structopt(long = "color", parse(try_from_str), default_value = "auto",
                raw(possible_values = "&[\"auto\", \"always\", \"never\"]"))]
    pub color: ColorArg,

    /// Subcommand to run
    #[structopt(subcommand)]
    pub command: Command,
}

// TODO: actually use this!
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ColorArg {
    Auto,
    Always,
    Never,
}

impl ColorArg {
    /// Returns a boolean that specifies if the output should be colorized,
    /// checking if we are in a terminal session if `Auto` is passed.
    pub fn should_colorize(self) -> bool {
        use isatty;

        match self {
            ColorArg::Auto => isatty::stdin_isatty(),
            ColorArg::Always => true,
            ColorArg::Never => false,
        }
    }
}

impl FromStr for ColorArg {
    type Err = &'static str;

    fn from_str(src: &str) -> Result<ColorArg, &'static str> {
        match src {
            "auto" => Ok(ColorArg::Auto),
            "always" => Ok(ColorArg::Always),
            "never" => Ok(ColorArg::Never),
            _ => Err("no match"),
        }
    }
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
