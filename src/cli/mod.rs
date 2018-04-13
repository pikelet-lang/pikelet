//! The command line interface for Pikelet

use codespan_reporting::termcolor::ColorChoice;
use failure::Error;
use std::str::FromStr;

pub mod check;
pub mod repl;

// TODO: test using https://github.com/killercup/assert_cli

#[derive(Debug, StructOpt)]
#[structopt(name = "pikelet")]
pub struct Opts {
    /// Configure coloring of output
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        raw(possible_values = "&[\"auto\", \"always\", \"ansi\", \"never\"]")
    )]
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
    AlwaysAnsi,
    Never,
}

impl Into<ColorChoice> for ColorArg {
    fn into(self) -> ColorChoice {
        match self {
            ColorArg::Auto => ColorChoice::Auto,
            ColorArg::Always => ColorChoice::Always,
            ColorArg::AlwaysAnsi => ColorChoice::AlwaysAnsi,
            ColorArg::Never => ColorChoice::Never,
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
    let color_choice = opts.color.into();
    match opts.command {
        Command::Check(check_opts) => check::run(color_choice, check_opts),
        Command::Repl(repl_opts) => repl::run(color_choice, repl_opts),
    }
}
