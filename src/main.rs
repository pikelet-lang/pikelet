extern crate lambdapi;
extern crate structopt;

use lambdapi::cli::Opts;
use structopt::StructOpt;

fn main() {
    lambdapi::cli::run(Opts::from_args());
}
