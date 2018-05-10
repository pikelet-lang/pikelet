extern crate failure;
extern crate pikelet;
extern crate structopt;

use failure::Error;
use pikelet::cli::Opts;
use structopt::StructOpt;

fn main() -> Result<(), Error> {
    pikelet::cli::run(Opts::from_args())
}
