use pikelet_cli::Options;
use std::error::Error;
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    pikelet_cli::run(Options::from_args())
}
