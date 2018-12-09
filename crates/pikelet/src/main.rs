use failure::Error;
use pikelet::Opts;
use structopt::StructOpt;

fn main() -> Result<(), Error> {
    pikelet::run(Opts::from_args())
}
