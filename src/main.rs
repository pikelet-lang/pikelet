extern crate pikelet;
extern crate structopt;

use pikelet::cli::Opts;
use std::process;
use structopt::StructOpt;

const EXIT_OK: i32 = 0;

#[cfg(os = "windows")]
const EXIT_ERR: i32 = 0x0100;
#[cfg(not(os = "windows"))]
const EXIT_ERR: i32 = 1;

fn main() {
    process::exit(match pikelet::cli::run(Opts::from_args()) {
        Ok(()) => EXIT_OK,
        Err(err) => {
            eprintln!("{}", err);
            EXIT_ERR
        },
    })
}
