extern crate lambdapi;
extern crate rustyline;
#[macro_use]
extern crate structopt;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::PathBuf;
use structopt::StructOpt;

use lambdapi::semantics;
use lambdapi::syntax::parse;

// TODO: test using https://github.com/killercup/assert_cli

#[derive(Debug, StructOpt)]
struct Opts {
    #[structopt(subcommand)]
    command: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    /// Check the that the given files type check
    #[structopt(name = "check")]
    Check(CheckOpts),

    /// A REPL for running expressions
    #[structopt(name = "repl")]
    Repl(ReplOpts),
}

#[derive(Debug, StructOpt)]
struct CheckOpts {
    /// Files to check
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

#[derive(Debug, StructOpt)]
struct ReplOpts {
    /// The prompt to display before expressions
    #[structopt(long = "prompt", default_value = "λΠ> ")]
    prompt: String,

    /// The history file to record previous commands to (blank to disable)
    #[structopt(long = "history-file", parse(from_os_str), default_value = "repl-history")]
    history_file: Option<PathBuf>,

    /// Files to preload into the REPL
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn main() {
    let opts = Opts::from_args();

    match opts.command {
        Command::Check(check_opts) => run_check(check_opts),
        Command::Repl(repl_opts) => run_repl(repl_opts),
    }
}

fn run_check(_check_opts: CheckOpts) {
    unimplemented!()
}

fn run_repl(opts: ReplOpts) {
    // TODO: Load files

    let mut rl = Editor::<()>::new();

    if let Some(ref history_file) = opts.history_file {
        if let Err(_) = rl.load_history(&history_file) {}
    }

    println!(
        "{}, version {} (:? for help, :q to quit)",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
    );

    // TODO: Load files

    loop {
        match rl.readline(&opts.prompt) {
            Ok(line) => {
                if let Some(_) = opts.history_file {
                    rl.add_history_entry(&line);
                }

                match step_repl(&line) {
                    Ok(()) => {},
                    Err(ReplError::Parse(err)) => println!("parse error: {}", err),
                    Err(ReplError::Type(err)) => println!("type error: {:?}", err),
                    Err(ReplError::Quit) => {
                        println!("Bye bye");
                        break;
                    },
                }
            },
            Err(err) => match err {
                ReadlineError::Interrupted => println!("Interrupt"),
                ReadlineError::Eof => {
                    println!("Bye bye");
                    break;
                },
                err => {
                    println!("Error: {:?}", err);
                    break;
                },
            },
        }
    }

    if let Some(ref history_file) = opts.history_file {
        rl.save_history(history_file).unwrap();
    }
}

fn step_repl(line: &str) -> Result<(), ReplError> {
    use lambdapi::semantics;
    use lambdapi::syntax::concrete::ReplCommand;
    use lambdapi::syntax::core::{Context, RcTerm};
    use lambdapi::syntax::pretty::{self, ToDoc};
    use lambdapi::syntax::translation::FromConcrete;

    match line.parse()? {
        ReplCommand::Help => {
            println!("");
            println!("Command       Arguments   Purpose");
            println!("");
            println!("<expr>                    evaluate a term");
            println!(":? :h :help               display this help text");
            println!(":q :quit                  quit the repl");
            println!(":t :type      <expr>      infer the type of an expression");
            println!("");
        },

        ReplCommand::Eval(parse_term) => {
            let term = RcTerm::from_concrete(&parse_term);
            let context = Context::new();
            let (_, inferred) = semantics::infer(&context, &term)?;
            let evaluated = semantics::normalize(&context, &term)?;
            let doc = pretty::pretty_ann(pretty::Options::default(), &evaluated, &inferred);

            println!("{}", doc.pretty(80));
        },
        ReplCommand::TypeOf(parse_term) => {
            let term = RcTerm::from_concrete(&parse_term);
            let context = Context::new();
            let (_, inferred) = semantics::infer(&context, &term)?;
            let doc = inferred.to_doc(pretty::Options::default());

            println!("{}", doc.pretty(80));
        },

        ReplCommand::NoOp => {},
        ReplCommand::Quit => return Err(ReplError::Quit),
    }

    Ok(())
}

enum ReplError {
    Parse(parse::ParseError),
    Type(semantics::TypeError),
    Quit,
}

impl From<parse::ParseError> for ReplError {
    fn from(src: parse::ParseError) -> ReplError {
        ReplError::Parse(src)
    }
}

impl From<semantics::TypeError> for ReplError {
    fn from(src: semantics::TypeError) -> ReplError {
        ReplError::Type(src)
    }
}

impl From<semantics::InternalError> for ReplError {
    fn from(src: semantics::InternalError) -> ReplError {
        ReplError::Type(src.into())
    }
}
