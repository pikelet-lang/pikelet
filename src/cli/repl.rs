//! The REPL (Read-Eval-Print-Loop)

use failure::Error;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::PathBuf;
use term_size;

use semantics;
use syntax::parse;

#[derive(Debug, StructOpt)]
pub struct Opts {
    /// The prompt to display before expressions
    #[structopt(long = "prompt", default_value = "λΠ> ")]
    pub prompt: String,

    /// The history file to record previous commands to (blank to disable)
    #[structopt(long = "history-file", parse(from_os_str), default_value = "repl-history")]
    pub history_file: Option<PathBuf>,

    /// Files to preload into the REPL
    #[structopt(name = "FILE", parse(from_os_str))]
    pub files: Vec<PathBuf>,
}

const HELP_TEXT: &[&str] = &[
    "",
    "Command       Arguments   Purpose",
    "",
    "<expr>                    evaluate a term",
    ":? :h :help               display this help text",
    ":q :quit                  quit the repl",
    ":t :type      <expr>      infer the type of an expression",
    "",
];

pub fn run(opts: Opts) -> Result<(), Error> {
    // TODO: Load files

    let mut rl = Editor::<()>::new();

    if let Some(ref history_file) = opts.history_file {
        rl.load_history(&history_file)?;
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

                match eval_print(&line) {
                    Ok(ControlFlow::Continue) => {},
                    Ok(ControlFlow::Break) => break,
                    Err(EvalPrintError::Parse(err)) => println!("parse error: {}", err),
                    Err(EvalPrintError::Type(err)) => println!("type error: {:?}", err),
                }
            },
            Err(err) => match err {
                ReadlineError::Interrupted => println!("Interrupt"),
                ReadlineError::Eof => break,
                err => {
                    println!("readline error: {:?}", err);
                    break;
                },
            },
        }
    }

    if let Some(ref history_file) = opts.history_file {
        rl.save_history(history_file)?;
    }

    println!("Bye bye");

    Ok(())
}

fn eval_print(line: &str) -> Result<ControlFlow, EvalPrintError> {
    use syntax::concrete::ReplCommand;
    use syntax::core::{Context, RcTerm};
    use syntax::pretty::{self, ToDoc};
    use syntax::translation::FromConcrete;

    const FALLBACK_WIDTH: usize = 80;

    fn term_width() -> usize {
        term_size::dimensions().map_or(FALLBACK_WIDTH, |(width, _)| width)
    }

    match line.parse()? {
        ReplCommand::Help => for line in HELP_TEXT {
            println!("{}", line);
        },

        ReplCommand::Eval(parse_term) => {
            let term = RcTerm::from_concrete(&parse_term);
            let context = Context::new();
            let (_, inferred) = semantics::infer(&context, &term)?;
            let evaluated = semantics::normalize(&context, &term)?;
            let doc = pretty::pretty_ann(pretty::Options::default(), &evaluated, &inferred);

            println!("{}", doc.pretty(term_width()));
        },
        ReplCommand::TypeOf(parse_term) => {
            let term = RcTerm::from_concrete(&parse_term);
            let context = Context::new();
            let (_, inferred) = semantics::infer(&context, &term)?;
            let doc = inferred.to_doc(pretty::Options::default());

            println!("{}", doc.pretty(term_width()));
        },

        ReplCommand::NoOp => {},
        ReplCommand::Quit => return Ok(ControlFlow::Break),
    }

    Ok(ControlFlow::Continue)
}

#[derive(Copy, Clone)]
enum ControlFlow {
    Break,
    Continue,
}

enum EvalPrintError {
    Parse(parse::ParseError),
    Type(semantics::TypeError),
}

impl From<parse::ParseError> for EvalPrintError {
    fn from(src: parse::ParseError) -> EvalPrintError {
        EvalPrintError::Parse(src)
    }
}

impl From<semantics::TypeError> for EvalPrintError {
    fn from(src: semantics::TypeError) -> EvalPrintError {
        EvalPrintError::Type(src)
    }
}

impl From<semantics::InternalError> for EvalPrintError {
    fn from(src: semantics::InternalError) -> EvalPrintError {
        EvalPrintError::Type(src.into())
    }
}
