//! The REPL (Read-Eval-Print-Loop)

use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting;
use failure::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::path::PathBuf;
use term_size;

use semantics;
use syntax::core::Context;
use syntax::parse;

/// Options for the `repl` subcommand
#[derive(Debug, StructOpt)]
pub struct Opts {
    /// The prompt to display before expressions
    #[structopt(long = "prompt", default_value = "Pikelet> ")]
    pub prompt: String,

    /// Don't print the welcome banner on startup
    #[structopt(long = "suppress-welcome-banner")]
    pub suppress_welcome_banner: bool,

    /// The history file to record previous commands to (blank to disable)
    #[structopt(long = "history-file", parse(from_os_str), default_value = "repl-history")]
    pub history_file: Option<PathBuf>,

    /// Files to preload into the REPL
    #[structopt(name = "FILE", parse(from_os_str))]
    pub files: Vec<PathBuf>,
}

fn print_welcome_banner() {
    const WELCOME_BANNER: &[&str] = &[
        r"    ____  _ __        __     __     ",
        r"   / __ \(_) /_____  / /__  / /_    ",
        r"  / /_/ / / //_/ _ \/ / _ \/ __/    ",
        r" / ____/ / ,< /  __/ /  __/ /_      ",
        r"/_/   /_/_/|_|\___/_/\___/\__/      ",
        r"",
    ];

    for (i, line) in WELCOME_BANNER.iter().enumerate() {
        match i {
            2 => println!("{}Version {}", line, env!("CARGO_PKG_VERSION")),
            3 => println!("{}{}", line, env!("CARGO_PKG_HOMEPAGE")),
            4 => println!("{}:? for help", line),
            _ => println!("{}", line),
        }
    }
}

fn print_help_text() {
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

    for line in HELP_TEXT {
        println!("{}", line);
    }
}

/// Run the `repl` subcommand with the given options
pub fn run(opts: Opts) -> Result<(), Error> {
    // TODO: Load files

    let mut rl = Editor::<()>::new();
    let mut codemap = CodeMap::new();
    let context = Context::default();

    if let Some(ref history_file) = opts.history_file {
        if let Err(_) = rl.load_history(&history_file) {
            // No previous REPL history!
        }
    }

    if !opts.suppress_welcome_banner {
        print_welcome_banner();
    }

    // TODO: Load files

    loop {
        match rl.readline(&opts.prompt) {
            Ok(line) => {
                if let Some(_) = opts.history_file {
                    rl.add_history_entry(&line);
                }

                let filename = FileName::virtual_("repl");
                match eval_print(&context, &codemap.add_filemap(filename, line)) {
                    Ok(ControlFlow::Continue) => {},
                    Ok(ControlFlow::Break) => break,
                    Err(EvalPrintError::Parse(errs)) => for err in errs {
                        codespan_reporting::emit(&codemap, &err.to_diagnostic());
                    },
                    Err(EvalPrintError::Type(err)) => {
                        codespan_reporting::emit(&codemap, &err.to_diagnostic());
                    },
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

fn eval_print(context: &Context, filemap: &FileMap) -> Result<ControlFlow, EvalPrintError> {
    use std::rc::Rc;
    use std::usize;

    use syntax::concrete::ReplCommand;
    use syntax::core::{SourceMeta, Term};
    use syntax::translation::ToCore;

    fn term_width() -> usize {
        term_size::dimensions()
            .map(|(width, _)| width)
            .unwrap_or(usize::MAX)
    }

    let (repl_command, parse_errors) = parse::repl_command(filemap);
    if !parse_errors.is_empty() {
        return Err(EvalPrintError::Parse(parse_errors));
    }

    match repl_command {
        ReplCommand::Help => print_help_text(),

        ReplCommand::Eval(parse_term) => {
            let raw_term = Rc::new(parse_term.to_core());
            let (term, inferred) = semantics::infer(context, &raw_term)?;
            let evaluated = semantics::normalize(context, &term)?;

            println!(
                "{term:width$}",
                term = Term::Ann(
                    SourceMeta::default(),
                    Rc::new(Term::from(&*evaluated)),
                    Rc::new(Term::from(&*inferred)),
                ),
                width = term_width(),
            );
        },
        ReplCommand::TypeOf(parse_term) => {
            let raw_term = Rc::new(parse_term.to_core());
            let (_, inferred) = semantics::infer(context, &raw_term)?;

            println!("{term:width$}", term = inferred, width = term_width());
        },

        ReplCommand::NoOp | ReplCommand::Error(_) => {},
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
    Parse(Vec<parse::ParseError>),
    Type(semantics::TypeError),
}

impl From<parse::ParseError> for EvalPrintError {
    fn from(src: parse::ParseError) -> EvalPrintError {
        EvalPrintError::Parse(vec![src])
    }
}

impl From<Vec<parse::ParseError>> for EvalPrintError {
    fn from(src: Vec<parse::ParseError>) -> EvalPrintError {
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
