//! The REPL (Read-Eval-Print-Loop)

use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use failure::Error;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::PathBuf;
use term_size;

use semantics;
use syntax::context::Context;
use syntax::parse;

/// Options for the `repl` subcommand
#[derive(Debug, StructOpt)]
pub struct Opts {
    /// The prompt to display before expressions
    #[structopt(long = "prompt", default_value = "Pikelet> ")]
    pub prompt: String,

    /// Disable the welcome banner on startup
    #[structopt(long = "no-banner")]
    pub no_banner: bool,

    /// Disable saving of command history on exit
    #[structopt(long = "no-history")]
    pub no_history: bool,

    /// The file to save the command history to
    #[structopt(long = "history-file", parse(from_os_str), default_value = "repl-history")]
    pub history_file: PathBuf,

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
        // warning on `env!` is a known issue
        #[cfg_attr(feature = "cargo-clippy", allow(print_literal))]
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
        "Command       Arguments        Purpose",
        "",
        "<term>                         evaluate a term",
        ":? :h :help                    display this help text",
        ":core         <term>           print the core representation of a term",
        ":let          <name> = <term>  add a named term to the REPL context",
        ":q :quit                       quit the repl",
        ":t :type      <term>           infer the type of a term",
        "",
    ];

    for line in HELP_TEXT {
        println!("{}", line);
    }
}

/// Run the `repl` subcommand with the given options
pub fn run(color: ColorChoice, opts: &Opts) -> Result<(), Error> {
    // TODO: Load files

    let mut rl = Editor::<()>::new();
    let mut codemap = CodeMap::new();
    let writer = StandardStream::stderr(color);
    let mut context = Context::default();

    if !opts.no_history && rl.load_history(&opts.history_file).is_err() {
        // No previous REPL history!
    }

    if !opts.no_banner {
        print_welcome_banner();
    }

    // TODO: Load files

    loop {
        match rl.readline(&opts.prompt) {
            Ok(line) => {
                if !opts.no_history {
                    rl.add_history_entry(&line);
                }

                let filename = FileName::virtual_("repl");
                match eval_print(&context, &codemap.add_filemap(filename, line)) {
                    Ok(ControlFlow::Continue(None)) => {},
                    Ok(ControlFlow::Continue(Some(new_context))) => context = new_context,
                    Ok(ControlFlow::Break) => break,
                    Err(EvalPrintError::Parse(errs)) => for err in errs {
                        codespan_reporting::emit(
                            &mut writer.lock(),
                            &codemap,
                            &err.to_diagnostic(),
                        )?;
                    },
                    Err(EvalPrintError::Type(err)) => {
                        codespan_reporting::emit(
                            &mut writer.lock(),
                            &codemap,
                            &err.to_diagnostic(),
                        )?;
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

    if !opts.no_history {
        rl.save_history(&opts.history_file)?;
    }

    println!("Bye bye");

    Ok(())
}

fn eval_print(context: &Context, filemap: &FileMap) -> Result<ControlFlow, EvalPrintError> {
    use codespan::ByteIndex;
    use moniker::FreeVar;
    use std::rc::Rc;

    use syntax::concrete::{ReplCommand, Term};
    use syntax::pretty::{self, ToDoc};
    use syntax::translation::{Desugar, Resugar};

    fn term_width() -> usize {
        term_size::dimensions()
            .map(|(width, _)| width)
            .unwrap_or(pretty::FALLBACK_WIDTH)
    }

    let (repl_command, parse_errors) = parse::repl_command(filemap);
    if !parse_errors.is_empty() {
        return Err(EvalPrintError::Parse(parse_errors));
    }

    match repl_command {
        ReplCommand::Help => print_help_text(),

        ReplCommand::Eval(parse_term) => {
            let raw_term = Rc::new(parse_term.desugar());
            let (term, inferred) = semantics::infer(context, &raw_term)?;
            let evaluated = semantics::normalize(context, &term)?;

            let ann_term = Term::Ann(Box::new(evaluated.resugar()), Box::new(inferred.resugar()));

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Core(parse_term) => {
            use syntax::core::Term;

            let raw_term = Rc::new(parse_term.desugar());
            let (term, inferred) = semantics::infer(context, &raw_term)?;

            let ann_term = Term::Ann(term, Rc::new(Term::from(&*inferred)));

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Let(name, parse_term) => {
            let raw_term = Rc::new(parse_term.desugar());
            let (term, inferred) = semantics::infer(context, &raw_term)?;

            let ann_term = Term::Ann(
                Box::new(Term::Var(ByteIndex::default(), name.clone())),
                Box::new(inferred.resugar()),
            );

            println!("{}", ann_term.to_doc().group().pretty(term_width()));

            let context = context.define_term(FreeVar::user(&*name), inferred, term);

            return Ok(ControlFlow::Continue(Some(context)));
        },
        ReplCommand::TypeOf(parse_term) => {
            let raw_term = Rc::new(parse_term.desugar());
            let (_, inferred) = semantics::infer(context, &raw_term)?;

            println!("{}", inferred.resugar().to_doc().pretty(term_width()));
        },

        ReplCommand::NoOp | ReplCommand::Error(_) => {},
        ReplCommand::Quit => return Ok(ControlFlow::Break),
    }

    Ok(ControlFlow::Continue(None))
}

#[derive(Clone)]
enum ControlFlow {
    Break,
    Continue(Option<Context>),
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
