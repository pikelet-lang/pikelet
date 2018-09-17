//! The REPL (Read-Eval-Print-Loop)

use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use failure::Error;
use linefeed::{Interface, ReadResult, Signal};
use std::path::PathBuf;
use term_size;

use semantics::{self, DeclarationEnv, DefinitionEnv, GlobalEnv, TcEnv};
use syntax::parse;
use syntax::translation::DesugarEnv;

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
    #[structopt(
        long = "history-file",
        parse(from_os_str),
        default_value = "repl-history"
    )]
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
        ":raw          <term>           print the raw representation of a term",
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
    let interface = Interface::new("repl")?;
    let mut codemap = CodeMap::new();
    let writer = StandardStream::stderr(color);
    let mut tc_env = TcEnv::default();
    let mut desugar_env = DesugarEnv::new(tc_env.mappings());

    interface.set_prompt(&opts.prompt)?;
    interface.set_report_signal(Signal::Interrupt, true);
    interface.set_report_signal(Signal::Quit, true);

    if !opts.no_history && interface.load_history(&opts.history_file).is_err() {
        // No previous REPL history!
    }

    if !opts.no_banner {
        print_welcome_banner();
    }

    // TODO: Load files

    loop {
        match interface.read_line()? {
            ReadResult::Input(line) => {
                if !opts.no_history && !line.trim().is_empty() {
                    interface.add_history_unique(line.clone());
                }

                let filename = FileName::virtual_("repl");
                match eval_print(
                    &mut desugar_env,
                    &mut tc_env,
                    &codemap.add_filemap(filename, line),
                ) {
                    Ok(ControlFlow::Continue) => {},
                    Ok(ControlFlow::Break) => break,
                    Err(EvalPrintError::Parse(errs)) => {
                        for err in errs {
                            let diagnostic = err.to_diagnostic();
                            codespan_reporting::emit(&mut writer.lock(), &codemap, &diagnostic)?;
                        }
                    },
                    Err(EvalPrintError::Type(err)) => {
                        let diagnostic = err.to_diagnostic();
                        codespan_reporting::emit(&mut writer.lock(), &codemap, &diagnostic)?;
                    },
                }
            },
            ReadResult::Signal(Signal::Quit) | ReadResult::Eof => break,
            ReadResult::Signal(Signal::Interrupt) => println!("Interrupt"),
            ReadResult::Signal(_) => {},
        }
    }

    if !opts.no_history {
        interface.save_history(&opts.history_file)?;
    }

    println!("Bye bye");

    Ok(())
}

fn eval_print(
    desugar_env: &mut DesugarEnv,
    tc_env: &mut TcEnv,
    filemap: &FileMap,
) -> Result<ControlFlow, EvalPrintError> {
    use codespan::ByteSpan;

    use syntax::concrete::{ReplCommand, Term};
    use syntax::pretty::{self, ToDoc};
    use syntax::translation::{Desugar, Resugar};

    fn term_width() -> usize {
        term_size::dimensions()
            .map(|(width, _)| width)
            .unwrap_or(pretty::FALLBACK_WIDTH)
    }

    let (repl_command, _import_paths, parse_errors) = parse::repl_command(filemap);
    if !parse_errors.is_empty() {
        return Err(EvalPrintError::Parse(parse_errors));
    }

    match repl_command {
        ReplCommand::Help => print_help_text(),

        ReplCommand::Eval(parse_term) => {
            let raw_term = parse_term.desugar(desugar_env);
            let (term, inferred) = semantics::infer_term(tc_env, &raw_term)?;
            let evaluated = semantics::nf_term(tc_env, &term)?;

            let ann_term = Term::Ann(
                Box::new(evaluated.resugar(tc_env.resugar_env())),
                Box::new(inferred.resugar(tc_env.resugar_env())),
            );

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Core(parse_term) => {
            use syntax::core::{RcTerm, Term};

            let raw_term = parse_term.desugar(desugar_env);
            let (term, inferred) = semantics::infer_term(tc_env, &raw_term)?;

            let ann_term = Term::Ann(term, RcTerm::from(Term::from(&*inferred)));

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Raw(parse_term) => {
            let raw_term = parse_term.desugar(desugar_env);

            println!("{}", raw_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Let(name, parse_term) => {
            let raw_term = parse_term.desugar(desugar_env);
            let (term, inferred) = semantics::infer_term(tc_env, &raw_term)?;

            let ann_term = Term::Ann(
                Box::new(Term::Name(ByteSpan::default(), name.clone(), None)),
                Box::new(inferred.resugar(tc_env.resugar_env())),
            );

            println!("{}", ann_term.to_doc().group().pretty(term_width()));

            let free_var = desugar_env.on_binding(&name);
            tc_env.insert_declaration(free_var.clone(), inferred);
            tc_env.insert_definition(free_var.clone(), term);

            return Ok(ControlFlow::Continue);
        },
        ReplCommand::TypeOf(parse_term) => {
            let raw_term = parse_term.desugar(desugar_env);
            let (_, inferred) = semantics::infer_term(tc_env, &raw_term)?;

            let inferred = inferred.resugar(tc_env.resugar_env());

            println!("{}", inferred.to_doc().group().pretty(term_width()));
        },

        ReplCommand::NoOp | ReplCommand::Error(_) => {},
        ReplCommand::Quit => return Ok(ControlFlow::Break),
    }

    Ok(ControlFlow::Continue)
}

#[derive(Clone)]
enum ControlFlow {
    Break,
    Continue,
}

#[derive(Debug, Fail)]
enum EvalPrintError {
    #[fail(display = "Parse error")]
    Parse(Vec<parse::ParseError>),
    #[fail(display = "Type error: {}", _0)]
    Type(#[cause] semantics::TypeError),
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
