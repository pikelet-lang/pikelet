//! The REPL (Read-Eval-Print-Loop)

extern crate codespan;
extern crate combine;
#[macro_use]
extern crate failure;
extern crate linefeed;
extern crate pikelet_concrete;
extern crate pikelet_core;
extern crate pikelet_driver;
#[macro_use]
extern crate structopt;
extern crate term_size;

use failure::Error;
use linefeed::{Interface, ReadResult, Signal};
use std::path::PathBuf;
use std::str::FromStr;

use pikelet_driver::termcolor::StandardStream;
use pikelet_driver::{ColorArg, Diagnostic, Driver, FileName};

/// Options for the `repl` subcommand
#[derive(Debug, StructOpt)]
pub struct Opts {
    /// Configure coloring of output
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        raw(possible_values = "ColorArg::VARIANTS")
    )]
    pub color: ColorArg,

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
        "<term>                         normalize a term",
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
pub fn run(opts: Opts) -> Result<(), Error> {
    use std::fs::File;
    use std::io::Read;

    let interface = Interface::new("repl")?;
    let writer = StandardStream::stderr(opts.color.into());
    let mut driver = Driver::with_prelude();

    interface.set_prompt(&opts.prompt)?;
    interface.set_report_signal(Signal::Interrupt, true);
    interface.set_report_signal(Signal::Quit, true);

    if !opts.no_history && interface.load_history(&opts.history_file).is_err() {
        // No previous REPL history!
    }

    if !opts.no_banner {
        print_welcome_banner();
    }

    // preload specified files
    for path in &opts.files {
        // FIXME: allow for customization of internal path
        let internal_path = path.to_str().unwrap().to_owned();
        let external_path = FileName::Real(path.clone());

        let mut file = File::open(path)?;
        let mut src = String::new();
        file.read_to_string(&mut src)?;

        if let Err(diagnostics) = driver.register_file(internal_path, external_path, src) {
            driver.emit(writer.lock(), &diagnostics).unwrap();
            return Err(format_err!("encountered an error!"));
        }
    }

    loop {
        match interface.read_line()? {
            ReadResult::Input(line) => {
                if !opts.no_history && !line.trim().is_empty() {
                    interface.add_history_unique(line.clone());
                }

                let repl_command = match line.parse() {
                    Ok(repl_command) => repl_command,
                    Err(diagnostics) => {
                        driver.emit(writer.lock(), &diagnostics).unwrap();
                        continue;
                    },
                };

                match eval_print(&mut driver, repl_command) {
                    Ok(ControlFlow::Continue) => {},
                    Ok(ControlFlow::Break) => break,
                    Err(diagnostics) => driver.emit(writer.lock(), &diagnostics).unwrap(),
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

#[derive(Clone)]
enum ControlFlow {
    Break,
    Continue,
}

/// Commands entered in the REPL
#[derive(Debug, Clone)]
pub enum ReplCommand {
    /// Normalize a term
    ///
    /// ```text
    /// <term>
    /// ```
    Normalize(String),
    /// Show the core representation of a term
    ///
    /// ```text
    /// :core <term>
    /// ```
    Core(String),
    /// Print some help about using the REPL
    ///
    /// ```text
    /// :?
    /// :h
    /// :help
    /// ```
    Help,
    /// Add a declaration to the REPL environment
    ///
    /// ```text
    /// :let <name> = <term>
    /// ```
    Let(String, String),
    ///  No command
    NoOp,
    /// Quit the REPL
    ///
    /// ```text
    /// :q
    /// :quit
    /// ```
    Quit,
    /// Print the type of the term
    ///
    /// ```text
    /// :t <term>
    /// :type <term>
    /// ```
    TypeOf(String),
}

impl FromStr for ReplCommand {
    type Err = Vec<Diagnostic>;

    fn from_str(src: &str) -> Result<ReplCommand, Vec<Diagnostic>> {
        use combine::char::*;
        use combine::*;

        let anys1 = || many1(any());
        let spaces1 = || skip_many1(space());
        let ident = || {
            value(())
                .with(letter())
                .and(many::<String, _>(alpha_num()))
                .map(|(hd, tl)| format!("{}{}", hd, tl))
        };

        let cmd = choice((
            token(':').with(choice((
                attempt(
                    choice((
                        attempt(string("help")),
                        attempt(string("?")),
                        attempt(string("h")),
                    ))
                    .map(|_| ReplCommand::Help),
                ),
                attempt(
                    choice((attempt(string("quit")), attempt(string("q"))))
                        .map(|_| ReplCommand::Quit),
                ),
                attempt(
                    string("core")
                        .with(spaces1())
                        .with(anys1())
                        .map(ReplCommand::Core),
                ),
                attempt(
                    choice((attempt(string("type")), string("t")))
                        .with(spaces1())
                        .with(anys1())
                        .map(ReplCommand::TypeOf),
                ),
                attempt(
                    string("let")
                        .with(spaces1())
                        .with(ident())
                        .skip(spaces())
                        .skip(string("="))
                        .skip(spaces())
                        .and(anys1())
                        .map(|(ident, src)| ReplCommand::Let(ident, src)),
                ),
            ))),
            anys1().map(ReplCommand::Normalize),
        ));

        let mut parser = spaces().with(cmd).skip(spaces());

        match parser.parse(src) {
            Ok((cmd, _)) => Ok(cmd),
            Err(_) => {
                // TODO: better errors here!
                Err(vec![Diagnostic::new_error("malformed REPL command")])
            },
        }
    }
}

fn eval_print(
    driver: &mut Driver,
    repl_command: ReplCommand,
) -> Result<ControlFlow, Vec<Diagnostic>> {
    use codespan::ByteSpan;

    use pikelet_concrete::syntax::concrete::Term;

    fn term_width() -> usize {
        term_size::dimensions()
            .map(|(width, _)| width)
            .unwrap_or(1_000_000)
    }

    let file_name = FileName::virtual_("repl");

    match repl_command {
        ReplCommand::Help => print_help_text(),

        ReplCommand::Normalize(term_src) => {
            let (term, inferred) = driver.infer_file(file_name, term_src)?;
            let evaluated = driver.normalize_term(&term)?;

            let ann_term = Term::Ann(
                Box::new(driver.resugar(&evaluated)),
                Box::new(driver.resugar(&inferred)),
            );

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Core(term_src) => {
            use pikelet_core::syntax::core::{RcTerm, Term};

            let (term, inferred) = driver.infer_file(file_name, term_src)?;
            let ann_term = Term::Ann(term, RcTerm::from(Term::from(&*inferred)));

            println!("{}", ann_term.to_doc().group().pretty(term_width()));
        },
        ReplCommand::Let(name, term_src) => {
            let (term, inferred) = driver.infer_file(file_name, term_src)?;
            driver.add_binding(&name, term.clone(), inferred.clone());

            let ann_term = Term::Ann(
                Box::new(Term::Name(ByteSpan::default(), name, None)),
                Box::new(driver.resugar(&inferred)),
            );

            println!("{}", ann_term.to_doc().group().pretty(term_width()));

            return Ok(ControlFlow::Continue);
        },
        ReplCommand::TypeOf(term_src) => {
            let (_, inferred) = driver.infer_file(file_name, term_src)?;
            let inferred = driver.resugar(&inferred);

            println!("{}", inferred.to_doc().group().pretty(term_width()));
        },

        ReplCommand::NoOp => {},
        ReplCommand::Quit => return Ok(ControlFlow::Break),
    }

    Ok(ControlFlow::Continue)
}
