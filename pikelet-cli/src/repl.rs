use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use pikelet::lang::{core, surface};
use pikelet::pass::{surface_to_core, surface_to_pretty};
use rustyline::error::ReadlineError;
use std::sync::Arc;

const HISTORY_FILE_NAME: &str = "history";

/// The Pikelet REPL/interactive mode.
#[derive(structopt::StructOpt)]
pub struct Options {
    /// The prompt to display before expressions.
    #[structopt(long = "prompt", default_value = "> ")]
    pub prompt: String,
    /// Disable the welcome banner on startup.
    #[structopt(long = "no-banner")]
    pub no_banner: bool,
    /// Disable saving of command history on exit.
    #[structopt(long = "no-history")]
    pub no_history: bool,
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
        #[allow(clippy::print_literal)]
        match i {
            2 => println!("{}Version {}", line, env!("CARGO_PKG_VERSION")),
            3 => println!("{}{}", line, env!("CARGO_PKG_HOMEPAGE")),
            4 => println!("{}:? for help", line),
            _ => println!("{}", line.trim_end()),
        }
    }
}

pub fn run(options: Options) -> anyhow::Result<()> {
    let mut editor = {
        let config = rustyline::Config::builder()
            .history_ignore_space(true)
            .history_ignore_dups(true)
            .build();

        rustyline::Editor::<()>::with_config(config)
    };

    if !options.no_banner {
        print_welcome_banner()
    }

    let xdg_dirs = xdg::BaseDirectories::with_prefix("pikelet/repl")?;
    let history_path = xdg_dirs.get_data_home().join(HISTORY_FILE_NAME);

    if !options.no_history && editor.load_history(&history_path).is_err() {
        // No previous REPL history!
    }

    let pretty_alloc = pretty::BoxAllocator;
    let writer = StandardStream::stderr(ColorChoice::Always);
    let reporting_config = codespan_reporting::term::Config::default();

    let globals = core::Globals::default();
    let (messages_tx, messages_rx) = crossbeam_channel::unbounded();
    let mut state = surface_to_core::State::new(&globals, messages_tx.clone());

    'repl: loop {
        let file = match editor.readline(&options.prompt) {
            Ok(line) => SimpleFile::new("<input>", line),
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted!");
                continue 'repl;
            }
            Err(ReadlineError::Eof) => break 'repl,
            Err(error) => return Err(error.into()),
        };

        if !options.no_history {
            editor.add_history_entry(file.source());
        }

        // TODO: Parse REPL commands
        let surface_term = surface::Term::from_str(file.source(), &messages_tx);
        let (core_term, r#type) = state.synth_type(&surface_term);

        let mut is_ok = true;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            is_ok &= diagnostic.severity < Severity::Error;

            codespan_reporting::term::emit(
                &mut writer.lock(),
                &reporting_config,
                &file,
                &diagnostic,
            )?;
        }

        if is_ok {
            let ann_term = core::Term::from(core::TermData::Ann(
                Arc::new(state.normalize_term(&core_term)),
                Arc::new(state.read_back_value(&r#type)),
            ));
            let term = state.core_to_surface_term(&ann_term);
            let doc = surface_to_pretty::from_term(&pretty_alloc, &term);

            println!("{}", doc.1.pretty(crate::term_width()));
        }
    }

    if !options.no_history && !editor.history().is_empty() {
        let history_path = xdg_dirs.place_data_file(HISTORY_FILE_NAME)?;
        editor.save_history(&history_path)?;
    }

    println!("Bye bye");

    Ok(())
}
