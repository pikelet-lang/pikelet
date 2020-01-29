use std::error::Error;
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

fn term_width() -> usize {
    match term_size::dimensions() {
        Some((width, _)) => width,
        None => std::usize::MAX,
    }
}

/// Run the REPL with the given options.
pub fn run(options: Options) -> Result<(), Box<dyn Error>> {
    use pikelet::{core, surface};
    use rustyline::error::ReadlineError;
    use rustyline::{Config, Editor};

    let mut editor = {
        let config = Config::builder()
            .history_ignore_space(true)
            .history_ignore_dups(true)
            .build();

        Editor::<()>::with_config(config)
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
    let globals = core::Globals::default();
    let mut state = surface::projections::core::State::new(&globals);

    loop {
        match editor.readline(&options.prompt) {
            Ok(line) => {
                if !options.no_history {
                    editor.add_history_entry(&line);
                }

                // TODO: Parse REPL commands

                let surface_term = match surface::Term::from_str(&line) {
                    Ok(surface_term) => surface_term,
                    Err(error) => {
                        println!("error: {}", error);
                        continue;
                    }
                };

                let (core_term, r#type) =
                    surface::projections::core::synth_term(&mut state, &surface_term);
                let errors = state.drain_errors().collect::<Vec<_>>();

                if !errors.is_empty() {
                    for error in &errors {
                        println!("error: {:?}", error);
                    }
                } else {
                    let ann_term = core::Term::Ann(
                        Arc::new(state.normalize_term(&core_term, &r#type)),
                        Arc::new(state.read_back_type(&r#type)),
                    );
                    let term = state.delaborate_term(&ann_term);
                    let doc = surface::projections::pretty::pretty_term(&pretty_alloc, &term);

                    println!("{}", doc.1.pretty(term_width()));
                }
            }
            Err(ReadlineError::Interrupted) => println!("Interrupted!"),
            Err(ReadlineError::Eof) => break,
            Err(error) => return Err(error.into()),
        }
    }

    if !options.no_history && !editor.history().is_empty() {
        let history_path = xdg_dirs.place_data_file(HISTORY_FILE_NAME)?;
        editor.save_history(&history_path)?;
    }

    println!("Bye bye");

    Ok(())
}
