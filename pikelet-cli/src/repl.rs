use std::error::Error;
use std::path::PathBuf;

/// The Pikelet REPL/interactive mode.
#[derive(structopt::StructOpt)]
pub struct Options {
    /// The file to save the command history to.
    #[structopt(long = "history-file", default_value = "repl-history")]
    pub history_file: PathBuf,
    /// The prompt to display before expressions.
    #[structopt(long = "prompt", default_value = "> ")]
    pub prompt: String,
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

    if editor.load_history(&options.history_file).is_err() {
        // No previous REPL history!
    }

    let globals = core::Globals::default();
    let mut state = surface::projections::core::State::new(&globals);

    loop {
        match editor.readline(&options.prompt) {
            Ok(line) => {
                editor.add_history_entry(&line);

                let surface_term = match surface::Term::from_str(&line) {
                    Ok(surface_term) => surface_term,
                    Err(error) => {
                        println!("error: {}", error);
                        continue;
                    }
                };
                let (core_term, _type) =
                    surface::projections::core::synth_term(&mut state, &surface_term);

                if !state.errors.is_empty() {
                    for error in &state.errors {
                        println!("error: {:?}", error);
                    }
                    println!();
                    state.errors.clear();
                } else {
                    println!("{:?}", core_term);

                    // TODO: normalize term
                    // TODO: print `{term} : {type}`
                }
            }
            Err(ReadlineError::Interrupted) => println!("Interrupted!"),
            Err(ReadlineError::Eof) => break,
            Err(error) => return Err(error.into()),
        }
    }

    editor.save_history(&options.history_file)?;

    println!("Bye bye");

    Ok(())
}
