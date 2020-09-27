use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use pikelet::lang::{core, surface};
use pikelet::pass::surface_to_core;
use std::path::PathBuf;

/// Check some Pikelet source files.
#[derive(structopt::StructOpt)]
pub struct Options {
    /// Validate the elaborated core language.
    #[structopt(long = "validate-core")]
    validate_core: bool,
    /// The Pikelet source files to be checked.
    #[structopt(name = "FILE")]
    file_names: Vec<PathBuf>,
}

pub fn run(options: Options) -> anyhow::Result<()> {
    let pretty_alloc = pretty::BoxAllocator;
    let writer = StandardStream::stderr(ColorChoice::Always);
    let reporting_config = codespan_reporting::term::Config::default();

    let globals = core::Globals::default();
    let (messages_tx, messages_rx) = crossbeam_channel::unbounded();
    let mut surface_to_core = surface_to_core::State::new(&globals, messages_tx.clone());
    let mut core_typing = match options.validate_core {
        true => Some(core::typing::State::new(&globals, messages_tx.clone())),
        false => None,
    };

    let mut is_ok = true;

    for file_name in &options.file_names {
        let source = std::fs::read_to_string(file_name)?;
        let file = SimpleFile::new(file_name.display().to_string(), source);

        let surface_term = match surface::Term::from_str(file.source()) {
            Ok(surface_term) => surface_term,
            Err(error) => {
                messages_tx.send(error.into()).unwrap();
                surface::Term::from(surface::TermData::Error)
            }
        };

        let (term, _) = surface_to_core.synth_type(&surface_term);
        if let Some(core_typing) = &mut core_typing {
            let _ = core_typing.synth_type(&term);
        }

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
    }

    match is_ok {
        true => Ok(()),
        false => Err(anyhow::anyhow!("errors found in supplied source files")),
    }
}
