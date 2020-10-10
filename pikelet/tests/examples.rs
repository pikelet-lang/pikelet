//! Integration tests against the language samples directory.

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
use pikelet::lang::{core, surface};
use pikelet::pass::surface_to_core;
use std::io::Write;

fn run_test(path: &str, source: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut is_failed = false;

    let mut writer = BufferedStandardStream::stdout(ColorChoice::Always);
    let globals = core::Globals::default();
    let pretty_alloc = pretty::BoxAllocator;
    let config = codespan_reporting::term::Config::default();
    let (messages_tx, messages_rx) = crossbeam_channel::unbounded();

    let file = SimpleFile::new(path, source);
    let surface_term = surface::Term::from_str(file.source(), &messages_tx);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "surface::Term::from_str messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    let mut state = surface_to_core::State::new(&globals, messages_tx.clone());
    let (core_term, r#type) = state.synth_type(&surface_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "surface_to_core::State::synth_type messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    let mut state = core::typing::State::new(&globals, messages_tx.clone());

    state.synth_type(&core_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "core::typing::State::synth_term messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    state.check_type(&core_term, &r#type);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "core::typing::State::check_term messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    if is_failed {
        Err("failed sample".into())
    } else {
        Ok(())
    }
}

macro_rules! example_test {
    ($test_name:ident, $path:literal) => {
        #[test]
        fn $test_name() -> Result<(), Box<dyn std::error::Error>> {
            run_test(
                concat!("examples/", $path, ".pi"),
                include_str!(concat!("../../examples/", $path, ".pi")),
            )
        }
    };
}

example_test!(comments, "comments");
example_test!(functions, "functions");
example_test!(hello_world, "hello-world");
example_test!(literals, "literals");
example_test!(prelude, "prelude");
example_test!(record_mesh, "record-mesh");
example_test!(record_term_deps, "record-term-deps");
example_test!(record_type_deps, "record-type-deps");
example_test!(universes, "universes");
example_test!(window_settings, "window-settings");
