//! Integration tests against the language samples directory.

#[cfg(test)]
fn run_test(path: &str, source: &str) -> anyhow::Result<()> {
    use codespan_reporting::files::SimpleFiles;
    use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
    use pikelet::lang::{core, surface};
    use pikelet::pass::surface_to_core;
    use std::io::Write;

    let mut is_failed = false;

    let mut writer = BufferedStandardStream::stdout(ColorChoice::Always);
    let globals = core::Globals::default();
    let pretty_alloc = pretty::BoxAllocator;
    let config = codespan_reporting::term::Config::default();
    let mut files = SimpleFiles::new();
    let (messages_tx, messages_rx) = crossbeam_channel::unbounded();

    let file_id = files.add(path, source);
    let file = files.get(file_id).unwrap();
    let surface_term = surface::Term::from_str(file_id, file.source(), &messages_tx);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "surface::Term::from_str messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    let mut state = surface_to_core::Context::new(&globals, messages_tx.clone());
    let (core_term, r#type) = state.synth_type(&surface_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "surface_to_core::State::synth_type messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    let mut state = core::typing::Context::new(&globals, messages_tx.clone());

    state.synth_type(&core_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        writeln!(writer, "core::typing::State::synth_term messages:")?;
        for message in messages_rx.try_iter() {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)?;
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
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)?;
            writer.flush()?;
        }
        writeln!(writer)?;
    }

    if is_failed {
        anyhow::bail!("failed sample");
    }

    Ok(())
}

macro_rules! test {
    ($test_name:ident, $path:literal) => {
        #[test]
        fn $test_name() -> anyhow::Result<()> {
            crate::run_test(
                concat!($path, ".pi"),
                include_str!(concat!("../../", $path, ".pi")),
            )
        }
    };
}

mod examples {
    test!(hello_world, "examples/hello-world");
    test!(meta, "examples/meta");
    test!(prelude, "examples/prelude");
    test!(record_mesh, "examples/record-mesh");
    test!(window_settings, "examples/window-settings");
}

mod tests {
    test!(comments, "tests/comments");
    test!(functions, "tests/functions");
    test!(literals, "tests/literals");
    test!(record_term_deps, "tests/record-term-deps");
    test!(record_type_deps, "tests/record-type-deps");
}
