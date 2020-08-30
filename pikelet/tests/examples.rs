//! Integration tests against the language samples directory.

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use pikelet::lang::{core, surface};
use pikelet::pass::surface_to_core;

fn run_test(path: &str, source: &str) {
    let mut is_failed = false;

    let writer = StandardStream::stdout(ColorChoice::Always);
    let globals = core::Globals::default();
    let pretty_alloc = pretty::BoxAllocator;
    let config = codespan_reporting::term::Config::default();
    let (messages_tx, messages_rx) = crossbeam_channel::unbounded();

    let file = SimpleFile::new(path, source);
    let surface_term = surface::Term::from_str(file.source()).unwrap();

    let mut state = surface_to_core::State::new(&globals, messages_tx.clone());
    let (core_term, r#type) = surface_to_core::synth_type(&mut state, &surface_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        eprintln!("surface_to_core::synth_type messages:");
        for message in &messages_rx {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                .unwrap();
        }
        eprintln!();
    }

    let mut state = core::typing::State::new(&globals, messages_tx.clone());

    core::typing::synth_type(&mut state, &core_term);
    if !messages_rx.is_empty() {
        is_failed = true;
        eprintln!("core::typing::synth_term messages:");
        for message in &messages_rx {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                .unwrap();
        }
        eprintln!();
    }

    core::typing::check_type(&mut state, &core_term, &r#type);
    if !messages_rx.is_empty() {
        is_failed = true;
        eprintln!("core::typing::check_term messages:");
        for message in &messages_rx {
            let diagnostic = message.to_diagnostic(&pretty_alloc);
            codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                .unwrap();
        }
        eprintln!();
    }

    if is_failed {
        panic!("failed sample");
    }
}

#[test]
fn comments() {
    run_test(
        "examples/comments.pi",
        include_str!("../../examples/comments.pi"),
    );
}

#[test]
fn cube() {
    run_test("examples/cube.pi", include_str!("../../examples/cube.pi"));
}

#[test]
fn functions() {
    run_test(
        "examples/functions.pi",
        include_str!("../../examples/functions.pi"),
    );
}

#[test]
fn hello_world() {
    run_test(
        "examples/hello-world.pi",
        include_str!("../../examples/hello-world.pi"),
    );
}

#[test]
fn module() {
    run_test(
        "examples/module.pi",
        include_str!("../../examples/module.pi"),
    );
}

#[test]
fn prelude() {
    run_test(
        "examples/prelude.pi",
        include_str!("../../examples/prelude.pi"),
    );
}

#[test]
fn universes() {
    run_test(
        "examples/universes.pi",
        include_str!("../../examples/universes.pi"),
    );
}

#[test]
fn window_settings() {
    run_test(
        "examples/window-settings.pi",
        include_str!("../../examples/window-settings.pi"),
    );
}
