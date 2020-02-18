//! Integration tests against the language samples directory.

use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use pikelet::{core, surface};

fn run_test(source: &str) {
    let mut is_failed = false;

    let surface_term = surface::Term::from_str(source).unwrap();

    let globals = core::Globals::default();
    let mut state = surface::projections::core::State::new(&globals);
    let (core_term, r#type) = surface::projections::core::synth_term(&mut state, &surface_term);
    let messages = state.drain_messages().collect::<Vec<_>>();
    if !messages.is_empty() {
        is_failed = true;
        eprintln!("surface::projections::core::synth_term messages:");
        for message in messages {
            let display_list = DisplayList::from(message.to_snippet(source)); // TODO: file path
            let formatter = DisplayListFormatter::new(true, false);
            eprintln!("{}", formatter.format(&display_list));
        }
        eprintln!();
    }

    let mut state = core::typing::State::new(&globals);
    core::typing::synth_term(&mut state, &core_term);
    let messages = state.drain_messages().collect::<Vec<_>>();
    if !messages.is_empty() {
        is_failed = true;
        eprintln!("core::typing::synth_term messages:");
        for message in messages {
            eprintln!("  {:?}", message);
        }
        eprintln!();
    }

    let mut state = core::typing::State::new(&globals);
    core::typing::check_term(&mut state, &core_term, &r#type);
    let messages = state.drain_messages().collect::<Vec<_>>();
    if !messages.is_empty() {
        is_failed = true;
        eprintln!("core::typing::check_term messages:");
        for message in messages {
            eprintln!("  {:?}", message);
        }
        eprintln!();
    }

    if is_failed {
        panic!("failed sample");
    }
}

#[test]
fn cube() {
    run_test(include_str!("../../examples/cube.pi"));
}

#[test]
fn functions() {
    run_test(include_str!("../../examples/functions.pi"));
}

#[test]
fn hello_world() {
    run_test(include_str!("../../examples/hello-world.pi"));
}

#[test]
fn module() {
    run_test(include_str!("../../examples/module.pi"));
}

#[test]
fn universes() {
    run_test(include_str!("../../examples/universes.pi"));
}

#[test]
fn window_settings() {
    run_test(include_str!("../../examples/window-settings.pi"));
}
