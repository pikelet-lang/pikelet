//! Integration tests against the language samples directory.

use pikelet::{core, surface};

fn run_test(input: &str) {
    let mut is_failed = false;

    let surface_term = surface::Term::from_str(input).unwrap();

    let globals = core::Globals::default();
    let mut state = surface::projections::core::State::new(&globals);
    let (core_term, r#type) = surface::projections::core::synth_term(&mut state, &surface_term);
    if !state.errors.is_empty() {
        is_failed = true;
        eprintln!("surface::projections::core::synth_term errors:");
        for error in state.errors {
            eprintln!("  {:?}", error);
        }
        eprintln!();
    }

    eprintln!("{:?}", core_term);
    eprintln!("{:?}", r#type);

    let mut state = core::typing::State::new(&globals);
    core::typing::synth_term(&mut state, &core_term);
    if !state.errors.is_empty() {
        is_failed = true;
        eprintln!("core::typing::synth_term errors:");
        for error in state.errors {
            eprintln!("  {:?}", error);
        }
        eprintln!();
    }

    let mut state = core::typing::State::new(&globals);
    core::typing::check_term(&mut state, &core_term, &r#type);
    if !state.errors.is_empty() {
        is_failed = true;
        eprintln!("core::typing::check_term errors:");
        for error in state.errors {
            eprintln!("  {:?}", error);
        }
        eprintln!();
    }

    eprintln!("{:?}", r#type);

    if is_failed {
        panic!("failed sample");
    }
}

#[test]
fn cube() {
    run_test(include_str!("../samples/cube.pi"));
}

#[test]
fn hello_world() {
    run_test(include_str!("../samples/hello-world.pi"));
}

#[test]
fn universes() {
    run_test(include_str!("../samples/universes.pi"));
}

#[test]
fn window_settings() {
    run_test(include_str!("../samples/window-settings.pi"));
}
