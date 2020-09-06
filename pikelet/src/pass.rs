//! Passes between intermediate languages.
//!
//! The most significant step in this process is the [`surface_to_core`] pass,
//! which handles elaboration of the surface language into the core language,
//! and is the source of most user-facing typing diagnostics.

pub mod core_to_pretty;
pub mod core_to_surface;
pub mod surface_to_core;
pub mod surface_to_pretty;
