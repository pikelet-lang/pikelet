//! A simple language.

#![allow(clippy::new_without_default, clippy::drop_ref)]

pub mod core;
pub mod surface;

/// Pop a number of elements off the end of a vector in one go.
fn pop_many<T>(vec: &mut Vec<T>, count: usize) {
    vec.truncate(vec.len().saturating_sub(count));
}
