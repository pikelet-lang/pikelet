//! Translations between representations in the compiler

mod concrete_to_core;
mod core_to_concrete;

/// Translate something from its concrete representation
pub trait FromConcrete<T> {
    fn from_concrete(src: T) -> Self;
}

/// Translate something from its core representation
pub trait FromCore<T> {
    fn from_core(src: T) -> Self;
}
