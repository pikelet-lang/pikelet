//! Translations between representations in the compiler

mod concrete_to_core;
mod core_to_concrete;

pub use self::concrete_to_core::ToCore;
pub use self::core_to_concrete::ToConcrete;
