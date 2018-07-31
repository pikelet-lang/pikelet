//! The syntax of the language

use std::fmt;

pub mod concrete;
pub mod core;
pub mod parse;
pub mod pretty;
pub mod raw;
pub mod translation;

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, BoundTerm)]
pub struct Level(pub u32);

impl Level {
    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl From<u32> for Level {
    fn from(src: u32) -> Level {
        Level(src)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
