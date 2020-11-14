//! Intermediate languages of the Pikelet compiler.

pub mod surface;
//       🠃
pub mod core;
//       🠃
pub mod anf;
//       🠃
pub mod cc;
//       🠃
//      ...

/// A range of source code.
#[derive(Debug, Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Into<std::ops::Range<usize>> for Range {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

impl From<std::ops::Range<usize>> for Range {
    fn from(src: std::ops::Range<usize>) -> Range {
        Range {
            start: src.start,
            end: src.end,
        }
    }
}

/// Data that covers some range of source code.
#[derive(Debug, Clone)]
pub struct Ranged<Data> {
    pub range: Range,
    pub data: Data,
}

impl<Data> Ranged<Data> {
    pub fn new(range: Range, data: Data) -> Ranged<Data> {
        Ranged { range, data }
    }
}

impl<Data> From<Data> for Ranged<Data> {
    #![allow(clippy::reversed_empty_ranges)]
    fn from(data: Data) -> Ranged<Data> {
        // TODO: Use a better marker for data that does not originate from to a
        // specific source location.
        Ranged::new(Range::from(0..0), data)
    }
}
