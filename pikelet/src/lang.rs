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

/// File identifier
pub type FileId = usize;

/// Location metadata, for diagnostic reporting purposes.
#[derive(Debug, Copy, Clone)]
pub enum Location {
    /// Generated code.
    Generated,
    /// Ranges in a text file.
    FileRange(FileId, Range),
}

impl Location {
    pub fn generated() -> Location {
        Location::Generated
    }

    pub fn file_range(file_id: FileId, range: impl Into<Range>) -> Location {
        Location::FileRange(file_id, range.into())
    }

    pub fn merge(self, other: Location) -> Location {
        match (self, other) {
            (Location::Generated, Location::Generated) => Location::Generated,
            (Location::FileRange(file_id0, range0), Location::FileRange(file_id1, range1)) => {
                assert_eq!(
                    file_id0, file_id1,
                    "tried to merge source locations with different file ids"
                );
                Location::FileRange(file_id0, Range::merge(range0, range1))
            }
            (_, _) => panic!("incompatible source ranges"),
        }
    }
}

/// A range of source code.
#[derive(Debug, Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub fn merge(self, other: Range) -> Range {
        Range {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
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

impl From<Range> for std::ops::Range<usize> {
    fn from(src: Range) -> std::ops::Range<usize> {
        src.start..src.end
    }
}

/// Data that covers some range of source code.
#[derive(Debug, Clone)]
pub struct Located<Data> {
    pub location: Location,
    pub data: Data,
}

impl<Data> Located<Data> {
    pub fn new(location: Location, data: Data) -> Located<Data> {
        Located { location, data }
    }

    pub fn generated(data: Data) -> Located<Data> {
        Located::new(Location::generated(), data)
    }
}
