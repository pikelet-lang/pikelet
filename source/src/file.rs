//! Various source mapping utilities

use std::borrow::Cow;
use std::{fmt, io};
use std::path::PathBuf;

use pos::{BytePos, ColumnIndex, LineIndex};

#[derive(Clone, Debug)]
pub enum FileName {
    /// A real file on disk
    Real(PathBuf),
    /// A synthetic file, eg. from the REPL
    Virtual(Cow<'static, str>),
}

impl fmt::Display for FileName {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FileName::Real(ref path) => write!(fmt, "{}", path.display()),
            FileName::Virtual(ref name) => write!(fmt, "<{}>", name),
        }
    }
}

/// Some source code
pub struct Source {
    /// The name of the file that the source came from
    name: FileName,
    /// The complete source code
    src: String,
    /// Locations of the line beginnings in the source
    line_offsets: Vec<BytePos>,
    /// The byte offset for the last byte in the file
    end_offset: BytePos,
}

impl Source {
    /// Construct a new sorce code, creating an index of line start locations
    pub fn new(name: FileName, src: String) -> Source {
        use std::iter;

        let mut end_offset = BytePos(0);
        let line_offsets = {
            let input_indices = src.bytes()
                .inspect(|_| end_offset.0 += 1)
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(i, _)| BytePos(i + 1)); // index of first char in the line

            iter::once(BytePos(0)).chain(input_indices).collect()
        };

        Source {
            name,
            src,
            line_offsets,
            end_offset,
        }
    }

    /// Read some source code from a file
    pub fn from_file(name: PathBuf) -> io::Result<Source> {
        use std::fs::File;
        use std::io::Read;

        let mut file = File::open(&name)?;
        let mut src = String::new();
        file.read_to_string(&mut src)?;

        Ok(Source::new(FileName::Real(name), src))
    }

    /// The name of the file that the source came from
    pub fn name(&self) -> &FileName {
        &self.name
    }

    /// The underlying source code
    pub fn src(&self) -> &str {
        &self.src
    }

    /// Returns the byte offset to the start of `line`
    ///
    /// ```rust
    /// use source::file::{FileName, Source};
    /// use source::pos::{BytePos, LineIndex};
    ///
    /// let source = Source::new(
    ///     FileName::Virtual("test".into()),
    ///     "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned(),
    /// );
    ///
    /// assert_eq!(source.line_offset(LineIndex(0)), Some(BytePos(0)));
    /// assert_eq!(source.line_offset(LineIndex(1)), Some(BytePos(7)));
    /// assert_eq!(source.line_offset(LineIndex(2)), Some(BytePos(13)));
    /// assert_eq!(source.line_offset(LineIndex(3)), Some(BytePos(14)));
    /// assert_eq!(source.line_offset(LineIndex(4)), Some(BytePos(20)));
    /// assert_eq!(source.line_offset(LineIndex(5)), Some(BytePos(26)));
    /// assert_eq!(source.line_offset(LineIndex(6)), None);
    /// ```
    pub fn line_offset(&self, index: LineIndex) -> Option<BytePos> {
        self.line_offsets.get(index.0).cloned()
    }

    /// Returns the line and column location of `byte`
    ///
    /// ```rust
    /// use source::file::{FileName, Source};
    /// use source::pos::{BytePos, ColumnIndex, LineIndex};
    ///
    /// let source = Source::new(
    ///     FileName::Virtual("test".into()),
    ///     "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned(),
    /// );
    ///
    /// assert_eq!(source.location(BytePos(0)), Some((LineIndex(0), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(7)), Some((LineIndex(1), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(13)), Some((LineIndex(2), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(14)), Some((LineIndex(3), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(20)), Some((LineIndex(4), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(26)), Some((LineIndex(5), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(300)), None);
    /// ```
    pub fn location(&self, absolute_offset: BytePos) -> Option<(LineIndex, ColumnIndex)> {
        self.line_index(absolute_offset).and_then(|line_index| {
            self.line_offset(line_index)
                .map(|line_offset| (line_index, ColumnIndex((absolute_offset - line_offset).0)))
        })
    }

    /// Returns the line index that the byte offset points to
    ///
    /// ```rust
    /// use source::file::{FileName, Source};
    /// use source::pos::{BytePos, LineIndex};
    ///
    /// let source = Source::new(
    ///     FileName::Virtual("test".into()),
    ///     "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned(),
    /// );
    ///
    /// assert_eq!(source.line_index(BytePos(0)), Some(LineIndex(0)));
    /// assert_eq!(source.line_index(BytePos(7)), Some(LineIndex(1)));
    /// assert_eq!(source.line_index(BytePos(13)), Some(LineIndex(2)));
    /// assert_eq!(source.line_index(BytePos(14)), Some(LineIndex(3)));
    /// assert_eq!(source.line_index(BytePos(20)), Some(LineIndex(4)));
    /// assert_eq!(source.line_index(BytePos(26)), Some(LineIndex(5)));
    /// assert_eq!(source.line_index(BytePos(300)), None);
    /// ```
    pub fn line_index(&self, absolute_offset: BytePos) -> Option<LineIndex> {
        if absolute_offset <= self.end_offset {
            let num_lines = self.line_offsets.len();

            Some(LineIndex(
                (0..num_lines)
                    .filter(|&i| self.line_offsets[i] > absolute_offset)
                    .map(|i| i - 1)
                    .next()
                    .unwrap_or(num_lines - 1),
            ))
        } else {
            None
        }
    }
}
