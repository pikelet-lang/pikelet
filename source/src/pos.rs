//! Wrapper types that specify positions in a source file

use std::{cmp, fmt};
use std::ops::{Add, Sub};

/// A zero-indexed line offest into a source file
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineIndex(pub usize);

impl LineIndex {
    /// The 1-indexed line number. Useful for pretty printing source locations.
    ///
    /// ```rust
    /// use source::pos::{LineIndex, LineNumber};
    ///
    /// assert_eq!(LineIndex(0).number(), LineNumber(1));
    /// assert_eq!(LineIndex(3).number(), LineNumber(4));
    /// ```
    pub fn number(self) -> LineNumber {
        LineNumber(self.0 + 1)
    }

    /// Apply the function `f` to the underlying index and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> LineIndex {
        LineIndex(f(self.0))
    }
}

impl Default for LineIndex {
    fn default() -> LineIndex {
        LineIndex(0)
    }
}

impl fmt::Debug for LineIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LineIndex(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

/// A 1-indexed line number. Useful for pretty printing source locations.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineNumber(pub usize);

impl LineNumber {
    /// Apply the function `f` to the underlying number and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> LineNumber {
        LineNumber(f(self.0))
    }
}

impl fmt::Debug for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LineNumber(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A zero-indexed column offest into a source file
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnIndex(pub usize);

impl ColumnIndex {
    /// The 1-indexed column number. Useful for pretty printing source locations.
    ///
    /// ```rust
    /// use source::pos::{ColumnIndex, ColumnNumber};
    ///
    /// assert_eq!(ColumnIndex(0).number(), ColumnNumber(1));
    /// assert_eq!(ColumnIndex(3).number(), ColumnNumber(4));
    /// ```
    pub fn number(self) -> ColumnNumber {
        ColumnNumber(self.0 + 1)
    }

    /// Apply the function `f` to the underlying index and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> ColumnIndex {
        ColumnIndex(f(self.0))
    }
}

impl Default for ColumnIndex {
    fn default() -> ColumnIndex {
        ColumnIndex(0)
    }
}

impl fmt::Debug for ColumnIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ColumnIndex(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

/// A 1-indexed column number. Useful for pretty printing source locations.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnNumber(pub usize);

impl ColumnNumber {
    /// Apply the function `f` to the underlying number and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> ColumnNumber {
        ColumnNumber(f(self.0))
    }
}

impl fmt::Debug for ColumnNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ColumnNumber(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for ColumnNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A byte offset in a source file
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BytePos(pub usize);

impl BytePos {
    /// Apply the function `f` to the underlying position and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> BytePos {
        BytePos(f(self.0))
    }
}

impl Default for BytePos {
    fn default() -> BytePos {
        BytePos(0)
    }
}

impl Add for BytePos {
    type Output = BytePos;

    fn add(self, rhs: BytePos) -> BytePos {
        BytePos(self.0 + rhs.0)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos(self.0 - rhs.0)
    }
}

impl fmt::Debug for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BytePos(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A unicode character offset in a source file
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharPos(pub usize);

impl CharPos {
    /// Apply the function `f` to the underlying position and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> CharPos {
        CharPos(f(self.0))
    }
}

impl Default for CharPos {
    fn default() -> CharPos {
        CharPos(0)
    }
}

impl Add for CharPos {
    type Output = CharPos;

    fn add(self, rhs: CharPos) -> CharPos {
        CharPos(self.0 + rhs.0)
    }
}

impl Sub for CharPos {
    type Output = CharPos;

    fn sub(self, rhs: CharPos) -> CharPos {
        CharPos(self.0 - rhs.0)
    }
}

impl fmt::Debug for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CharPos(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A region of code in a source file
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Span {
    lo: BytePos,
    hi: BytePos,
}

impl Span {
    /// Create a new span
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(6));
    /// ```
    ///
    /// `lo` are reordered `hi` to maintain the invariant that `lo <= hi`
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(6), BytePos(3));
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(6));
    /// ```
    pub fn new(lo: BytePos, hi: BytePos) -> Span {
        if lo <= hi {
            Span { lo, hi }
        } else {
            Span { lo: hi, hi: lo }
        }
    }

    pub fn start() -> Span {
        Span {
            lo: BytePos(0),
            hi: BytePos(0),
        }
    }

    /// Create a span over the given character
    ///
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::from_char_utf8(BytePos(3), 'A');
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(4));
    ///
    /// let span = Span::from_char_utf8(BytePos(3), 'ß');
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(5));
    ///
    /// let span = Span::from_char_utf8(BytePos(3), 'ℝ');
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(6));
    ///
    /// let span = Span::from_char_utf8(BytePos(3), '💣');
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(7));
    /// ```
    pub fn from_char_utf8(lo: BytePos, ch: char) -> Span {
        Span {
            lo,
            hi: lo.map(|x| x + ch.len_utf8()),
        }
    }

    /// Get the low byte position
    pub fn lo(self) -> BytePos {
        self.lo
    }

    /// Get the high byte position
    pub fn hi(self) -> BytePos {
        self.hi
    }

    /// Return a new span with the low byte position replaced with the supplied byte position
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.with_lo(BytePos(2)), Span::new(BytePos(2), BytePos(6)));
    /// assert_eq!(span.with_lo(BytePos(5)), Span::new(BytePos(5), BytePos(6)));
    /// assert_eq!(span.with_lo(BytePos(7)), Span::new(BytePos(6), BytePos(7)));
    /// ```
    pub fn with_lo(self, lo: BytePos) -> Span {
        Span::new(lo, self.hi())
    }

    /// Return a new span with the high byte position replaced with the supplied byte position
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.with_hi(BytePos(7)), Span::new(BytePos(3), BytePos(7)));
    /// assert_eq!(span.with_hi(BytePos(5)), Span::new(BytePos(3), BytePos(5)));
    /// assert_eq!(span.with_hi(BytePos(2)), Span::new(BytePos(2), BytePos(3)));
    /// ```
    pub fn with_hi(self, hi: BytePos) -> Span {
        Span::new(self.lo(), hi)
    }

    /// Returns a new span representing just the end-point of this span
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.end_point(), Span::new(BytePos(5), BytePos(6)))
    /// ```
    pub fn end_point(self) -> Span {
        self.with_lo(cmp::max(self.hi() - BytePos(1), self.lo()))
    }

    /// Return true if `self` fully encloses `other`.
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(5), BytePos(8));
    ///
    /// assert_eq!(a.contains(a), true);
    /// assert_eq!(a.contains(Span::new(BytePos(6), BytePos(7))), true);
    /// assert_eq!(a.contains(Span::new(BytePos(6), BytePos(10))), false);
    /// assert_eq!(a.contains(Span::new(BytePos(3), BytePos(6))), false);
    /// ```
    pub fn contains(self, other: Span) -> bool {
        self.lo() <= other.lo() && other.hi() <= self.hi()
    }

    /// Return a `Span` that would enclose both `self` and `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns  ~~~~~~~~~~~~~~~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.to(b), Span::new(BytePos(2), BytePos(14)));
    /// ```
    pub fn to(self, end: Span) -> Span {
        Span::new(cmp::min(self.lo(), end.lo()), cmp::max(self.hi(), end.hi()))
    }

    /// Return a `Span` between the end of `self` to the beginning of `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns         ~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.between(b), Span::new(BytePos(5), BytePos(10)));
    /// ```
    pub fn between(self, end: Span) -> Span {
        Span::new(self.hi(), end.lo())
    }

    /// Return a `Span` between the beginning of `self` to the beginning of `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns  ~~~~~~~~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use source::pos::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.until(b), Span::new(BytePos(2), BytePos(10)));
    /// ```
    pub fn until(self, end: Span) -> Span {
        Span::new(self.lo(), end.lo())
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.lo.fmt(f)?;
        write!(f, "..")?;
        self.hi.fmt(f)?;
        Ok(())
    }
}
