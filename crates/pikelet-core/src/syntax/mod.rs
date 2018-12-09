use pretty::{BoxDoc, Doc};
use std::fmt;
use std::ops::{Add, AddAssign};

pub mod core;
pub mod domain;

/// An effectively 'infinite' line length for when we don't have an explicit
/// width provided for pretty printing.
///
/// `pretty.rs` seems to bug-out and break on every line when using
/// `usize::MAX`, so we'll just use a really big number instead...
pub const PRETTY_FALLBACK_WIDTH: usize = 1_000_000;

/// Imported definitions
#[derive(Clone)]
pub enum Import {
    Term(core::RcTerm),
    Prim(for<'a> fn(&'a [domain::RcValue]) -> Option<domain::RcValue>),
}

impl fmt::Debug for Import {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Import::Term(ref term) => f.debug_tuple("Term").field(term).finish(),
            Import::Prim(_) => f.debug_tuple("Prim").field(&"|params| { .. }").finish(),
        }
    }
}

/// Literals
///
/// We could church encode all the things, but that would be prohibitively expensive!
#[derive(Debug, Clone, PartialEq, PartialOrd, moniker::BoundTerm, moniker::BoundPattern)]
pub enum Literal {
    Bool(bool),
    String(String),
    Char(char),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    S8(i8),
    S16(i16),
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
}

impl Literal {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Literal::Bool(true) => Doc::text("true"),
            Literal::Bool(false) => Doc::text("false"),
            Literal::String(ref value) => Doc::text(format!("{:?}", value)),
            Literal::Char(value) => Doc::text(format!("{:?}", value)),
            Literal::U8(value) => Doc::as_string(&value),
            Literal::U16(value) => Doc::as_string(&value),
            Literal::U32(value) => Doc::as_string(&value),
            Literal::U64(value) => Doc::as_string(&value),
            Literal::S8(value) => Doc::as_string(&value),
            Literal::S16(value) => Doc::as_string(&value),
            Literal::S32(value) => Doc::as_string(&value),
            Literal::S64(value) => Doc::as_string(&value),
            Literal::F32(value) => Doc::as_string(&value),
            Literal::F64(value) => Doc::as_string(&value),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, moniker::BoundTerm)]
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

/// A shift in universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, moniker::BoundTerm, moniker::BoundPattern)]
pub struct LevelShift(pub u32);

impl From<u32> for LevelShift {
    fn from(src: u32) -> LevelShift {
        LevelShift(src)
    }
}

impl fmt::Display for LevelShift {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add for LevelShift {
    type Output = LevelShift;

    fn add(self, other: LevelShift) -> LevelShift {
        LevelShift(self.0 + other.0)
    }
}

impl AddAssign for LevelShift {
    fn add_assign(&mut self, other: LevelShift) {
        self.0 += other.0;
    }
}

impl Add<LevelShift> for Level {
    type Output = Level;

    fn add(self, other: LevelShift) -> Level {
        Level(self.0 + other.0)
    }
}

impl AddAssign<LevelShift> for Level {
    fn add_assign(&mut self, other: LevelShift) {
        self.0 += other.0;
    }
}

/// A label that describes the name of a field in a record
///
/// Labels are significant when comparing for alpha-equality
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, moniker::BoundPattern, moniker::BoundTerm)]
pub struct Label(pub String);

impl From<String> for Label {
    fn from(src: String) -> Label {
        Label(src)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
