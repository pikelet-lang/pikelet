//! The core language.
//!
//! This is not intended to be used directly by users of the programming
//! language.

use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use crate::lang::Located;

pub mod marshall;
pub mod semantics;
pub mod typing;

/// Constants used in the core language.
// FIXME: Partial eq for floating point numbers
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    /// 8-bit unsigned integers.
    U8(u8),
    /// 16-bit unsigned integers.
    U16(u16),
    /// 32-bit unsigned integers.
    U32(u32),
    /// 64-bit unsigned integers.
    U64(u64),
    /// 8-bit signed [two's complement] integers.
    ///
    /// [two's complement]: https://en.wikipedia.org/wiki/Two%27s_complement
    S8(i8),
    /// 16-bit signed [two's complement] integers.
    ///
    /// [two's complement]: https://en.wikipedia.org/wiki/Two%27s_complement
    S16(i16),
    /// 32-bit signed [two's complement] integers.
    ///
    /// [two's complement]: https://en.wikipedia.org/wiki/Two%27s_complement
    S32(i32),
    /// 64-bit signed [two's complement] integers.
    ///
    /// [two's complement]: https://en.wikipedia.org/wiki/Two%27s_complement
    S64(i64),
    /// 32-bit [IEEE-754] floating point numbers.
    ///
    /// [IEEE-754]: https://en.wikipedia.org/wiki/IEEE_754
    F32(f32),
    /// 64-bit [IEEE-754] floating point numbers.
    ///
    /// [IEEE-754]: https://en.wikipedia.org/wiki/IEEE_754
    F64(f64),
    /// [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
    Char(char),
    /// [UTF-8] encoded strings.
    ///
    /// [UTF-8]: http://www.unicode.org/glossary/#UTF_8
    String(String),
}

/// Universe levels.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct UniverseLevel(pub u32);

impl std::ops::Add<UniverseOffset> for UniverseLevel {
    type Output = Option<UniverseLevel>;

    fn add(self, other: UniverseOffset) -> Option<UniverseLevel> {
        u32::checked_add(self.0, other.0).map(UniverseLevel)
    }
}

impl From<u32> for UniverseLevel {
    fn from(level: u32) -> UniverseLevel {
        UniverseLevel(level)
    }
}

/// Universe level offsets.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct UniverseOffset(pub u32);

impl std::ops::Add<UniverseOffset> for UniverseOffset {
    type Output = Option<UniverseOffset>;

    fn add(self, other: UniverseOffset) -> Option<UniverseOffset> {
        u32::checked_add(self.0, other.0).map(UniverseOffset)
    }
}

impl From<u32> for UniverseOffset {
    fn from(offset: u32) -> UniverseOffset {
        UniverseOffset(offset)
    }
}

pub type Term = Located<TermData>;

/// Terms in the core language.
#[derive(Clone, Debug)]
pub enum TermData {
    /// Global variables.
    Global(String),
    /// Local variables.
    Local(LocalIndex),

    /// Annotated terms
    Ann(Arc<Term>, Arc<Term>),

    /// The type of types.
    TypeType(UniverseLevel),
    /// Lift a term by the given number of universe levels.
    Lift(Arc<Term>, UniverseOffset),

    /// Function types.
    ///
    /// Also known as: pi type, dependent product type.
    FunctionType(Option<String>, Arc<Term>, Arc<Term>),
    /// Function terms.
    ///
    /// Also known as: lambda abstraction, anonymous function.
    FunctionTerm(String, Arc<Term>),
    /// Function eliminations.
    ///
    /// Also known as: function application.
    FunctionElim(Arc<Term>, Arc<Term>),

    /// Record types.
    RecordType(Arc<[(String, Arc<Term>)]>),
    /// Record terms.
    RecordTerm(Arc<[(String, Arc<Term>)]>),
    /// Record eliminations.
    ///
    /// Also known as: record projection, field lookup.
    RecordElim(Arc<Term>, String),

    /// Array terms.
    ArrayTerm(Vec<Arc<Term>>),
    /// List terms.
    ListTerm(Vec<Arc<Term>>),

    /// Constants.
    Constant(Constant),

    /// Error sentinel.
    Error,
}

impl From<Constant> for TermData {
    fn from(constant: Constant) -> TermData {
        TermData::Constant(constant)
    }
}

/// An environment of global definitions.
pub struct Globals {
    entries: BTreeMap<String, (Arc<Term>, Option<Arc<Term>>)>,
}

impl Globals {
    pub fn new(entries: BTreeMap<String, (Arc<Term>, Option<Arc<Term>>)>) -> Globals {
        Globals { entries }
    }

    pub fn get(&self, name: &str) -> Option<&(Arc<Term>, Option<Arc<Term>>)> {
        self.entries.get(name)
    }

    pub fn entries(&self) -> impl Iterator<Item = (&String, &(Arc<Term>, Option<Arc<Term>>))> {
        self.entries.iter()
    }
}

impl Default for Globals {
    fn default() -> Globals {
        let mut entries = BTreeMap::new();

        let global = |name: &str| Arc::new(Term::generated(TermData::Global(name.to_owned())));
        let type_type = |level| Arc::new(Term::generated(TermData::TypeType(UniverseLevel(level))));
        let function_type = |input_type, output_type| {
            Arc::new(Term::generated(TermData::FunctionType(
                None,
                input_type,
                output_type,
            )))
        };

        entries.insert("Type".to_owned(), (type_type(1), Some(type_type(0))));
        entries.insert("Bool".to_owned(), (global("Type"), None));
        entries.insert("U8".to_owned(), (global("Type"), None));
        entries.insert("U16".to_owned(), (global("Type"), None));
        entries.insert("U32".to_owned(), (global("Type"), None));
        entries.insert("U64".to_owned(), (global("Type"), None));
        entries.insert("S8".to_owned(), (global("Type"), None));
        entries.insert("S16".to_owned(), (global("Type"), None));
        entries.insert("S32".to_owned(), (global("Type"), None));
        entries.insert("S64".to_owned(), (global("Type"), None));
        entries.insert("F32".to_owned(), (global("Type"), None));
        entries.insert("F64".to_owned(), (global("Type"), None));
        entries.insert("Char".to_owned(), (global("Type"), None));
        entries.insert("String".to_owned(), (global("Type"), None));
        entries.insert("true".to_owned(), (global("Bool"), None));
        entries.insert("false".to_owned(), (global("Bool"), None));
        entries.insert(
            "Array".to_owned(),
            (
                function_type(global("U32"), function_type(type_type(0), type_type(0))),
                None,
            ),
        );
        entries.insert(
            "List".to_owned(),
            (function_type(type_type(0), type_type(0)), None),
        );

        Globals::new(entries)
    }
}

/// A De Bruijn index into the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalIndex(pub u32);

impl LocalIndex {
    /// Convert a local index to a local level in the current environment.
    ///
    /// `None` is returned if the local environment is not large enough to
    /// contain the local variable.
    pub fn to_level(self, size: LocalSize) -> Option<LocalLevel> {
        Some(LocalLevel(u32::checked_sub(size.0, self.0 + 1)?))
    }
}

/// A De Bruijn level into the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalLevel(u32);

impl LocalLevel {
    /// Convert a local level to a local index in the current environment.
    ///
    /// `None` is returned if the local environment is not large enough to
    /// contain the local variable.
    pub fn to_index(self, size: LocalSize) -> Option<LocalIndex> {
        Some(LocalIndex(u32::checked_sub(size.0, self.0 + 1)?))
    }
}

/// The size of the local environment, used for index-to-level conversions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalSize(u32);

impl LocalSize {
    pub fn increment(self) -> LocalSize {
        LocalSize(self.0 + 1)
    }

    /// Return the level of the next variable to be added to the environment.
    pub fn next_level(self) -> LocalLevel {
        LocalLevel(self.0)
    }
}

/// A local environment.
#[derive(Clone)]
pub struct Locals<Entry> {
    /// The local entries that are currently defined in the environment.
    entries: im::Vector<Entry>,
}

impl<Entry: Clone> Locals<Entry> {
    /// Create a new local environment.
    pub fn new() -> Locals<Entry> {
        Locals {
            entries: im::Vector::new(),
        }
    }

    /// Get the size of the environment.
    pub fn size(&self) -> LocalSize {
        LocalSize(self.entries.len() as u32) // FIXME: Check for overflow?
    }

    /// Lookup an entry in the environment.
    pub fn get(&self, index: LocalIndex) -> Option<&Entry> {
        self.entries
            .get(self.entries.len().checked_sub(index.0 as usize + 1)?)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
        self.entries.push_back(entry);
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) -> Option<Entry> {
        self.entries.pop_back()
    }

    /// Pop a number of entries off the environment.
    pub fn pop_many(&mut self, count: usize) {
        self.entries
            .truncate(self.entries.len().saturating_sub(count));
    }

    /// Clear the entries from the environment.
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl<Entry: Clone + fmt::Debug> fmt::Debug for Locals<Entry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Locals")
            .field("entries", &self.entries)
            .finish()
    }
}
