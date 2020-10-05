//! The core language.
//!
//! This is not intended to be used directly by users of the programming
//! language.

use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use crate::lang::Ranged;

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

pub type Term = Ranged<TermData>;

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
    RecordTerm(BTreeMap<String, Arc<Term>>),
    /// Record eliminations.
    ///
    /// Also known as: record projection, field lookup.
    RecordElim(Arc<Term>, String),

    /// Ordered sequences.
    Sequence(Vec<Arc<Term>>),

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

        let global = |name: &str| Arc::new(Term::from(TermData::Global(name.to_owned())));
        let type_type = |level| Arc::new(Term::from(TermData::TypeType(UniverseLevel(level))));
        let function_type = |input_type, output_type| {
            Arc::new(Term::from(TermData::FunctionType(
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

/// A De Bruijn level into the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalLevel(u32);

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

    /// Convert a variable level to a variable index in the current environment.
    pub fn index(self, level: LocalLevel) -> Option<LocalIndex> {
        Some(LocalIndex(u32::checked_sub(self.0, level.0 + 1)?))
    }

    /// Convert a variable index to a variable level in the current environment.
    pub fn level(self, index: LocalIndex) -> Option<LocalLevel> {
        Some(LocalLevel(u32::checked_sub(self.0, index.0 + 1)?))
    }
}

/// A local environment.
#[derive(Clone)]
pub struct Locals<Entry> {
    /// The local values that are currently defined in the environment.
    values: im::Vector<Entry>,
}

impl<Entry: Clone> Locals<Entry> {
    /// Create a new local environment.
    pub fn new() -> Locals<Entry> {
        Locals {
            values: im::Vector::new(),
        }
    }

    /// Get the size of the environment.
    pub fn size(&self) -> LocalSize {
        LocalSize(self.values.len() as u32) // FIXME: Check for overflow?
    }

    /// Lookup an entry in the environment.
    pub fn get(&self, index: LocalIndex) -> Option<&Entry> {
        self.values
            .get(self.values.len().checked_sub(index.0 as usize + 1)?)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
        self.values.push_back(entry);
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) -> Option<Entry> {
        self.values.pop_back()
    }

    /// Pop a number of entries off the environment.
    pub fn pop_many(&mut self, count: usize) {
        self.values
            .truncate(self.values.len().saturating_sub(count));
    }

    /// Clear the entries from the environment.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

impl<Entry: Clone + fmt::Debug> fmt::Debug for Locals<Entry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Locals")
            .field("entries", &self.values)
            .finish()
    }
}

pub trait HasType {
    fn r#type() -> Arc<Term>;
}

macro_rules! impl_has_type {
    ($Self:ty, $term:expr) => {
        impl HasType for $Self {
            fn r#type() -> Arc<Term> {
                Arc::new($term)
            }
        }
    };
}

impl_has_type!(bool, Term::from(TermData::Global("Bool".to_owned())));
impl_has_type!(u8, Term::from(TermData::Global("U8".to_owned())));
impl_has_type!(u16, Term::from(TermData::Global("U16".to_owned())));
impl_has_type!(u32, Term::from(TermData::Global("U32".to_owned())));
impl_has_type!(u64, Term::from(TermData::Global("U64".to_owned())));
impl_has_type!(i8, Term::from(TermData::Global("S8".to_owned())));
impl_has_type!(i16, Term::from(TermData::Global("S16".to_owned())));
impl_has_type!(i32, Term::from(TermData::Global("S32".to_owned())));
impl_has_type!(i64, Term::from(TermData::Global("S64".to_owned())));
impl_has_type!(f32, Term::from(TermData::Global("F32".to_owned())));
impl_has_type!(f64, Term::from(TermData::Global("F64".to_owned())));
impl_has_type!(char, Term::from(TermData::Global("Char".to_owned())));
impl_has_type!(String, Term::from(TermData::Global("String".to_owned())));
impl_has_type!(str, Term::from(TermData::Global("String".to_owned())));

impl<T: HasType> HasType for Vec<T> {
    fn r#type() -> Arc<Term> {
        Arc::new(Term::from(TermData::FunctionElim(
            Arc::new(Term::from(TermData::Global("List".to_owned()))),
            T::r#type(),
        )))
    }
}

macro_rules! impl_has_type_array {
    ($($len:expr),*) => {
        $(impl<T: HasType> HasType for [T; $len] {
            fn r#type() -> Arc<Term> {
                Arc::new(Term::from(TermData::FunctionElim(
                    Arc::new(Term::from(TermData::FunctionElim(
                        Arc::new(Term::from(TermData::Global("List".to_owned()))),
                        Arc::new(Term::from(TermData::from(Constant::U32($len as u32)))),
                    ))),
                    T::r#type(),
                )))
            }
        })*
    };
}

impl_has_type_array!(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32
);

/// Attempt to deserialize something from a `Term`.
///
/// # Laws
///
/// ```skipped
/// check_type(&term, &Self::r#type()) && Self::try_from_term(term).is_ok()
/// ```
// TODO: Make more efficient with visitors
pub trait TryFromTerm: HasType + Sized {
    type Error: Sized;
    fn try_from_term(term: &Term) -> Result<Self, Self::Error>;
}

macro_rules! impl_try_from_term {
    ($Self:ty, |$p:pat| $term:expr) => {
        impl TryFromTerm for $Self {
            type Error = ();

            fn try_from_term(term: &Term) -> Result<$Self, ()> {
                match &term.data {
                    $p => $term,
                    _ => Err(()),
                }
            }
        }
    };
}

impl_try_from_term!(bool, |TermData::Global(name)| match name.as_str() {
    "true" => Ok(true),
    "false" => Ok(false),
    _ => Err(()),
});
impl_try_from_term!(u8, |TermData::Constant(Constant::U8(value))| Ok(*value));
impl_try_from_term!(u16, |TermData::Constant(Constant::U16(value))| Ok(*value));
impl_try_from_term!(u32, |TermData::Constant(Constant::U32(value))| Ok(*value));
impl_try_from_term!(u64, |TermData::Constant(Constant::U64(value))| Ok(*value));
impl_try_from_term!(i8, |TermData::Constant(Constant::S8(value))| Ok(*value));
impl_try_from_term!(i16, |TermData::Constant(Constant::S16(value))| Ok(*value));
impl_try_from_term!(i32, |TermData::Constant(Constant::S32(value))| Ok(*value));
impl_try_from_term!(i64, |TermData::Constant(Constant::S64(value))| Ok(*value));
impl_try_from_term!(f32, |TermData::Constant(Constant::F32(value))| Ok(*value));
impl_try_from_term!(f64, |TermData::Constant(Constant::F64(value))| Ok(*value));
impl_try_from_term!(char, |TermData::Constant(Constant::Char(value))| Ok(*value));
impl_try_from_term!(String, |TermData::Constant(Constant::String(value))| Ok(
    value.clone(),
));

impl<T: TryFromTerm> TryFromTerm for Vec<T> {
    type Error = ();

    fn try_from_term(term: &Term) -> Result<Vec<T>, ()> {
        match &term.data {
            TermData::Sequence(entry_terms) => entry_terms
                .iter()
                .map(|entry_term| T::try_from_term(entry_term).map_err(|_| ()))
                .collect::<Result<Vec<_>, ()>>(),
            _ => Err(()),
        }
    }
}

macro_rules! impl_try_from_term_array {
    ($($len:expr),*) => {
        $(impl<T: TryFromTerm + Sized> TryFromTerm for [T; $len] {
            type Error = ();

            fn try_from_term(term: &Term) -> Result<[T; $len], ()> {
                match &term.data {
                    TermData::Sequence(entry_terms) if entry_terms.len() == $len => {
                        use std::mem::MaybeUninit;

                        let mut entries: [MaybeUninit::<T>; $len] = unsafe {
                            MaybeUninit::uninit().assume_init()
                        };
                        for (i, entry_term) in entry_terms.iter().enumerate() {
                            entries[i] = MaybeUninit::new(T::try_from_term(entry_term).map_err(|_| ())?);
                        }

                        // NOTE: We'd prefer to do the following:
                        //
                        // ```
                        // Ok(unsafe { std::mem::transmute::<_, [T; $len]>(entries) })
                        // ```
                        //
                        // Sadly we run into the following issue: https://github.com/rust-lang/rust/issues/61956
                        // For this reason we need to do the following (hideous) workaround:

                        let ptr = &mut entries as *mut _ as *mut [T; $len];
                        let result = unsafe { ptr.read() };
                        core::mem::forget(entries);
                        Ok(result)
                    },
                    _ => Err(()),
                }
            }
        })*
    };
}

impl_try_from_term_array!(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32
);

/// Serialize something to a `Term`.
///
/// # Laws
///
/// ```skipped
/// check_type(&Self::to_term(&value), &Self::r#type()) == true
/// ```
// TODO: Make more efficient with visitors
pub trait ToTerm: HasType {
    fn to_term(&self) -> Term;
}

macro_rules! impl_to_term {
    ($Self:ty, |$p:pat| $term_data:expr) => {
        impl ToTerm for $Self {
            fn to_term(&self) -> Term {
                let $p = self;
                Term::from($term_data)
            }
        }
    };
}

impl_to_term!(bool, |value| match value {
    true => TermData::Global("true".to_owned()),
    false => TermData::Global("false".to_owned()),
});
impl_to_term!(u8, |value| TermData::from(Constant::U8(*value)));
impl_to_term!(u16, |value| TermData::from(Constant::U16(*value)));
impl_to_term!(u32, |value| TermData::from(Constant::U32(*value)));
impl_to_term!(u64, |value| TermData::from(Constant::U64(*value)));
impl_to_term!(i8, |value| TermData::from(Constant::S8(*value)));
impl_to_term!(i16, |value| TermData::from(Constant::S16(*value)));
impl_to_term!(i32, |value| TermData::from(Constant::S32(*value)));
impl_to_term!(i64, |value| TermData::from(Constant::S64(*value)));
impl_to_term!(f32, |value| TermData::from(Constant::F32(*value)));
impl_to_term!(f64, |value| TermData::from(Constant::F64(*value)));
impl_to_term!(char, |value| TermData::from(Constant::Char(*value)));
impl_to_term!(String, |value| TermData::from(Constant::String(
    value.clone()
)));
impl_to_term!(str, |value| TermData::from(Constant::String(
    value.to_owned()
)));

impl<T: ToTerm> ToTerm for Vec<T> {
    fn to_term(&self) -> Term {
        Term::from(TermData::Sequence(
            self.iter()
                .map(|entry_term| Arc::new(T::to_term(entry_term)))
                .collect(),
        ))
    }
}

macro_rules! impl_to_term_array {
    ($($len:expr),*) => {
        $(impl<T: ToTerm> ToTerm for [T; $len] {
            fn to_term(&self) -> Term {
                Term::from(TermData::Sequence(
                    self.iter()
                        .map(|entry_term| Arc::new(T::to_term(entry_term)))
                        .collect(),
                ))
            }
        })*
    };
}

impl_to_term_array!(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32
);
