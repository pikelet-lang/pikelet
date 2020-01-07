//! The core language.
//!
//! This is not intended to be used directly by users of the programming
//! language.

use std::collections::BTreeMap;
use std::sync::Arc;

pub mod projections;
pub mod semantics;
pub mod typing;

/// Constants used in the core language.
// FIXME: Partial eq for floating point numbers
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
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
    Char(char),
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

/// Terms in the core language.
#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    /// The type of types.
    Universe(UniverseLevel),
    /// Global variables.
    Global(String),
    /// Local variables.
    Local(LocalIndex),
    /// Constants.
    Constant(Constant),
    /// Ordered sequences.
    Sequence(Vec<Arc<Term>>),
    /// Annotated terms
    Ann(Arc<Term>, Arc<Term>),
    /// Record types.
    RecordType(Vec<(String, Arc<Term>)>),
    /// Record terms.
    RecordTerm(BTreeMap<String, Arc<Term>>),
    /// Record eliminations (field access).
    RecordElim(Arc<Term>, String),
    /// Function types.
    FunctionType(Arc<Term>, Arc<Term>),
    /// Function terms (lambda abstractions).
    FunctionTerm(String, Arc<Term>),
    /// Function eliminations (function application).
    FunctionElim(Arc<Term>, Arc<Term>),
    /// Lift a term by the given number of universe levels.
    Lift(Arc<Term>, UniverseOffset),
    /// Error sentinel.
    Error,
}

impl Term {
    /// Create a universe at the given level.
    pub fn universe(level: impl Into<UniverseLevel>) -> Term {
        Term::Universe(level.into())
    }

    /// Create a global variable.
    pub fn global(name: impl Into<String>) -> Term {
        Term::Global(name.into())
    }

    /// Lift a term by the given offset.
    pub fn lift(self, offset: impl Into<UniverseOffset>) -> Term {
        match offset.into() {
            UniverseOffset(0) => self,
            offset => Term::Lift(Arc::new(self), offset),
        }
    }
}

/// Values in the core language.
#[derive(Clone, Debug)]
pub enum Value {
    /// The type of types.
    Universe(UniverseLevel),
    /// A suspended elimination (neutral value).
    ///
    /// This is a value that cannot be reduced further as a result of being
    /// stuck on some head. Instead we maintain a 'spine' of eliminators so that
    /// we may perform further reduction later on.
    ///
    /// A type annotation is maintained in order to allow for type-directed
    /// [eta-conversion] to take place during read-back.
    ///
    /// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
    Elim(Head, Vec<Elim>, Arc<Value>),
    /// Constants.
    Constant(Constant),
    /// Ordered sequences.
    Sequence(Vec<Arc<Value>>),
    /// Record types.
    RecordType(Vec<(String, Arc<Value>)>),
    /// Record terms.
    RecordTerm(BTreeMap<String, Arc<Value>>),
    /// Function types.
    FunctionType(Arc<Value>, Arc<Value>),
    /// Function terms (lambda abstractions).
    FunctionTerm(String, Closure),
    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a universe at the given level.
    pub fn universe(level: impl Into<UniverseLevel>) -> Value {
        Value::Universe(level.into())
    }

    /// Create a global variable.
    pub fn global(
        name: impl Into<String>,
        offset: impl Into<UniverseOffset>,
        r#type: impl Into<Arc<Value>>,
    ) -> Value {
        Value::Elim(
            Head::Global(name.into(), offset.into()),
            Vec::new(),
            r#type.into(),
        )
    }

    /// Create a local variable.
    pub fn local(level: impl Into<LocalLevel>, r#type: impl Into<Arc<Value>>) -> Value {
        Value::Elim(Head::Local(level.into()), Vec::new(), r#type.into())
    }
}

/// The head of an elimination.
#[derive(Clone, Debug)]
pub enum Head {
    /// Global variables.
    Global(String, UniverseOffset),
    /// Local variables.
    Local(LocalLevel),
}

/// An eliminator, to be used in the spine of an elimination.
#[derive(Clone, Debug)]
pub enum Elim {
    /// Record eliminators (field access).
    Record(String),
    /// Function eliminatiors (function application).
    Function(Arc<Value>, Arc<Value>),
}

/// Closure, which captures a local environment.
#[derive(Clone, Debug)]
pub struct Closure {
    pub universe_offset: UniverseOffset,
    pub values: Locals<Arc<Value>>,
    pub term: Arc<Term>,
}

impl Closure {
    pub fn new(
        universe_offset: UniverseOffset,
        values: Locals<Arc<Value>>,
        term: Arc<Term>,
    ) -> Closure {
        Closure {
            universe_offset,
            values,
            term,
        }
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

        entries.insert(
            "Type".to_owned(),
            (
                Arc::new(Term::universe(1)),
                Some(Arc::new(Term::universe(0))),
            ),
        );
        entries.insert("Bool".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("U8".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("U16".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("U32".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("U64".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("S8".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("S16".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("S32".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("S64".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("F32".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("F64".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("Char".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("String".to_owned(), (Arc::new(Term::global("Type")), None));
        entries.insert("true".to_owned(), (Arc::new(Term::global("Bool")), None));
        entries.insert("false".to_owned(), (Arc::new(Term::global("Bool")), None));
        entries.insert(
            "Array".to_owned(),
            (
                Arc::new(Term::FunctionType(
                    Arc::new(Term::Global("U32".to_owned())),
                    Arc::new(Term::FunctionType(
                        Arc::new(Term::universe(0)),
                        Arc::new(Term::universe(0)),
                    )),
                )),
                None,
            ),
        );
        entries.insert(
            "List".to_owned(),
            (
                Arc::new(Term::FunctionType(
                    Arc::new(Term::universe(0)),
                    Arc::new(Term::universe(0)),
                )),
                None,
            ),
        );

        Globals::new(entries)
    }
}

/// An index into the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalIndex(u32);

/// An level into the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalLevel(u32);

/// The size of the local environment.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalSize(u32);

impl LocalSize {
    /// Return the level of the next variable to be added to the environment.
    pub fn next_level(self) -> LocalLevel {
        LocalLevel(self.0)
    }

    /// Convert a variable level to a variable index in the current environment.
    pub fn index(self, level: LocalLevel) -> LocalIndex {
        LocalIndex(self.0 - (level.0 + 1)) // FIXME: Check for over/underflow?
    }
}

/// A local environment.
#[derive(Clone, Debug)]
pub struct Locals<Entry> {
    /// The local values that are currently defined in the environment.
    values: Vec<Entry>,
}

impl<Entry> Locals<Entry> {
    /// Create a new local environment.
    pub fn new() -> Locals<Entry> {
        Locals { values: Vec::new() }
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
        self.values.push(entry);
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) {
        self.values.pop();
    }

    /// Pop a number of entries off the environment.
    pub fn pop_many(&mut self, count: usize) {
        crate::pop_many(&mut self.values, count);
    }

    /// Clear the entries from the environment.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

pub trait HasType {
    fn r#type() -> Arc<Value>;
}

macro_rules! impl_has_type {
    ($Self:ty, $term:expr) => {
        impl HasType for $Self {
            fn r#type() -> Arc<Value> {
                Arc::new($term)
            }
        }
    };
}

impl_has_type!(bool, Value::global("Bool", 0, Value::universe(0)));
impl_has_type!(u8, Value::global("U8", 0, Value::universe(0)));
impl_has_type!(u16, Value::global("U16", 0, Value::universe(0)));
impl_has_type!(u32, Value::global("U32", 0, Value::universe(0)));
impl_has_type!(u64, Value::global("U64", 0, Value::universe(0)));
impl_has_type!(i8, Value::global("S8", 0, Value::universe(0)));
impl_has_type!(i16, Value::global("S16", 0, Value::universe(0)));
impl_has_type!(i32, Value::global("S32", 0, Value::universe(0)));
impl_has_type!(i64, Value::global("S64", 0, Value::universe(0)));
impl_has_type!(f32, Value::global("F32", 0, Value::universe(0)));
impl_has_type!(f64, Value::global("F64", 0, Value::universe(0)));
impl_has_type!(char, Value::global("Char", 0, Value::universe(0)));
impl_has_type!(String, Value::global("String", 0, Value::universe(0)));
impl_has_type!(str, Value::global("String", 0, Value::universe(0)));

impl<T: HasType> HasType for Vec<T> {
    fn r#type() -> Arc<Value> {
        Arc::new(Value::Elim(
            Head::Global("List".to_owned(), UniverseOffset(0)),
            vec![Elim::Function(T::r#type(), Arc::new(Value::universe(0)))],
            Arc::new(Value::universe(0)),
        ))
    }
}

macro_rules! impl_has_type_array {
    ($($len:expr),*) => {
        $(impl<T: HasType> HasType for [T; $len] {
            fn r#type() -> Arc<Value> {
                Arc::new(Value::Elim(
                    Head::Global("Array".to_owned(), UniverseOffset(0)),
                    vec![
                        Elim::Function(
                            Arc::new(Value::Constant(Constant::U32($len as u32))),
                            Arc::new(Value::global("U32", 0, Value::universe(0))),
                        ),
                        Elim::Function(T::r#type(), Arc::new(Value::universe(0))),
                    ],
                    Arc::new(Value::universe(0)),
                ))
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
                match term {
                    $p => $term,
                    _ => Err(()),
                }
            }
        }
    };
}

impl_try_from_term!(bool, |Term::Global(name)| match name.as_str() {
    "true" => Ok(true),
    "false" => Ok(false),
    _ => Err(()),
});
impl_try_from_term!(u8, |Term::Constant(Constant::U8(value))| Ok(*value));
impl_try_from_term!(u16, |Term::Constant(Constant::U16(value))| Ok(*value));
impl_try_from_term!(u32, |Term::Constant(Constant::U32(value))| Ok(*value));
impl_try_from_term!(u64, |Term::Constant(Constant::U64(value))| Ok(*value));
impl_try_from_term!(i8, |Term::Constant(Constant::S8(value))| Ok(*value));
impl_try_from_term!(i16, |Term::Constant(Constant::S16(value))| Ok(*value));
impl_try_from_term!(i32, |Term::Constant(Constant::S32(value))| Ok(*value));
impl_try_from_term!(i64, |Term::Constant(Constant::S64(value))| Ok(*value));
impl_try_from_term!(f32, |Term::Constant(Constant::F32(value))| Ok(*value));
impl_try_from_term!(f64, |Term::Constant(Constant::F64(value))| Ok(*value));
impl_try_from_term!(char, |Term::Constant(Constant::Char(value))| Ok(*value));
impl_try_from_term!(String, |Term::Constant(Constant::String(value))| Ok(
    value.clone(),
));

impl<T: TryFromTerm> TryFromTerm for Vec<T> {
    type Error = ();

    fn try_from_term(term: &Term) -> Result<Vec<T>, ()> {
        match term {
            Term::Sequence(entry_terms) => entry_terms
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
                match term {
                    Term::Sequence(entry_terms) if entry_terms.len() == $len => {
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
    ($Self:ty, |$p:pat| $term:expr) => {
        impl ToTerm for $Self {
            fn to_term(&self) -> Term {
                let $p = self;
                $term
            }
        }
    };
}

impl_to_term!(bool, |value| Term::Global(match value {
    true => "true".to_owned(),
    false => "false".to_owned(),
}));
impl_to_term!(u8, |value| Term::Constant(Constant::U8(*value)));
impl_to_term!(u16, |value| Term::Constant(Constant::U16(*value)));
impl_to_term!(u32, |value| Term::Constant(Constant::U32(*value)));
impl_to_term!(u64, |value| Term::Constant(Constant::U64(*value)));
impl_to_term!(i8, |value| Term::Constant(Constant::S8(*value)));
impl_to_term!(i16, |value| Term::Constant(Constant::S16(*value)));
impl_to_term!(i32, |value| Term::Constant(Constant::S32(*value)));
impl_to_term!(i64, |value| Term::Constant(Constant::S64(*value)));
impl_to_term!(f32, |value| Term::Constant(Constant::F32(*value)));
impl_to_term!(f64, |value| Term::Constant(Constant::F64(*value)));
impl_to_term!(char, |value| Term::Constant(Constant::Char(*value)));
impl_to_term!(String, |value| Term::Constant(Constant::String(
    value.clone()
)));
impl_to_term!(str, |value| Term::Constant(Constant::String(
    value.to_owned()
)));

impl<T: ToTerm> ToTerm for Vec<T> {
    fn to_term(&self) -> Term {
        Term::Sequence(
            self.iter()
                .map(|entry_term| Arc::new(T::to_term(entry_term)))
                .collect(),
        )
    }
}

macro_rules! impl_to_term_array {
    ($($len:expr),*) => {
        $(impl<T: ToTerm> ToTerm for [T; $len] {
            fn to_term(&self) -> Term {
                Term::Sequence(
                    self.iter()
                        .map(|entry_term| Arc::new(T::to_term(entry_term)))
                        .collect(),
                )
            }
        })*
    };
}

impl_to_term_array!(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32
);
