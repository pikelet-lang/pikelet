//! Marshalling API between Rust types and Pikelet's core language.

use std::sync::Arc;

use crate::lang::core::{Constant, Term, TermData};

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

impl_has_type!(bool, Term::generated(TermData::Global("Bool".to_owned())));
impl_has_type!(u8, Term::generated(TermData::Global("U8".to_owned())));
impl_has_type!(u16, Term::generated(TermData::Global("U16".to_owned())));
impl_has_type!(u32, Term::generated(TermData::Global("U32".to_owned())));
impl_has_type!(u64, Term::generated(TermData::Global("U64".to_owned())));
impl_has_type!(i8, Term::generated(TermData::Global("S8".to_owned())));
impl_has_type!(i16, Term::generated(TermData::Global("S16".to_owned())));
impl_has_type!(i32, Term::generated(TermData::Global("S32".to_owned())));
impl_has_type!(i64, Term::generated(TermData::Global("S64".to_owned())));
impl_has_type!(f32, Term::generated(TermData::Global("F32".to_owned())));
impl_has_type!(f64, Term::generated(TermData::Global("F64".to_owned())));
impl_has_type!(char, Term::generated(TermData::Global("Char".to_owned())));
impl_has_type!(
    String,
    Term::generated(TermData::Global("String".to_owned()))
);
impl_has_type!(str, Term::generated(TermData::Global("String".to_owned())));

impl<T: HasType> HasType for Vec<T> {
    fn r#type() -> Arc<Term> {
        Arc::new(Term::generated(TermData::FunctionElim(
            Arc::new(Term::generated(TermData::Global("List".to_owned()))),
            T::r#type(),
        )))
    }
}

impl<T: HasType, const LEN: usize /* TODO: u32 */> HasType for [T; LEN] {
    fn r#type() -> Arc<Term> {
        Arc::new(Term::generated(TermData::FunctionElim(
            Arc::new(Term::generated(TermData::FunctionElim(
                Arc::new(Term::generated(TermData::Global("List".to_owned()))),
                Arc::new(Term::generated(TermData::from(Constant::U32(LEN as u32)))), // FIXME: this could overflow!
            ))),
            T::r#type(),
        )))
    }
}

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
            TermData::ListTerm(entry_terms) => entry_terms
                .iter()
                .map(|entry_term| T::try_from_term(entry_term).map_err(|_| ()))
                .collect::<Result<Vec<_>, ()>>(),
            _ => Err(()),
        }
    }
}

impl<T: TryFromTerm + Sized, const LEN: usize> TryFromTerm for [T; LEN] {
    type Error = ();

    fn try_from_term(term: &Term) -> Result<[T; LEN], ()> {
        match &term.data {
            TermData::ArrayTerm(entry_terms) if entry_terms.len() == LEN => {
                use std::mem::MaybeUninit;

                let mut entries: [MaybeUninit<T>; LEN] =
                    unsafe { MaybeUninit::uninit().assume_init() };
                for (i, entry_term) in entry_terms.iter().enumerate() {
                    entries[i] = MaybeUninit::new(T::try_from_term(entry_term).map_err(|_| ())?);
                }

                // NOTE: We'd prefer to do the following:
                //
                // ```
                // Ok(unsafe { std::mem::transmute::<_, [T; LEN]>(entries) })
                // ```
                //
                // Sadly we run into the following issue: https://github.com/rust-lang/rust/issues/61956
                // For this reason we need to do the following (hideous) workaround:

                let ptr = &mut entries as *mut _ as *mut [T; LEN];
                let result = unsafe { ptr.read() };
                core::mem::forget(entries);
                Ok(result)
            }
            _ => Err(()),
        }
    }
}

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
                Term::generated($term_data)
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
        Term::generated(TermData::ListTerm(
            self.iter().map(T::to_term).map(Arc::new).collect(),
        ))
    }
}

impl<T: ToTerm, const LEN: usize> ToTerm for [T; LEN] {
    fn to_term(&self) -> Term {
        Term::generated(TermData::ArrayTerm(
            self.iter().map(T::to_term).map(Arc::new).collect(),
        ))
    }
}
