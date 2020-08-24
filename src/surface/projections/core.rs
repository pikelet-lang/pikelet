//! Elaborates the surface language into the core language.

use std::str::FromStr;
use std::sync::Arc;

use crate::core;
use crate::surface::{Literal, Term};

/// The state of the elaborator.
pub struct State<'me> {
    globals: &'me core::Globals,
    locals: core::Locals,
    pub errors: Vec<TypeError>,
}

impl<'me> State<'me> {
    /// Construct a new elaborator state.
    pub fn new(globals: &'me core::Globals) -> State<'me> {
        State {
            globals,
            locals: core::Locals::new(),
            errors: Vec::new(),
        }
    }

    /// Report an error.
    pub fn report(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Reset the elaborator state while retaining existing allocations.
    pub fn clear(&mut self) {
        self.errors.clear();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    MaximumUniverseLevelReached,
    UnboundName(String),
    DuplicateNamesInRecordTerm(Vec<String>),
    DuplicateNamesInRecordType(Vec<String>),
    MissingNamesInRecordTerm(Vec<String>),
    UnexpectedNamesInRecordTerm(Vec<String>),
    InvalidNumberLiteral,
    InvalidCharLiteral,
    InvalidStringLiteral,
    NoLiteralConversion(core::Value),
    AmbiguousLiteral,
    AmbiguousSequence,
    UnexpectedSequenceLength(usize, Arc<core::Value>),
    ExpectedType(core::Value),
    MismatchedTypes(core::Value, core::Value),
}

/// Check that a term is a universe and return its level.
pub fn check_type<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
) -> (core::Term, Option<core::UniverseLevel>) {
    match synth_term(state, term) {
        (term, core::Value::Universe(level)) => (term, Some(level)),
        (_, r#type) => {
            state.report(TypeError::ExpectedType(r#type));
            (core::Term::Error, None)
        }
    }
}

/// Check that a term matches the expected type.
pub fn check_term<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
    expected_type: &core::Value,
) -> core::Term {
    match (term, expected_type) {
        (Term::Literal(_, literal), _) => check_literal(state, literal, expected_type),
        (Term::Sequence(_, entry_terms), core::Value::ArrayType(len, core_entry_type)) => {
            let core_entry_terms = entry_terms
                .iter()
                .map(|entry_term| Arc::new(check_term(state, entry_term, core_entry_type)))
                .collect();

            match **len {
                core::Value::Constant(core::Constant::U32(len))
                    if len as usize == entry_terms.len() =>
                {
                    core::Term::Sequence(core_entry_terms)
                }
                _ => {
                    state.report(TypeError::UnexpectedSequenceLength(
                        entry_terms.len(),
                        len.clone(),
                    ));

                    core::Term::Error
                }
            }
        }
        (Term::Sequence(_, entry_terms), core::Value::ListType(core_entry_type)) => {
            core::Term::Sequence(
                entry_terms
                    .iter()
                    .map(|entry_term| Arc::new(check_term(state, entry_term, core_entry_type)))
                    .collect(),
            )
        }
        (Term::RecordTerm(_, term_entries), core::Value::RecordType(core_type_entries)) => {
            use std::collections::btree_map::Entry;
            use std::collections::BTreeMap;

            let mut duplicate_names = Vec::new();
            let mut missing_names = Vec::new();
            let mut term_entries =
                (term_entries.iter()).fold(BTreeMap::new(), |mut acc, (name, term)| {
                    match acc.entry(name.as_ref()) {
                        Entry::Vacant(entry) => drop(entry.insert(term)),
                        Entry::Occupied(_) => duplicate_names.push(name.as_ref().to_owned()),
                    }
                    acc
                });
            let mut core_term_entries = BTreeMap::new();

            for (name, core_type) in core_type_entries {
                match term_entries.remove(&name.as_str()) {
                    Some(term) => {
                        let core_term = Arc::new(check_term(state, term, core_type));
                        core_term_entries.insert(name.clone(), core_term);
                    }
                    None => missing_names.push(name.clone()),
                }
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordTerm(duplicate_names));
            }
            if !missing_names.is_empty() {
                state.report(TypeError::MissingNamesInRecordTerm(missing_names));
            }
            if !term_entries.is_empty() {
                let unexpected_names = (term_entries.into_iter())
                    .map(|(name, _)| name.to_owned())
                    .collect();
                state.report(TypeError::UnexpectedNamesInRecordTerm(unexpected_names));
            }

            core::Term::RecordTerm(core_term_entries)
        }
        (term, expected_type) => match synth_term(state, term) {
            (term, ty) if core::semantics::is_subtype(&ty, expected_type) => term,
            (_, ty) => {
                state.report(TypeError::MismatchedTypes(ty, expected_type.clone()));
                core::Term::Error
            }
        },
    }
}

/// Synthesize the type of a term.
pub fn synth_term<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
) -> (core::Term, core::Value) {
    use std::collections::{BTreeMap, BTreeSet};

    match term {
        Term::Name(_, name) => match state.globals.get(name.as_ref()) {
            Some((r#type, _)) => (
                core::Term::Global(name.as_ref().to_owned()).lift(state.locals.universe_offset()),
                core::semantics::eval_term(state.globals, &mut state.locals, r#type),
            ),
            None => {
                state.report(TypeError::UnboundName(name.as_ref().to_owned()));
                (core::Term::Error, core::Value::Error)
            }
        },
        Term::Ann(term, r#type) => {
            let (core_type, _) = check_type(state, r#type);
            let core_type_value =
                core::semantics::eval_term(state.globals, &mut state.locals, &core_type);
            let core_term = check_term(state, term, &core_type_value);
            (
                core::Term::Ann(Arc::new(core_term), Arc::new(core_type.clone())),
                core_type_value,
            )
        }
        Term::Literal(_, literal) => synth_literal(state, literal),
        Term::Sequence(_, _) => {
            state.report(TypeError::AmbiguousSequence);
            (core::Term::Error, core::Value::Error)
        }
        Term::RecordTerm(_, term_entries) => {
            let mut duplicate_names = Vec::new();
            let mut core_term_entries = BTreeMap::new();
            let mut core_type_entries = Vec::new();

            for (name, term) in term_entries {
                use std::collections::btree_map::Entry;

                let (term, r#type) = synth_term(state, term);

                match core_term_entries.entry(name.as_ref().to_owned()) {
                    Entry::Occupied(_) => duplicate_names.push(name.as_ref().to_owned()),
                    Entry::Vacant(entry) => {
                        entry.insert(Arc::new(term));
                        core_type_entries.push((name.as_ref().to_owned(), Arc::new(r#type)));
                    }
                }
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordTerm(duplicate_names));
            }

            (
                core::Term::RecordTerm(core_term_entries),
                core::Value::RecordType(core_type_entries),
            )
        }
        Term::RecordType(_, type_entries) => {
            let mut max_level = core::UniverseLevel(0);
            let mut duplicate_names = Vec::new();
            let mut seen_names = BTreeSet::new();
            let mut core_type_entries = Vec::new();

            for (name, r#type) in type_entries {
                if seen_names.insert(name.as_ref()) {
                    let (core_type, level) = check_type(state, r#type);
                    max_level = match level {
                        Some(level) => std::cmp::max(max_level, level),
                        None => return (core::Term::Error, core::Value::Error),
                    };
                    core_type_entries.push((name.as_ref().to_owned(), Arc::new(core_type)));
                } else {
                    duplicate_names.push(name.as_ref().to_owned());
                    check_type(state, r#type);
                }
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordType(duplicate_names));
            }

            (
                core::Term::RecordType(core_type_entries),
                core::Value::Universe(max_level),
            )
        }
        Term::ArrayType(_, len, entry_type) => {
            let u32_type = core::Value::global("U32", 0, core::Value::universe(0));
            let core_len = Arc::new(check_term(state, len, &u32_type));
            let (core_entry_type, level) = check_type(state, entry_type);

            match level {
                Some(level) => (
                    core::Term::ArrayType(core_len, Arc::new(core_entry_type)),
                    core::Value::Universe(level),
                ),
                None => (core::Term::Error, core::Value::Error),
            }
        }
        Term::ListType(_, entry_type) => {
            let (core_entry_type, level) = check_type(state, entry_type);

            match level {
                Some(level) => (
                    core::Term::ListType(Arc::new(core_entry_type)),
                    core::Value::Universe(level),
                ),
                None => (core::Term::Error, core::Value::Error),
            }
        }
        Term::Lift(_, term, offset) => {
            match state.locals.universe_offset() + core::UniverseOffset(*offset) {
                Some(lifted_level) => {
                    let previous_level = state.locals.set_universe_offset(lifted_level);
                    let (core_term, r#type) = synth_term(state, term);
                    state.locals.set_universe_offset(previous_level);
                    (core_term, r#type)
                }
                None => {
                    state.report(TypeError::MaximumUniverseLevelReached);
                    (core::Term::Error, core::Value::Error)
                }
            }
        }
        Term::Error(_) => (core::Term::Error, core::Value::Error),
    }
}

/// Check that a literal matches the expected type.
fn check_literal<S: AsRef<str>>(
    state: &mut State<'_>,
    literal: &Literal<S>,
    expected_type: &core::Value,
) -> core::Term {
    if let core::Value::Neutral(core::Neutral::Global(name, _), _) = expected_type {
        match (literal, name.as_ref()) {
            (Literal::Number(data), "U8") => parse_number(state, data, core::Constant::U8),
            (Literal::Number(data), "U16") => parse_number(state, data, core::Constant::U16),
            (Literal::Number(data), "U32") => parse_number(state, data, core::Constant::U32),
            (Literal::Number(data), "U64") => parse_number(state, data, core::Constant::U64),
            (Literal::Number(data), "S8") => parse_number(state, data, core::Constant::S8),
            (Literal::Number(data), "S16") => parse_number(state, data, core::Constant::S16),
            (Literal::Number(data), "S32") => parse_number(state, data, core::Constant::S32),
            (Literal::Number(data), "S64") => parse_number(state, data, core::Constant::S64),
            (Literal::Number(data), "F32") => parse_number(state, data, core::Constant::F32),
            (Literal::Number(data), "F64") => parse_number(state, data, core::Constant::F64),
            (Literal::Char(data), "Char") => parse_char(state, data),
            (Literal::String(data), "String") => parse_string(state, data),
            (_, _) => {
                state.report(TypeError::NoLiteralConversion(expected_type.clone()));
                core::Term::Error
            }
        }
    } else {
        state.report(TypeError::NoLiteralConversion(expected_type.clone()));
        core::Term::Error
    }
}

/// Check that a literal matches the expected type.
fn synth_literal<S: AsRef<str>>(
    state: &mut State<'_>,
    literal: &Literal<S>,
) -> (core::Term, core::Value) {
    match literal {
        Literal::Number(_) => {
            state.report(TypeError::AmbiguousLiteral);
            (core::Term::Error, core::Value::Error)
        }
        Literal::Char(data) => (
            parse_char(state, data),
            core::Value::global("Char", 0, core::Value::universe(0)),
        ),
        Literal::String(data) => (
            parse_string(state, data),
            core::Value::global("String", 0, core::Value::universe(0)),
        ),
    }
}

fn parse_number<S: AsRef<str>, T: FromStr>(
    state: &mut State<'_>,
    data: &S,
    f: impl Fn(T) -> core::Constant,
) -> core::Term {
    // TODO: improve parser (eg. numeric separators, positive sign)
    match data.as_ref().parse() {
        Ok(value) => core::Term::Constant(f(value)),
        Err(_) => {
            state.report(TypeError::InvalidNumberLiteral);
            core::Term::Error
        }
    }
}

fn parse_char<S: AsRef<str>>(state: &mut State<'_>, data: &S) -> core::Term {
    // TODO: Improve parser (escapes)
    match data.as_ref().chars().nth(1) {
        Some(value) => core::Term::Constant(core::Constant::Char(value)),
        None => {
            state.report(TypeError::InvalidCharLiteral);
            core::Term::Error
        }
    }
}

fn parse_string<S: AsRef<str>>(state: &mut State<'_>, data: &S) -> core::Term {
    // TODO: Improve parser (escapes)
    match data.as_ref().get(1..data.as_ref().len() - 1) {
        Some(value) => core::Term::Constant(core::Constant::String(value.to_owned())),
        None => {
            state.report(TypeError::InvalidStringLiteral);
            core::Term::Error
        }
    }
}
