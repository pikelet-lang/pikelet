//! Bidirectional type checker for the core language.
//!
//! This is used to validate that terms are well-formed.

use std::sync::Arc;

use crate::core::semantics;
use crate::core::{Constant, Globals, Locals, Term, UniverseLevel, UniverseOffset, Value};

/// The state of the type checker.
pub struct State<'me> {
    globals: &'me Globals,
    locals: Locals,
    pub errors: Vec<TypeError>,
}

impl<'me> State<'me> {
    /// Construct a new type checker state.
    pub fn new(globals: &'me Globals) -> State<'me> {
        State {
            globals,
            locals: Locals::new(),
            errors: Vec::new(),
        }
    }

    /// Report an error.
    pub fn report(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Reset the type checker state while retaining existing allocations.
    pub fn clear(&mut self) {
        self.errors.clear();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    MaximumUniverseLevelReached,
    UnboundName(String),
    DuplicateNamesInRecordType(Vec<String>),
    MissingNamesInRecordTerm(Vec<String>),
    UnexpectedNamesInRecordTerm(Vec<String>),
    FieldNotFoundInRecord(String),
    ExpectedRecord(Value),
    AmbiguousSequence,
    UnexpectedSequenceLength(usize, Arc<Value>),
    ExpectedType(Value),
    MismatchedTypes(Value, Value),
}

/// Check that a term is a universe and return its level.
pub fn check_type(state: &mut State<'_>, term: &Term) -> Option<UniverseLevel> {
    match synth_term(state, term) {
        Value::Universe(level) => Some(level),
        r#type => {
            state.report(TypeError::ExpectedType(r#type));
            None
        }
    }
}

/// Check that a term matches the expected type.
pub fn check_term(state: &mut State<'_>, term: &Term, expected_type: &Value) {
    match (term, expected_type) {
        (Term::Sequence(entry_terms), Value::ArrayType(len, entry_type)) => {
            for entry_term in entry_terms {
                check_term(state, entry_term, entry_type);
            }
            match **len {
                Value::Constant(Constant::U32(len)) if len as usize == entry_terms.len() => {}
                _ => state.report(TypeError::UnexpectedSequenceLength(
                    entry_terms.len(),
                    len.clone(),
                )),
            }
        }
        (Term::Sequence(entry_terms), Value::ListType(entry_type)) => {
            for entry_term in entry_terms {
                check_term(state, entry_term, entry_type);
            }
        }
        (Term::RecordTerm(term_entries), Value::RecordType(type_entries)) => {
            let mut missing_names = Vec::new();
            let mut term_entries = term_entries.clone();

            for (name, r#type) in type_entries {
                match term_entries.remove(name) {
                    Some(term) => check_term(state, &term, r#type),
                    None => missing_names.push(name.clone()),
                }
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
        }
        (term, expected_type) => match synth_term(state, term) {
            ty if semantics::is_subtype(&ty, expected_type) => {}
            ty => state.report(TypeError::MismatchedTypes(ty, expected_type.clone())),
        },
    }
}

/// Synthesize the type of a term.
pub fn synth_term(state: &mut State<'_>, term: &Term) -> Value {
    match term {
        Term::Universe(level) => match *level + UniverseOffset(1) {
            Some(level) => Value::universe(level),
            None => {
                state.report(TypeError::MaximumUniverseLevelReached);
                Value::Error
            }
        },
        Term::Global(name) => match state.globals.get(name) {
            Some((r#type, _)) => semantics::eval_term(state.globals, &mut state.locals, r#type),
            None => {
                state.report(TypeError::UnboundName(name.to_owned()));
                Value::Error
            }
        },
        Term::Constant(constant) => match constant {
            Constant::U8(_) => Value::global("U8", 0, Value::universe(0)),
            Constant::U16(_) => Value::global("U16", 0, Value::universe(0)),
            Constant::U32(_) => Value::global("U32", 0, Value::universe(0)),
            Constant::U64(_) => Value::global("U64", 0, Value::universe(0)),
            Constant::S8(_) => Value::global("S8", 0, Value::universe(0)),
            Constant::S16(_) => Value::global("S16", 0, Value::universe(0)),
            Constant::S32(_) => Value::global("S32", 0, Value::universe(0)),
            Constant::S64(_) => Value::global("S64", 0, Value::universe(0)),
            Constant::F32(_) => Value::global("F32", 0, Value::universe(0)),
            Constant::F64(_) => Value::global("F64", 0, Value::universe(0)),
            Constant::Char(_) => Value::global("Char", 0, Value::universe(0)),
            Constant::String(_) => Value::global("String", 0, Value::universe(0)),
        },
        Term::Sequence(_) => {
            state.report(TypeError::AmbiguousSequence);
            Value::Error
        }
        Term::Ann(term, r#type) => {
            check_type(state, r#type);
            let r#type = semantics::eval_term(state.globals, &mut state.locals, r#type);
            check_term(state, term, &r#type);
            r#type
        }
        Term::RecordTerm(term_entries) => {
            let type_entries = term_entries
                .iter()
                .map(|(name, term)| (name.clone(), Arc::new(synth_term(state, term))))
                .collect();

            Value::RecordType(type_entries)
        }
        Term::RecordType(type_entries) => {
            use std::collections::BTreeSet;

            let mut max_level = UniverseLevel(0);
            let mut duplicate_names = Vec::new();
            let mut seen_names = BTreeSet::new();

            for (name, r#type) in type_entries {
                if !seen_names.insert(name) {
                    duplicate_names.push(name.clone());
                }
                max_level = match check_type(state, r#type) {
                    Some(level) => std::cmp::max(max_level, level),
                    None => return Value::Error,
                };
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordType(duplicate_names));
            }

            Value::Universe(max_level)
        }
        Term::RecordElim(head, name) => match synth_term(state, head) {
            Value::RecordType(type_entries) => {
                match type_entries.iter().find(|(n, _)| n == name) {
                    Some((_, r#type)) => (**r#type).clone(), // TODO: return `Arc<Value>`?
                    None => {
                        state.report(TypeError::FieldNotFoundInRecord(name.clone()));
                        Value::Error
                    }
                }
            }
            head_type => {
                state.report(TypeError::ExpectedRecord(head_type));
                Value::Error
            }
        },
        Term::ArrayType(len, entry_type) => {
            let u32_type = Value::global("U32", 0, Value::universe(0));
            check_term(state, len, &u32_type);
            let level = check_type(state, entry_type);

            match level {
                Some(level) => Value::Universe(level),
                None => Value::Error,
            }
        }
        Term::ListType(entry_type) => {
            let level = check_type(state, entry_type);

            match level {
                Some(level) => Value::Universe(level),
                None => Value::Error,
            }
        }
        Term::Lift(term, shift) => match state.locals.universe_offset() + *shift {
            Some(lifted_level) => {
                let offset = state.locals.set_universe_offset(lifted_level);
                let r#type = synth_term(state, term);
                state.locals.set_universe_offset(offset);
                r#type
            }
            None => {
                state.report(TypeError::MaximumUniverseLevelReached);
                Value::Error
            }
        },
        Term::Error => Value::Error,
    }
}
