//! Elaborates the surface language into the core language.

use std::ops::Range;
use std::str::FromStr;
use std::sync::Arc;

use crate::core;
use crate::surface::{Literal, Term};

/// The state of the elaborator.
pub struct State<'me> {
    /// Global definition environment.
    globals: &'me core::Globals,
    /// The current universe offset.
    universe_offset: core::UniverseOffset,
    /// Substitutions from the user-defined names to the level in which they were bound.
    names_to_levels: Vec<(String, core::LocalLevel)>,
    /// Local name environment (used for pretty printing).
    names: core::Locals<String>,
    /// Local type environment (used for getting the types of local variables).
    types: core::Locals<Arc<core::Value>>,
    /// Local value environment (used for evaluation).
    values: core::Locals<Arc<core::Value>>,
    /// The errors accumulated during elaboration.
    errors: Vec<TypeError>,
}

impl<'me> State<'me> {
    /// Construct a new elaborator state.
    pub fn new(globals: &'me core::Globals) -> State<'me> {
        State {
            globals,
            universe_offset: core::UniverseOffset(0),
            names_to_levels: Vec::new(),
            names: core::Locals::new(),
            types: core::Locals::new(),
            values: core::Locals::new(),
            errors: Vec::new(),
        }
    }

    /// Get the next level to be used for a local entry.
    fn next_level(&self) -> core::LocalLevel {
        self.values.size().next_level()
    }

    /// Get a local entry.
    fn get_local(&self, name: &str) -> Option<(core::LocalIndex, &Arc<core::Value>)> {
        let (_, level) = self.names_to_levels.iter().rev().find(|(n, _)| n == name)?;
        let index = self.values.size().index(*level);
        let ty = self.types.get(index)?;
        Some((index, ty))
    }

    /// Push a local entry.
    fn push_local(&mut self, name: String, value: Arc<core::Value>, r#type: Arc<core::Value>) {
        self.names_to_levels.push((name.clone(), self.next_level()));
        self.names.push(name);
        self.types.push(r#type);
        self.values.push(value);
    }

    /// Push a local parameter.
    fn push_param(&mut self, name: String, r#type: Arc<core::Value>) -> Arc<core::Value> {
        let value = Arc::new(core::Value::local(self.next_level(), r#type.clone()));
        self.push_local(name, value.clone(), r#type);
        value
    }

    // /// Pop a local entry.
    // fn pop_local(&mut self) {
    //     self.names_to_levels.pop();
    //     self.names.pop();
    //     self.types.pop();
    //     self.values.pop();
    // }

    /// Pop the given number of local entries.
    fn pop_many_locals(&mut self, count: usize) {
        self.names_to_levels
            .truncate(self.names_to_levels.len().saturating_sub(count));
        self.names.pop_many(count);
        self.types.pop_many(count);
        self.values.pop_many(count);
    }

    /// Report an error.
    fn report(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Drain the current errors.
    pub fn drain_errors(&mut self) -> std::vec::Drain<TypeError> {
        self.errors.drain(..)
    }

    /// Reset the elaborator state while retaining existing allocations.
    pub fn clear(&mut self) {
        self.universe_offset = core::UniverseOffset(0);
        self.names_to_levels.clear();
        self.types.clear();
        self.values.clear();
        self.errors.clear();
    }

    /// Evaluate a term using the current state of the elaborator.
    pub fn eval_term(&mut self, term: &core::Term) -> Arc<core::Value> {
        core::semantics::eval_term(self.globals, self.universe_offset, &mut self.values, term)
    }

    /// Eliminate a closure.
    fn eval_closure_elim(
        &self,
        closure: &core::Closure,
        argument: Arc<core::Value>,
    ) -> Arc<core::Value> {
        core::semantics::eval_closure_elim(self.globals, closure, argument)
    }

    /// Normalize a term using the current state of the elaborator.
    pub fn normalize_term(&mut self, term: &core::Term, r#type: &core::Value) -> core::Term {
        core::semantics::normalize_term(
            self.globals,
            self.universe_offset,
            &mut self.values,
            term,
            r#type,
        )
    }

    /// Read back a type using the current state of the elaborator.
    pub fn read_back_type(&mut self, r#type: &core::Value) -> core::Term {
        core::semantics::read_back_type(self.globals, self.values.size(), r#type)
    }

    /// Check if `value0` is a subtype of `value1`.
    pub fn is_subtype(&self, value0: &core::Value, value1: &core::Value) -> bool {
        core::semantics::is_subtype(self.globals, self.values.size(), value0, value1)
    }

    /// Delaborate a `core::Term` into a `surface::Term`.
    pub fn delaborate_term(&mut self, core_term: &core::Term) -> Term<String> {
        core::projections::surface::delaborate_term(
            &mut core::projections::surface::State::new(&mut self.names),
            &core_term,
        )
    }
}

#[derive(Clone, Debug)]
pub enum TypeError {
    MaximumUniverseLevelReached {
        span: Range<usize>,
    },
    UnboundName {
        span: Range<usize>,
        name: String,
    },
    DuplicateNamesInRecordTerm {
        span: Range<usize>,
        duplicate_names: Vec<(Range<usize>, String)>,
    },
    DuplicateNamesInRecordType {
        span: Range<usize>,
        duplicate_names: Vec<(Range<usize>, String)>,
    },
    MissingNamesInRecordTerm {
        span: Range<usize>,
        missing_names: Vec<String>,
    },
    UnexpectedNamesInRecordTerm {
        span: Range<usize>,
        unexpected_names: Vec<(Range<usize>, String)>,
    },
    AmbiguousRecordTerm {
        span: Range<usize>,
    },
    FieldNotFound {
        span: Range<usize>,
        expected_field_name: String,
        head_type: Arc<core::Value>,
    },
    TooManyParametersForFunctionTerm {
        excess_parameters: Vec<String>,
        expected_type: Arc<core::Value>,
    },
    AmbiguousFunctionTerm {
        span: Range<usize>,
    },
    NotAFunction {
        span: Range<usize>,
        head_type: Arc<core::Value>,
    },
    InvalidNumberLiteral {
        span: Range<usize>,
    },
    InvalidCharLiteral {
        span: Range<usize>,
    },
    InvalidStringLiteral {
        span: Range<usize>,
    },
    NoLiteralConversion {
        span: Range<usize>,
        expected_type: Arc<core::Value>,
    },
    AmbiguousLiteral {
        span: Range<usize>,
    },
    AmbiguousSequence {
        span: Range<usize>,
    },
    MismatchedSequenceLength {
        span: Range<usize>,
        found_len: usize,
        expected_len: Arc<core::Value>,
    },
    NoSequenceConversion {
        span: Range<usize>,
        expected_type: Arc<core::Value>,
    },
    ExpectedType {
        span: Range<usize>,
        found_type: Arc<core::Value>,
    },
    MismatchedTypes {
        span: Range<usize>,
        found_type: Arc<core::Value>,
        expected_type: Arc<core::Value>,
    },
}

/// Check that a term is a universe and return its level.
pub fn check_type<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
) -> (core::Term, Option<core::UniverseLevel>) {
    let (core_term, r#type) = synth_term(state, term);
    match r#type.as_ref() {
        core::Value::Universe(level) => (core_term, Some(*level)),
        _ => {
            state.report(TypeError::ExpectedType {
                span: term.span(),
                found_type: r#type,
            });
            (core::Term::Error, None)
        }
    }
}

/// Check that a term matches the expected type.
pub fn check_term<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
    expected_type: &Arc<core::Value>,
) -> core::Term {
    match (term, expected_type.as_ref()) {
        (Term::Literal(_, literal), core::Value::Elim(core::Head::Global(name, _), spine, _)) => {
            use crate::core::Constant::*;

            match (literal, name.as_ref(), spine.as_slice()) {
                (Literal::Number(data), "U8", []) => parse_number(state, term.span(), data, U8),
                (Literal::Number(data), "U16", []) => parse_number(state, term.span(), data, U16),
                (Literal::Number(data), "U32", []) => parse_number(state, term.span(), data, U32),
                (Literal::Number(data), "U64", []) => parse_number(state, term.span(), data, U64),
                (Literal::Number(data), "S8", []) => parse_number(state, term.span(), data, S8),
                (Literal::Number(data), "S16", []) => parse_number(state, term.span(), data, S16),
                (Literal::Number(data), "S32", []) => parse_number(state, term.span(), data, S32),
                (Literal::Number(data), "S64", []) => parse_number(state, term.span(), data, S64),
                (Literal::Number(data), "F32", []) => parse_number(state, term.span(), data, F32),
                (Literal::Number(data), "F64", []) => parse_number(state, term.span(), data, F64),
                (Literal::Char(data), "Char", []) => parse_char(state, term.span(), data),
                (Literal::String(data), "String", []) => parse_string(state, term.span(), data),
                (_, _, _) => {
                    state.report(TypeError::NoLiteralConversion {
                        span: term.span(),
                        expected_type: expected_type.clone(),
                    });
                    core::Term::Error
                }
            }
        }
        (Term::Literal(_, _), core::Value::Error) => core::Term::Error,
        (Term::Literal(_, _), _) => {
            state.report(TypeError::NoLiteralConversion {
                span: term.span(),
                expected_type: expected_type.clone(),
            });
            core::Term::Error
        }
        (
            Term::Sequence(_, entry_terms),
            core::Value::Elim(core::Head::Global(name, _), elims, _),
        ) => match (name.as_ref(), elims.as_slice()) {
            ("Array", [core::Elim::Function(len, _), core::Elim::Function(core_entry_type, _)]) => {
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
                        state.report(TypeError::MismatchedSequenceLength {
                            span: term.span(),
                            found_len: entry_terms.len(),
                            expected_len: len.clone(),
                        });

                        core::Term::Error
                    }
                }
            }
            ("List", [core::Elim::Function(core_entry_type, _)]) => {
                let core_entry_terms = entry_terms
                    .iter()
                    .map(|entry_term| Arc::new(check_term(state, entry_term, core_entry_type)))
                    .collect();

                core::Term::Sequence(core_entry_terms)
            }
            _ => {
                state.report(TypeError::NoSequenceConversion {
                    span: term.span(),
                    expected_type: expected_type.clone(),
                });
                core::Term::Error
            }
        },
        (Term::Sequence(_, _), core::Value::Error) => core::Term::Error,
        (Term::Sequence(_, _), _) => {
            state.report(TypeError::NoSequenceConversion {
                span: term.span(),
                expected_type: expected_type.clone(),
            });
            core::Term::Error
        }
        (Term::RecordTerm(_, term_entries), _) => {
            use std::collections::btree_map::Entry;
            use std::collections::BTreeMap;

            let mut duplicate_names = Vec::new();
            let mut missing_names = Vec::new();

            let mut expected_type = expected_type.clone();
            let mut core_term_entries = BTreeMap::new();
            let mut term_entries = (term_entries.iter()).fold(
                BTreeMap::new(),
                |mut acc, (entry_name_span, entry_name, entry_term)| {
                    let span = entry_name_span.clone();
                    match acc.entry(entry_name.as_ref().to_owned()) {
                        Entry::Vacant(entry) => drop(entry.insert((span, entry_term))),
                        Entry::Occupied(entry) => duplicate_names.push((span, entry.key().clone())),
                    }
                    acc
                },
            );

            while let core::Value::RecordTypeExtend(entry_name, entry_type, rest_type) =
                expected_type.as_ref()
            {
                expected_type = match term_entries.remove(entry_name.as_str()) {
                    Some((_, term)) => {
                        let core_term = Arc::new(check_term(state, term, entry_type));
                        let core_value = state.eval_term(&core_term);
                        core_term_entries.insert(entry_name.clone(), core_term);
                        state.eval_closure_elim(rest_type, core_value)
                    }
                    None => {
                        missing_names.push(entry_name.clone());
                        state.eval_closure_elim(rest_type, Arc::new(core::Value::Error))
                    }
                };
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordTerm {
                    span: term.span(),
                    duplicate_names,
                });
            }
            if !missing_names.is_empty() {
                state.report(TypeError::MissingNamesInRecordTerm {
                    span: term.span(),
                    missing_names,
                });
            }
            if !term_entries.is_empty() {
                let unexpected_names = (term_entries.into_iter())
                    .map(|(entry_name, (entry_name_span, _))| (entry_name_span, entry_name))
                    .collect();
                state.report(TypeError::UnexpectedNamesInRecordTerm {
                    span: term.span(),
                    unexpected_names,
                });
            }

            core::Term::RecordTerm(core_term_entries)
        }
        (Term::FunctionTerm(_, param_names, body), _) => {
            let core_body = {
                let mut expected_type = expected_type;
                let mut param_names = param_names.iter();
                while let Some(param_name) = param_names.next() {
                    match expected_type.as_ref() {
                        core::Value::FunctionType(param_type, body_type) => {
                            state.push_param(param_name.as_ref().to_owned(), param_type.clone());
                            expected_type = body_type;
                        }
                        _ => {
                            state.report(TypeError::TooManyParametersForFunctionTerm {
                                excess_parameters: param_names
                                    .map(|param_name| param_name.as_ref().to_owned())
                                    .collect(),
                                expected_type: expected_type.clone(),
                            });
                            return core::Term::Error;
                        }
                    }
                }
                check_term(state, body, expected_type)
            };
            state.pop_many_locals(param_names.len());
            (param_names.iter().rev()).fold(core_body, |core_body, param_name| {
                core::Term::FunctionTerm(param_name.as_ref().to_owned(), Arc::new(core_body))
            })
        }
        (term, _) => match synth_term(state, term) {
            (term, found_type) if state.is_subtype(&found_type, expected_type) => term,
            (_, found_type) => {
                state.report(TypeError::MismatchedTypes {
                    span: term.span(),
                    found_type,
                    expected_type: expected_type.clone(),
                });
                core::Term::Error
            }
        },
    }
}

/// Synthesize the type of a term.
pub fn synth_term<S: AsRef<str>>(
    state: &mut State<'_>,
    term: &Term<S>,
) -> (core::Term, Arc<core::Value>) {
    use std::collections::{BTreeMap, BTreeSet};

    match term {
        Term::Name(_, name) => {
            if let Some((index, r#type)) = state.get_local(name.as_ref()) {
                return (core::Term::Local(index), r#type.clone());
            }

            if let Some((r#type, _)) = state.globals.get(name.as_ref()) {
                let global = core::Term::Global(name.as_ref().to_owned());
                return (global.lift(state.universe_offset), state.eval_term(r#type));
            }

            state.report(TypeError::UnboundName {
                span: term.span(),
                name: name.as_ref().to_owned(),
            });
            (core::Term::Error, Arc::new(core::Value::Error))
        }
        Term::Ann(term, r#type) => {
            let (core_type, _) = check_type(state, r#type);
            let core_type_value = state.eval_term(&core_type);
            let core_term = check_term(state, term, &core_type_value);
            (
                core::Term::Ann(Arc::new(core_term), Arc::new(core_type)),
                core_type_value,
            )
        }
        Term::Literal(_, literal) => match literal {
            Literal::Number(_) => {
                state.report(TypeError::AmbiguousLiteral { span: term.span() });
                (core::Term::Error, Arc::new(core::Value::Error))
            }
            Literal::Char(data) => (
                parse_char(state, term.span(), data),
                Arc::new(core::Value::global("Char", 0, core::Value::universe(0))),
            ),
            Literal::String(data) => (
                parse_string(state, term.span(), data),
                Arc::new(core::Value::global("String", 0, core::Value::universe(0))),
            ),
        },
        Term::Sequence(_, _) => {
            state.report(TypeError::AmbiguousSequence { span: term.span() });
            (core::Term::Error, Arc::new(core::Value::Error))
        }
        Term::RecordTerm(_, term_entries) => {
            if term_entries.is_empty() {
                (
                    core::Term::RecordTerm(BTreeMap::new()),
                    Arc::from(core::Value::RecordTypeEmpty),
                )
            } else {
                state.report(TypeError::AmbiguousRecordTerm { span: term.span() });
                (core::Term::Error, Arc::new(core::Value::Error))
            }
        }
        Term::RecordType(_, type_entries) => {
            let mut max_level = core::UniverseLevel(0);
            let mut duplicate_names = Vec::new();
            let mut seen_names = BTreeSet::new();
            let mut core_type_entries = Vec::new();

            for (entry_name_span, entry_name, entry_type) in type_entries {
                if seen_names.insert(entry_name.as_ref()) {
                    let (core_type, level) = check_type(state, entry_type);
                    max_level = match level {
                        Some(level) => std::cmp::max(max_level, level),
                        None => return (core::Term::Error, Arc::new(core::Value::Error)),
                    };
                    let core_type = Arc::new(core_type);
                    let core_type_value = state.eval_term(&core_type);
                    core_type_entries.push((entry_name.as_ref().to_owned(), core_type));
                    state.push_param(entry_name.as_ref().to_owned(), core_type_value);
                } else {
                    duplicate_names.push((entry_name_span.clone(), entry_name.as_ref().to_owned()));
                    check_type(state, entry_type);
                }
            }

            if !duplicate_names.is_empty() {
                state.report(TypeError::DuplicateNamesInRecordType {
                    span: term.span(),
                    duplicate_names,
                });
            }

            state.pop_many_locals(seen_names.len());

            (
                core::Term::RecordType(core_type_entries),
                Arc::new(core::Value::Universe(max_level)),
            )
        }
        Term::RecordElim(_, head, name) => {
            let (core_head, mut head_type) = synth_term(state, head);
            let core_head = Arc::new(core_head);

            while let core::Value::RecordTypeExtend(current_name, entry_type, rest_type) =
                head_type.as_ref()
            {
                let term = core::Term::RecordElim(core_head.clone(), name.as_ref().to_owned());
                if name.as_ref() == current_name {
                    return (term, entry_type.clone());
                } else {
                    let value = state.eval_term(&term);
                    head_type = state.eval_closure_elim(rest_type, value);
                }
            }

            state.report(TypeError::FieldNotFound {
                span: term.span(),
                expected_field_name: name.as_ref().to_owned(),
                head_type,
            });
            (core::Term::Error, Arc::new(core::Value::Error))
        }
        Term::FunctionType(param_type, body_type) => {
            match (check_type(state, param_type), check_type(state, body_type)) {
                ((core_param_type, Some(param_level)), (core_body_type, Some(body_level))) => (
                    core::Term::FunctionType(Arc::new(core_param_type), Arc::new(core_body_type)),
                    Arc::new(core::Value::Universe(std::cmp::max(
                        param_level,
                        body_level,
                    ))),
                ),
                (_, _) => (core::Term::Error, Arc::new(core::Value::Error)),
            }
        }
        Term::FunctionTerm(_, _, _) => {
            state.report(TypeError::AmbiguousFunctionTerm { span: term.span() });
            (core::Term::Error, Arc::new(core::Value::Error))
        }
        Term::FunctionElim(head, arguments) => {
            let (mut core_head, mut head_type) = synth_term(state, head);

            for argument in arguments {
                match head_type.as_ref() {
                    core::Value::FunctionType(param_type, body_type) => {
                        core_head = core::Term::FunctionElim(
                            Arc::new(core_head),
                            Arc::new(check_term(state, argument, &param_type)),
                        );
                        head_type = body_type.clone();
                    }
                    core::Value::Error => return (core::Term::Error, Arc::new(core::Value::Error)),
                    _ => {
                        state.report(TypeError::NotAFunction {
                            span: term.span(),
                            head_type,
                        });
                        return (core::Term::Error, Arc::new(core::Value::Error));
                    }
                }
            }

            (core_head, head_type)
        }
        Term::Lift(_, inner_term, offset) => {
            match state.universe_offset + core::UniverseOffset(*offset) {
                Some(new_offset) => {
                    let previous_offset = std::mem::replace(&mut state.universe_offset, new_offset);
                    let (core_term, r#type) = synth_term(state, inner_term);
                    state.universe_offset = previous_offset;
                    (core_term, r#type)
                }
                None => {
                    state.report(TypeError::MaximumUniverseLevelReached { span: term.span() });
                    (core::Term::Error, Arc::new(core::Value::Error))
                }
            }
        }
        Term::Error(_) => (core::Term::Error, Arc::new(core::Value::Error)),
    }
}

fn parse_number<S: AsRef<str>, T: FromStr>(
    state: &mut State<'_>,
    span: Range<usize>,
    data: &S,
    f: impl Fn(T) -> core::Constant,
) -> core::Term {
    // TODO: improve parser (eg. numeric separators, positive sign)
    match data.as_ref().parse() {
        Ok(value) => core::Term::Constant(f(value)),
        Err(_) => {
            state.report(TypeError::InvalidNumberLiteral { span });
            core::Term::Error
        }
    }
}

fn parse_char<S: AsRef<str>>(state: &mut State<'_>, span: Range<usize>, data: &S) -> core::Term {
    // TODO: Improve parser (escapes)
    match data.as_ref().chars().nth(1) {
        Some(value) => core::Term::Constant(core::Constant::Char(value)),
        None => {
            state.report(TypeError::InvalidCharLiteral { span });
            core::Term::Error
        }
    }
}

fn parse_string<S: AsRef<str>>(state: &mut State<'_>, span: Range<usize>, data: &S) -> core::Term {
    // TODO: Improve parser (escapes)
    match data.as_ref().get(1..data.as_ref().len() - 1) {
        Some(value) => core::Term::Constant(core::Constant::String(value.to_owned())),
        None => {
            state.report(TypeError::InvalidStringLiteral { span });
            core::Term::Error
        }
    }
}
