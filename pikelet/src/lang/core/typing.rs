//! Bidirectional type checker for the core language.
//!
//! This is a simpler implementation of type checking than the one found in [pass::surface_to_core],
//! because it only needs to check the (much simpler) core language,
//! and doesn't need to perform any additional elaboration.
//! We can use it as a way to validate that elaborated terms are well-formed for debugging and development purposes.

use crossbeam_channel::Sender;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Head, RecordTypeClosure, Unfold, Value};
use crate::lang::core::{
    Constant, Globals, LocalLevel, Locals, Term, UniverseLevel, UniverseOffset,
};
use crate::reporting::{AmbiguousTerm, CoreTypingMessage, ExpectedType};

/// The state of the type checker.
pub struct State<'me> {
    /// Global definition environment.
    globals: &'me Globals,
    /// The current universe offset.
    universe_offset: UniverseOffset,
    /// Local type environment (used for getting the types of local variables).
    types: Locals<Arc<Value>>,
    /// Local value environment (used for evaluation).
    values: Locals<Arc<Value>>,
    /// The diagnostic messages accumulated during type checking.
    message_tx: Sender<crate::reporting::Message>,
}

impl<'me> State<'me> {
    /// Construct a new type checker state.
    pub fn new(globals: &'me Globals, message_tx: Sender<crate::reporting::Message>) -> State<'me> {
        State {
            globals,
            universe_offset: UniverseOffset(0),
            types: Locals::new(),
            values: Locals::new(),
            message_tx,
        }
    }

    /// Get the next level to be used for a local entry.
    fn next_level(&self) -> LocalLevel {
        self.values.size().next_level()
    }

    /// Push a local entry.
    fn push_local(&mut self, value: Arc<Value>, r#type: Arc<Value>) {
        self.types.push(r#type);
        self.values.push(value);
    }

    /// Push a local parameter.
    fn push_local_param(&mut self, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.next_level()));
        self.push_local(value.clone(), r#type);
        value
    }

    /// Pop a local entry.
    fn pop_local(&mut self) {
        self.types.pop();
        self.values.pop();
    }

    /// Pop the given number of local entries.
    fn pop_many_locals(&mut self, count: usize) {
        self.types.pop_many(count);
        self.values.pop_many(count);
    }

    /// Report a diagnostic message.
    fn report(&mut self, message: CoreTypingMessage) {
        self.message_tx.send(message.into()).unwrap();
    }

    /// Reset the type checker state while retaining existing allocations.
    pub fn clear(&mut self) {
        self.universe_offset = UniverseOffset(0);
        self.types.clear();
        self.values.clear();
    }

    /// Evaluate a term using the current state of the type checker.
    pub fn eval_term(&mut self, term: &Term) -> Arc<Value> {
        semantics::eval_term(self.globals, self.universe_offset, &mut self.values, term)
    }

    /// Return the type of the record elimination.
    pub fn record_elim_type(
        &mut self,
        head_value: Arc<Value>,
        name: &str,
        closure: &RecordTypeClosure,
    ) -> Option<Arc<Value>> {
        semantics::record_elim_type(self.globals, head_value, name, closure)
    }

    /// Read back a value into a normal form using the current state of the elaborator.
    pub fn read_back_value(&mut self, value: &Value) -> Term {
        semantics::read_back_value(self.globals, self.values.size(), Unfold::None, value)
    }

    /// Check if `value0` is a subtype of `value1`.
    pub fn is_subtype(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_subtype(self.globals, self.values.size(), value0, value1)
    }
}

/// Check that a term is a type and return the universe level it inhabits.
pub fn is_type(state: &mut State<'_>, term: &Term) -> Option<UniverseLevel> {
    let r#type = synth_type(state, term);
    match r#type.force(state.globals) {
        Value::TypeType(level) => Some(*level),
        Value::Error => None,
        _ => {
            let r#type = state.read_back_value(&r#type);
            state.report(CoreTypingMessage::MismatchedTypes {
                found_type: r#type,
                expected_type: ExpectedType::Universe,
            });
            None
        }
    }
}

/// Check that a term is an element of a type.
pub fn check_type(state: &mut State<'_>, term: &Term, expected_type: &Arc<Value>) {
    match (term, expected_type.force(state.globals)) {
        (_, Value::Error) => {}

        (
            Term::FunctionTerm(_, output_term),
            Value::FunctionType(_, input_type, output_closure),
        ) => {
            let input_term = state.push_local_param(input_type.clone());
            let output_type = output_closure.elim(state.globals, input_term);
            check_type(state, output_term, &output_type);
            state.pop_local();
        }
        (Term::FunctionTerm(_, _), _) => {
            state.report(CoreTypingMessage::TooManyInputsInFunctionTerm);
        }

        (Term::RecordTerm(term_entries), Value::RecordType(closure)) => {
            let mut missing_labels = Vec::new();

            let mut pending_term_entries = term_entries.clone();

            closure.entries(state.globals, |label, r#type| {
                match pending_term_entries.remove(label) {
                    Some(entry_term) => {
                        check_type(state, &entry_term, &r#type);
                        state.eval_term(&entry_term)
                    }
                    None => {
                        missing_labels.push(label.to_owned());
                        Arc::new(Value::Error)
                    }
                }
            });

            if !missing_labels.is_empty() && !pending_term_entries.is_empty() {
                let unexpected_labels = (pending_term_entries.into_iter())
                    .map(|(label, _)| label)
                    .collect();
                state.report(CoreTypingMessage::InvalidRecordTerm {
                    missing_labels,
                    unexpected_labels,
                });
            }
        }

        (Term::Sequence(entry_terms), Value::Stuck(Head::Global(name, _), spine)) => {
            match (name.as_ref(), spine.as_slice()) {
                ("Array", [Elim::Function(len), Elim::Function(entry_type)]) => {
                    let entry_type = entry_type.force(state.globals);
                    for entry_term in entry_terms {
                        check_type(state, entry_term, entry_type);
                    }

                    match len.force(state.globals).as_ref() {
                        Value::Constant(Constant::U32(len))
                            if *len as usize == entry_terms.len() => {}
                        len => {
                            let expected_len = state.read_back_value(len);
                            state.report(CoreTypingMessage::MismatchedSequenceLength {
                                found_len: entry_terms.len(),
                                expected_len,
                            })
                        }
                    }
                }
                ("List", [Elim::Function(entry_type)]) => {
                    let entry_type = entry_type.force(state.globals);
                    for entry_term in entry_terms {
                        check_type(state, entry_term, entry_type);
                    }
                }
                _ => {
                    let expected_type = state.read_back_value(expected_type);
                    state.report(CoreTypingMessage::NoSequenceConversion { expected_type })
                }
            }
        }
        (Term::Sequence(_), _) => {
            let expected_type = state.read_back_value(expected_type);
            state.report(CoreTypingMessage::NoSequenceConversion { expected_type })
        }

        (term, _) => match synth_type(state, term) {
            found_type if state.is_subtype(&found_type, expected_type) => {}
            found_type => {
                let found_type = state.read_back_value(&found_type);
                let expected_type = ExpectedType::Type(state.read_back_value(expected_type));
                state.report(CoreTypingMessage::MismatchedTypes {
                    found_type,
                    expected_type,
                })
            }
        },
    }
}

/// Synthesize the type of a term.
pub fn synth_type(state: &mut State<'_>, term: &Term) -> Arc<Value> {
    match term {
        Term::Global(name) => match state.globals.get(name) {
            Some((r#type, _)) => state.eval_term(r#type),
            None => {
                state.report(CoreTypingMessage::UnboundGlobal {
                    name: name.to_owned(),
                });
                Arc::new(Value::Error)
            }
        },
        Term::Local(index) => match state.types.get(*index) {
            Some(r#type) => r#type.clone(),
            None => {
                state.report(CoreTypingMessage::UnboundLocal);
                Arc::new(Value::Error)
            }
        },

        Term::Ann(term, r#type) => {
            is_type(state, r#type);
            let r#type = state.eval_term(r#type);
            check_type(state, term, &r#type);
            r#type
        }

        Term::TypeType(level) => match *level + UniverseOffset(1) {
            Some(level) => Arc::new(Value::type_type(level)),
            None => {
                state.report(CoreTypingMessage::MaximumUniverseLevelReached);
                Arc::new(Value::Error)
            }
        },
        Term::Lift(term, offset) => match state.universe_offset + *offset {
            Some(new_offset) => {
                let previous_offset = std::mem::replace(&mut state.universe_offset, new_offset);
                let r#type = synth_type(state, term);
                state.universe_offset = previous_offset;
                r#type
            }
            None => {
                state.report(CoreTypingMessage::MaximumUniverseLevelReached);
                Arc::new(Value::Error)
            }
        },

        Term::FunctionType(_, input_type, output_type) => {
            let input_level = is_type(state, input_type);
            let input_type = match input_level {
                None => Arc::new(Value::Error),
                Some(_) => state.eval_term(input_type),
            };

            state.push_local_param(input_type);
            let output_level = is_type(state, output_type);
            state.pop_local();

            match (input_level, output_level) {
                (Some(input_level), Some(output_level)) => {
                    Arc::new(Value::TypeType(std::cmp::max(input_level, output_level)))
                }
                (_, _) => Arc::new(Value::Error),
            }
        }
        Term::FunctionTerm(_, _) => {
            state.report(CoreTypingMessage::AmbiguousTerm {
                term: AmbiguousTerm::FunctionTerm,
            });
            Arc::new(Value::Error)
        }
        Term::FunctionElim(head_term, input_term) => {
            let head_type = synth_type(state, head_term);
            match head_type.force(state.globals) {
                Value::FunctionType(_, input_type, output_closure) => {
                    check_type(state, input_term, &input_type);
                    let input_value = state.eval_term(input_term);
                    output_closure.elim(state.globals, input_value)
                }
                Value::Error => Arc::new(Value::Error),
                _ => {
                    let head_type = state.read_back_value(&head_type);
                    state.report(CoreTypingMessage::TooManyInputsInFunctionElim { head_type });
                    Arc::new(Value::Error)
                }
            }
        }

        Term::RecordTerm(term_entries) => {
            if term_entries.is_empty() {
                Arc::from(Value::RecordType(RecordTypeClosure::new(
                    state.universe_offset,
                    state.values.clone(),
                    Arc::new([]),
                )))
            } else {
                state.report(CoreTypingMessage::AmbiguousTerm {
                    term: AmbiguousTerm::RecordTerm,
                });
                Arc::new(Value::Error)
            }
        }
        Term::RecordType(type_entries) => {
            use std::collections::BTreeSet;

            let mut max_level = UniverseLevel(0);
            let mut duplicate_labels = Vec::new();
            let mut seen_labels = BTreeSet::new();

            for (name, r#type) in type_entries.iter() {
                if !seen_labels.insert(name) {
                    duplicate_labels.push(name.clone());
                }
                max_level = match is_type(state, r#type) {
                    Some(level) => std::cmp::max(max_level, level),
                    None => {
                        state.pop_many_locals(seen_labels.len());
                        return Arc::new(Value::Error);
                    }
                };
                let r#type = state.eval_term(r#type);
                state.push_local_param(r#type);
            }

            state.pop_many_locals(seen_labels.len());

            if !duplicate_labels.is_empty() {
                state.report(CoreTypingMessage::InvalidRecordType { duplicate_labels });
            }

            Arc::new(Value::TypeType(max_level))
        }
        Term::RecordElim(head_term, label) => {
            let head_type = synth_type(state, head_term);

            match head_type.force(state.globals) {
                Value::RecordType(closure) => {
                    let head_value = state.eval_term(head_term);

                    if let Some(entry_type) = state.record_elim_type(head_value, label, closure) {
                        return entry_type;
                    }
                }
                Value::Error => return Arc::new(Value::Error),
                _ => {}
            }

            let head_type = state.read_back_value(&head_type);
            state.report(CoreTypingMessage::LabelNotFound {
                expected_label: label.clone(),
                head_type,
            });
            Arc::new(Value::Error)
        }

        Term::Sequence(_) => {
            state.report(CoreTypingMessage::AmbiguousTerm {
                term: AmbiguousTerm::Sequence,
            });
            Arc::new(Value::Error)
        }

        Term::Constant(constant) => Arc::new(match constant {
            Constant::U8(_) => Value::global("U8", 0),
            Constant::U16(_) => Value::global("U16", 0),
            Constant::U32(_) => Value::global("U32", 0),
            Constant::U64(_) => Value::global("U64", 0),
            Constant::S8(_) => Value::global("S8", 0),
            Constant::S16(_) => Value::global("S16", 0),
            Constant::S32(_) => Value::global("S32", 0),
            Constant::S64(_) => Value::global("S64", 0),
            Constant::F32(_) => Value::global("F32", 0),
            Constant::F64(_) => Value::global("F64", 0),
            Constant::Char(_) => Value::global("Char", 0),
            Constant::String(_) => Value::global("String", 0),
        }),

        Term::Error => Arc::new(Value::Error),
    }
}
