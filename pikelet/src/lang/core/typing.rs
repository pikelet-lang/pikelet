//! Bidirectional type checker for the [core language].
//!
//! This is a simpler implementation of type checking than the one found in
//! [pass::surface_to_core], because it only needs to check the (much simpler)
//! core language, and doesn't need to perform any additional elaboration.
//! We can use it as a way to validate that elaborated terms are well-formed
//! for debugging and development purposes.
//!
//! [core language]: crate::lang::core
//! [`pass::surface_to_core`]: crate::pass::surface_to_core

use contracts::debug_ensures;
use crossbeam_channel::Sender;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Unfold, Value};
use crate::lang::core::{Constant, Globals, LocalIndex, LocalSize, Locals, Term, TermData};
use crate::reporting::{AmbiguousTerm, CoreTypingMessage, ExpectedType, Message};

/// Type checking context.
pub struct Context<'me> {
    /// Global definition environment.
    globals: &'me Globals,
    /// Local type environment (used for getting the types of local variables).
    local_declarations: Vec<Arc<Value>>,
    /// Local value environment (used for evaluation).
    local_definitions: Locals<Arc<Value>>,
    /// The diagnostic messages accumulated during type checking.
    message_tx: Sender<Message>,
}

impl<'me> Context<'me> {
    /// Construct a new type checker state.
    pub fn new(globals: &'me Globals, message_tx: Sender<Message>) -> Context<'me> {
        Context {
            globals,
            local_declarations: Vec::new(),
            local_definitions: Locals::new(),
            message_tx,
        }
    }

    /// Get the size of the local environment.
    fn size(&self) -> LocalSize {
        self.local_definitions.size()
    }

    /// Get a local entry.
    fn get_local(&self, local_index: LocalIndex) -> Option<&Arc<Value>> {
        let local_level = self.size().index_to_level(local_index)?;
        self.local_declarations.get(local_level.to_usize())
    }

    /// Push a local entry.
    fn push_local(&mut self, value: Arc<Value>, r#type: Arc<Value>) {
        self.local_declarations.push(r#type);
        self.local_definitions.push(value);
    }

    /// Push a local parameter.
    fn push_local_param(&mut self, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.size().next_level(), []));
        self.push_local(value.clone(), r#type);
        value
    }

    /// Pop a local entry.
    fn pop_local(&mut self) {
        self.local_declarations.pop();
        self.local_definitions.pop();
    }

    /// Pop the given number of local entries.
    fn pop_many_locals(&mut self, count: usize) {
        let len = self.size().to_usize().saturating_sub(count);
        self.local_declarations.truncate(len);
        self.local_definitions.truncate(len);
    }

    /// Report a diagnostic message.
    fn report(&self, message: CoreTypingMessage) {
        self.message_tx.send(message.into()).unwrap();
    }

    /// Evaluate a [`Term`] into a [`Value`].
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`Term`]: crate::lang::core::Term
    pub fn eval(&mut self, term: &Term) -> Arc<Value> {
        semantics::eval(self.globals, &mut self.local_definitions, term)
    }

    /// Return the type of the record elimination.
    pub fn record_elim_type(
        &mut self,
        head_term: &Term,
        head_type: &Arc<Value>,
        label: &str,
    ) -> Option<Arc<Value>> {
        semantics::record_elim_type(
            self.globals,
            &mut self.local_definitions,
            head_term,
            head_type,
            label,
        )
    }

    /// Read back a value into a normal form using the current state of the elaborator.
    pub fn read_back(&self, value: &Value) -> Term {
        semantics::read_back(self.globals, self.size(), Unfold::Never, value)
    }

    /// Check that one [`Value`] is computationally equal to another [`Value`].
    ///
    /// Returns `false` if either value is not a type.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, self.size(), value0, value1)
    }

    /// Check that a term is a type.
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn is_type(&mut self, term: &Term) -> bool {
        let r#type = self.synth_type(term);
        match r#type.force(self.globals) {
            Value::TypeType => true,
            Value::Error => false,
            _ => {
                self.report(CoreTypingMessage::MismatchedTypes {
                    found_type: self.read_back(&r#type),
                    expected_type: ExpectedType::Universe,
                });
                false
            }
        }
    }

    /// Check that a term is an element of a type.
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn check_type(&mut self, term: &Term, expected_type: &Arc<Value>) {
        match (&term.data, expected_type.force(self.globals)) {
            (_, Value::Error) => {}

            (
                TermData::FunctionTerm(_, output_term),
                Value::FunctionType(_, input_type, output_closure),
            ) => {
                let input_term = self.push_local_param(input_type.clone());
                let output_type = output_closure.apply(self.globals, input_term);
                self.check_type(output_term, &output_type);
                self.pop_local();
            }
            (TermData::FunctionTerm(_, _), _) => {
                self.report(CoreTypingMessage::TooManyInputsInFunctionTerm);
            }

            (TermData::RecordTerm(term_labels, terms), Value::RecordType(type_labels, closure)) => {
                if term_labels.len() != terms.len() {
                    self.report(CoreTypingMessage::InvalidRecordTermLabelCount);
                    return;
                }
                if term_labels != type_labels {
                    self.report(CoreTypingMessage::UnexpectedRecordTermLabels {
                        found_labels: term_labels.clone(),
                        expected_labels: type_labels.clone(),
                    });
                    return;
                }

                let mut pending_terms = terms.iter();
                let mut entry_count = 0;

                closure.for_each_entry(self.globals, |r#type| match pending_terms.next() {
                    Some(term) => {
                        self.check_type(&term, &r#type);
                        let value = self.eval(&term);

                        self.push_local(value.clone(), r#type);
                        entry_count += 1;

                        value
                    }
                    None => Arc::new(Value::Error),
                });

                self.pop_many_locals(entry_count);
            }

            (TermData::ArrayTerm(entry_terms), forced_type) => match forced_type.try_global() {
                Some(("Array", [Elim::Function(len), Elim::Function(entry_type)])) => {
                    let forced_entry_type = entry_type.force(self.globals);
                    for entry_term in entry_terms {
                        self.check_type(entry_term, forced_entry_type);
                    }

                    match len.force(self.globals).as_ref() {
                        Value::Constant(Constant::U32(len))
                            if *len as usize == entry_terms.len() => {}
                        _ => {
                            self.report(CoreTypingMessage::MismatchedTypes {
                                expected_type: ExpectedType::Type(self.read_back(expected_type)),
                                found_type: self.read_back(&Value::global(
                                    "Array",
                                    [
                                        Elim::Function(len.clone()),
                                        Elim::Function(entry_type.clone()),
                                    ],
                                )),
                            });
                        }
                    }
                }
                Some(_) | None => self.report(CoreTypingMessage::UnexpectedArrayTerm {
                    expected_type: self.read_back(expected_type),
                }),
            },
            (TermData::ListTerm(entry_terms), forced_type) => match forced_type.try_global() {
                Some(("List", [Elim::Function(entry_type)])) => {
                    let forced_entry_type = entry_type.force(self.globals);
                    for entry_term in entry_terms {
                        self.check_type(entry_term, forced_entry_type);
                    }
                }
                Some(_) | None => self.report(CoreTypingMessage::UnexpectedListTerm {
                    expected_type: self.read_back(expected_type),
                }),
            },

            (_, _) => match self.synth_type(term) {
                found_type if self.is_equal(&found_type, expected_type) => {}
                found_type => self.report(CoreTypingMessage::MismatchedTypes {
                    found_type: self.read_back(&found_type),
                    expected_type: ExpectedType::Type(self.read_back(expected_type)),
                }),
            },
        }
    }

    /// Synthesize the type of a term.
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn synth_type(&mut self, term: &Term) -> Arc<Value> {
        match &term.data {
            TermData::Global(name) => match self.globals.get(name) {
                Some((r#type, _)) => self.eval(r#type),
                None => {
                    self.report(CoreTypingMessage::UnboundGlobal {
                        name: name.to_owned(),
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Local(local_index) => match self.get_local(*local_index) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.report(CoreTypingMessage::UnboundLocal);
                    Arc::new(Value::Error)
                }
            },

            TermData::Ann(term, r#type) => {
                if !self.is_type(r#type) {
                    return Arc::new(Value::Error);
                }
                let r#type = self.eval(r#type);
                self.check_type(term, &r#type);
                r#type
            }

            TermData::TypeType => Arc::new(Value::TypeType),

            TermData::FunctionType(_, input_type, output_type) => {
                if !self.is_type(input_type) {
                    return Arc::new(Value::Error);
                }
                let input_type = self.eval(input_type);

                self.push_local_param(input_type);
                if !self.is_type(output_type) {
                    self.pop_local();
                    return Arc::new(Value::Error);
                }
                self.pop_local();
                Arc::new(Value::TypeType)
            }
            TermData::FunctionTerm(_, _) => {
                self.report(CoreTypingMessage::AmbiguousTerm {
                    term: AmbiguousTerm::FunctionTerm,
                });
                Arc::new(Value::Error)
            }
            TermData::FunctionElim(head_term, input_term) => {
                let head_type = self.synth_type(head_term);
                match head_type.force(self.globals) {
                    Value::FunctionType(_, input_type, output_closure) => {
                        self.check_type(input_term, &input_type);
                        let input_value = self.eval(input_term);
                        output_closure.apply(self.globals, input_value)
                    }
                    Value::Error => Arc::new(Value::Error),
                    _ => {
                        self.report(CoreTypingMessage::TooManyInputsInFunctionElim {
                            head_type: self.read_back(&head_type),
                        });
                        Arc::new(Value::Error)
                    }
                }
            }

            TermData::RecordTerm(_, _) => {
                self.report(CoreTypingMessage::AmbiguousTerm {
                    term: AmbiguousTerm::RecordTerm,
                });
                Arc::new(Value::Error)
            }
            TermData::RecordType(labels, types) => {
                use std::collections::BTreeSet;

                if labels.len() != types.len() {
                    self.report(CoreTypingMessage::InvalidRecordTypeLabelCount);
                    return Arc::new(Value::Error);
                }

                let mut duplicate_labels = Vec::new();
                let mut seen_labels = BTreeSet::new();

                for (name, r#type) in Iterator::zip(labels.iter(), types.iter()) {
                    if !seen_labels.insert(name) {
                        duplicate_labels.push(name.clone());
                    }
                    if !self.is_type(r#type) {
                        self.pop_many_locals(seen_labels.len());
                        return Arc::new(Value::Error);
                    }
                    let r#type = self.eval(r#type);
                    self.push_local_param(r#type);
                }

                self.pop_many_locals(seen_labels.len());

                if !duplicate_labels.is_empty() {
                    self.report(CoreTypingMessage::InvalidRecordType { duplicate_labels });
                }

                Arc::new(Value::TypeType)
            }
            TermData::RecordElim(head_term, label) => {
                let head_type = self.synth_type(head_term);

                match self.record_elim_type(&head_term, &head_type, label) {
                    Some(entry_type) => entry_type,
                    None => {
                        self.report(CoreTypingMessage::LabelNotFound {
                            expected_label: label.clone(),
                            head_type: self.read_back(&head_type),
                        });
                        Arc::new(Value::Error)
                    }
                }
            }

            TermData::ArrayTerm(_) => {
                self.report(CoreTypingMessage::AmbiguousTerm {
                    term: AmbiguousTerm::Sequence,
                });
                Arc::new(Value::Error)
            }
            TermData::ListTerm(_) => {
                self.report(CoreTypingMessage::AmbiguousTerm {
                    term: AmbiguousTerm::Sequence,
                });
                Arc::new(Value::Error)
            }

            TermData::Constant(Constant::U8(_)) => Arc::new(Value::global("U8", [])),
            TermData::Constant(Constant::U16(_)) => Arc::new(Value::global("U16", [])),
            TermData::Constant(Constant::U32(_)) => Arc::new(Value::global("U32", [])),
            TermData::Constant(Constant::U64(_)) => Arc::new(Value::global("U64", [])),
            TermData::Constant(Constant::S8(_)) => Arc::new(Value::global("S8", [])),
            TermData::Constant(Constant::S16(_)) => Arc::new(Value::global("S16", [])),
            TermData::Constant(Constant::S32(_)) => Arc::new(Value::global("S32", [])),
            TermData::Constant(Constant::S64(_)) => Arc::new(Value::global("S64", [])),
            TermData::Constant(Constant::F32(_)) => Arc::new(Value::global("F32", [])),
            TermData::Constant(Constant::F64(_)) => Arc::new(Value::global("F64", [])),
            TermData::Constant(Constant::Char(_)) => Arc::new(Value::global("Char", [])),
            TermData::Constant(Constant::String(_)) => Arc::new(Value::global("String", [])),

            TermData::Error => Arc::new(Value::Error),
        }
    }
}
