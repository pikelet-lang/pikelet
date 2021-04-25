//! Elaborates the [surface language] into the [core language].
//!
//! This translation pass is the main place where user-facing type errors will be returned.
//!
//! [surface language]: crate::lang::surface
//! [core language]: crate::lang::core

use contracts::debug_ensures;
use crossbeam_channel::Sender;
use num_traits::{Float, PrimInt, Signed, Unsigned};
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, RecordClosure, Unfold, Value};
use crate::lang::surface::{Term, TermData};
use crate::lang::{core, Location};
use crate::literal;
use crate::pass::core_to_surface;
use crate::reporting::{AmbiguousTerm, ExpectedType, Message, SurfaceToCoreMessage};

/// Elaboration context.
pub struct Context<'globals> {
    /// Global definition environment.
    globals: &'globals core::Globals,
    /// Type environment (used for getting the types of variables).
    types: Vec<(Option<String>, Arc<Value>)>,
    /// Value environment (used for evaluation).
    values: core::Env<Arc<Value>>,
    /// Distillation context (used for pretty printing).
    core_to_surface: core_to_surface::Context<'globals>,
    /// The diagnostic messages accumulated during elaboration.
    message_tx: Sender<Message>,
}

impl<'globals> Context<'globals> {
    /// Construct a new elaborator state.
    pub fn new(globals: &'globals core::Globals, message_tx: Sender<Message>) -> Context<'globals> {
        Context {
            globals,
            types: Vec::new(),
            values: core::Env::new(),
            core_to_surface: core_to_surface::Context::new(globals),
            message_tx,
        }
    }

    /// Get the number of values in the context.
    fn size(&self) -> core::EnvSize {
        self.values.size()
    }

    /// Get the type of a variable and its index at the current binding depth.
    fn get_type(&self, name: &str) -> Option<(&Arc<Value>, core::VarIndex)> {
        Iterator::zip(core::var_indices(), self.types.iter().rev()).find_map(
            |(index, (decl_name, r#type))| match decl_name {
                Some(decl_name) if decl_name == name => Some((r#type, index)),
                Some(_) | None => None,
            },
        )
    }

    /// Push a new definition onto the context, along its type annotation.
    fn push_definition(&mut self, name: Option<&str>, value: Arc<Value>, r#type: Arc<Value>) {
        self.types.push((name.map(str::to_owned), r#type));
        self.values.push(value);
        self.core_to_surface.push_scope(name);
    }

    /// Push a parameter onto the context.
    fn push_variable(&mut self, name: Option<&str>, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::var(self.size().next_level(), []));
        self.push_definition(name, value.clone(), r#type);
        value
    }

    /// Pop a scope off the context.
    fn pop_scope(&mut self) {
        self.types.pop();
        self.values.pop();
        self.core_to_surface.pop_scope();
    }

    /// Truncate the scopes in the context to the given size.
    fn truncate_scopes(&mut self, env_size: core::EnvSize) {
        self.types.truncate(env_size.to_usize());
        self.values.truncate(env_size);
        self.core_to_surface.truncate_scopes(env_size);
    }

    /// Report a diagnostic message.
    fn report(&self, error: SurfaceToCoreMessage) {
        self.message_tx.send(error.into()).unwrap();
    }

    /// Evaluate a [`core::Term`] into a [`Value`].
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&mut self, term: &core::Term) -> Arc<Value> {
        semantics::eval(self.globals, &mut self.values, term)
    }

    /// Return the type of the record elimination.
    pub fn record_elim_type(
        &mut self,
        head_term: &core::Term,
        head_type: &Arc<Value>,
        label: &str,
    ) -> Option<Arc<Value>> {
        semantics::record_elim_type(self.globals, &mut self.values, head_term, head_type, label)
    }

    /// Fully normalize a [`core::Term`] using [normalization by evaluation].
    ///
    /// [`core::Term`]: crate::lang::core::Term
    /// [normalization by evaluation]: https://en.wikipedia.org/wiki/Normalisation_by_evaluation
    pub fn normalize(&mut self, term: &core::Term) -> core::Term {
        semantics::normalize(self.globals, &mut self.values, term)
    }

    /// Read back a [`Value`] to a [`core::Term`] using the current
    /// state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and types in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn read_back(&self, value: &Value) -> core::Term {
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

    /// Distill a [`core::Term`] into a [`surface::Term`].
    ///
    /// [`core::Term`]: crate::lang::core::Term
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn core_to_surface(&mut self, core_term: &core::Term) -> Term {
        self.core_to_surface.from_term(&core_term)
    }

    /// Read back a [`Value`] into a [`surface::Term`] using the
    /// current state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and types in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn read_back_to_surface(&mut self, value: &Value) -> Term {
        self.core_to_surface(&self.read_back(value))
    }

    /// Check that a term is a type, and return the elaborated term.
    #[debug_ensures(self.types.len() == old(self.types.len()))]
    #[debug_ensures(self.values.size() == old(self.values.size()))]
    pub fn is_type(&mut self, term: &Term) -> Option<core::Term> {
        let (core_term, r#type) = self.synth_type(term);
        match r#type.force(self.globals) {
            Value::TypeType => Some(core_term),
            Value::Error => Some(core::Term::new(term.location, core::TermData::Error)),
            found_type => {
                let found_type = self.read_back_to_surface(&found_type);
                self.report(SurfaceToCoreMessage::MismatchedTypes {
                    location: term.location,
                    found_type,
                    expected_type: ExpectedType::Universe,
                });
                None
            }
        }
    }

    /// Check that a term is an element of a type, and return the elaborated term.
    #[debug_ensures(self.types.len() == old(self.types.len()))]
    #[debug_ensures(self.values.size() == old(self.values.size()))]
    pub fn check_type(&mut self, term: &Term, expected_type: &Arc<Value>) -> core::Term {
        match (&term.data, expected_type.force(self.globals)) {
            (_, Value::Error) => core::Term::new(term.location, core::TermData::Error),

            (TermData::FunctionTerm(input_names, output_term), _) => {
                let initial_size = self.size();
                let mut expected_type = expected_type.clone();
                let mut pending_input_names = input_names.iter();

                while let Some(input_name) = pending_input_names.next() {
                    match expected_type.force(self.globals) {
                        Value::FunctionType(_, input_type, output_closure) => {
                            let input_value =
                                self.push_variable(Some(&input_name.data), input_type.clone());
                            expected_type = output_closure.apply(self.globals, input_value);
                        }
                        Value::Error => {
                            self.truncate_scopes(initial_size);
                            return core::Term::new(term.location, core::TermData::Error);
                        }
                        _ => {
                            self.report(SurfaceToCoreMessage::TooManyInputsInFunctionTerm {
                                unexpected_inputs: std::iter::once(input_name.location)
                                    .chain(
                                        pending_input_names.map(|input_name| input_name.location),
                                    )
                                    .collect(),
                            });
                            self.check_type(output_term, &expected_type);
                            self.truncate_scopes(initial_size);
                            return core::Term::new(term.location, core::TermData::Error);
                        }
                    }
                }

                let core_output_term = self.check_type(output_term, &expected_type);
                self.truncate_scopes(initial_size);
                (input_names.iter().rev()).fold(core_output_term, |core_output_term, input_name| {
                    core::Term::new(
                        Location::merge(input_name.location, core_output_term.location),
                        core::TermData::FunctionTerm(
                            input_name.data.clone(),
                            Arc::new(core_output_term),
                        ),
                    )
                })
            }

            (TermData::RecordTerm(term_entries), Value::RecordType(type_labels, closure)) => {
                let initial_size = self.size();
                let mut pending_entries = term_entries.iter();
                let mut pending_type_labels = type_labels.iter();
                let mut core_terms = Vec::with_capacity(pending_entries.len());

                let mut missing_labels = Vec::new();
                let mut unexpected_labels = Vec::new();

                closure.for_each_entry(self.globals, |r#type| {
                    if let Some(label) = pending_type_labels.next() {
                        while let Some((next_label, name, term)) = pending_entries.next() {
                            if next_label.data == *label {
                                let name = name.as_ref().unwrap_or(next_label);
                                let core_term = self.check_type(term, &r#type);
                                let core_value = self.eval(&core_term);

                                self.push_definition(Some(&name.data), core_value.clone(), r#type);
                                core_terms.push(Arc::new(core_term));

                                return core_value;
                            } else {
                                unexpected_labels.push(next_label.location)
                            }
                        }
                        missing_labels.push(label.to_owned());
                    }
                    Arc::new(Value::Error)
                });

                self.truncate_scopes(initial_size);
                unexpected_labels.extend(pending_entries.map(|(label, _, _)| label.location));

                if !missing_labels.is_empty() || !unexpected_labels.is_empty() {
                    self.report(SurfaceToCoreMessage::InvalidRecordTerm {
                        location: term.location,
                        missing_labels,
                        unexpected_labels,
                    });
                }

                core::Term::new(
                    term.location,
                    core::TermData::RecordTerm(type_labels.clone(), core_terms.into()),
                )
            }

            (TermData::SequenceTerm(entry_terms), forced_type) => match forced_type.try_global() {
                Some(("Array", [Elim::Function(len), Elim::Function(core_entry_type)])) => {
                    let core_entry_type = core_entry_type.force(self.globals);
                    let core_entry_terms = entry_terms
                        .iter()
                        .map(|entry_term| Arc::new(self.check_type(entry_term, core_entry_type)))
                        .collect();

                    let len = len.force(self.globals);
                    match len.as_ref() {
                        Value::Constant(core::Constant::U32(len))
                            if *len as usize == entry_terms.len() =>
                        {
                            core::Term::new(
                                term.location,
                                core::TermData::ArrayTerm(core_entry_terms),
                            )
                        }
                        Value::Error => core::Term::new(term.location, core::TermData::Error),
                        _ => {
                            let expected_len = self.read_back_to_surface(&len);
                            self.report(SurfaceToCoreMessage::MismatchedSequenceLength {
                                location: term.location,
                                found_len: entry_terms.len(),
                                expected_len,
                            });
                            core::Term::new(term.location, core::TermData::Error)
                        }
                    }
                }
                Some(("List", [Elim::Function(core_entry_type)])) => {
                    let core_entry_type = core_entry_type.force(self.globals);
                    let core_entry_terms = entry_terms
                        .iter()
                        .map(|entry_term| Arc::new(self.check_type(entry_term, core_entry_type)))
                        .collect();

                    core::Term::new(term.location, core::TermData::ListTerm(core_entry_terms))
                }
                Some(_) | None => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.report(SurfaceToCoreMessage::NoSequenceConversion {
                        location: term.location,
                        expected_type,
                    });
                    core::Term::new(term.location, core::TermData::Error)
                }
            },
            (TermData::NumberTerm(data), forced_type) => match forced_type.try_global() {
                Some(("U8", [])) => self.parse_unsigned(term.location, data, core::Constant::U8),
                Some(("U16", [])) => self.parse_unsigned(term.location, data, core::Constant::U16),
                Some(("U32", [])) => self.parse_unsigned(term.location, data, core::Constant::U32),
                Some(("U64", [])) => self.parse_unsigned(term.location, data, core::Constant::U64),
                Some(("S8", [])) => self.parse_signed(term.location, data, core::Constant::S8),
                Some(("S16", [])) => self.parse_signed(term.location, data, core::Constant::S16),
                Some(("S32", [])) => self.parse_signed(term.location, data, core::Constant::S32),
                Some(("S64", [])) => self.parse_signed(term.location, data, core::Constant::S64),
                Some(("F32", [])) => self.parse_float(term.location, data, core::Constant::F32),
                Some(("F64", [])) => self.parse_float(term.location, data, core::Constant::F64),
                Some(_) | None => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.report(SurfaceToCoreMessage::NoLiteralConversion {
                        location: term.location,
                        expected_type,
                    });
                    core::Term::new(term.location, core::TermData::Error)
                }
            },
            (TermData::CharTerm(data), forced_type) => match forced_type.try_global() {
                Some(("Char", [])) => self.parse_char(term.location, data),
                Some(_) | None => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.report(SurfaceToCoreMessage::NoLiteralConversion {
                        location: term.location,
                        expected_type,
                    });
                    core::Term::new(term.location, core::TermData::Error)
                }
            },
            (TermData::StringTerm(data), forced_type) => match forced_type.try_global() {
                Some(("String", [])) => self.parse_string(term.location, data),
                Some(_) | None => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.report(SurfaceToCoreMessage::NoLiteralConversion {
                        location: term.location,
                        expected_type,
                    });
                    core::Term::new(term.location, core::TermData::Error)
                }
            },

            (_, _) => match self.synth_type(term) {
                (term, found_type) if self.is_equal(&found_type, expected_type) => term,
                (_, found_type) => {
                    let found_type = self.read_back_to_surface(&found_type);
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.report(SurfaceToCoreMessage::MismatchedTypes {
                        location: term.location,
                        found_type,
                        expected_type: ExpectedType::Type(expected_type),
                    });
                    core::Term::new(term.location, core::TermData::Error)
                }
            },
        }
    }

    /// Synthesize the type of a surface term, and return the elaborated term.
    #[debug_ensures(self.types.len() == old(self.types.len()))]
    #[debug_ensures(self.values.size() == old(self.values.size()))]
    pub fn synth_type(&mut self, term: &Term) -> (core::Term, Arc<Value>) {
        use std::collections::BTreeMap;

        let error_term = || core::Term::new(term.location, core::TermData::Error);

        match &term.data {
            TermData::Name(name) => {
                if let Some((r#type, var_index)) = self.get_type(name.as_ref()) {
                    let term_data = core::TermData::Var(var_index);
                    return (core::Term::new(term.location, term_data), r#type.clone());
                }

                if let Some((r#type, _)) = self.globals.get(name.as_ref()) {
                    let term_data = core::TermData::Global(name.clone());
                    return (core::Term::new(term.location, term_data), self.eval(r#type));
                }

                self.report(SurfaceToCoreMessage::UnboundName {
                    location: term.location,
                    name: name.clone(),
                });
                (error_term(), Arc::new(Value::Error))
            }

            TermData::Ann(term, r#type) => {
                let core_type = match self.is_type(r#type) {
                    Some(core_type) => core_type,
                    None => return (error_term(), Arc::new(Value::Error)),
                };
                let core_type_value = self.eval(&core_type);
                let core_term = self.check_type(term, &core_type_value);
                (
                    core::Term::new(
                        term.location,
                        core::TermData::Ann(Arc::new(core_term), Arc::new(core_type)),
                    ),
                    core_type_value,
                )
            }

            TermData::FunctionType(input_type_groups, output_type) => {
                let initial_size = self.size();
                let mut core_inputs = Vec::new();

                for (input_names, input_type) in input_type_groups {
                    for input_name in input_names {
                        let core_input_type = match self.is_type(input_type) {
                            Some(core_input_type) => core_input_type,
                            None => {
                                self.truncate_scopes(initial_size);
                                return (error_term(), Arc::new(Value::Error));
                            }
                        };

                        let core_input_type_value = self.eval(&core_input_type);
                        self.push_variable(Some(&input_name.data), core_input_type_value);
                        core_inputs.push((input_name.clone(), core_input_type));
                    }
                }

                let core_output_type = match self.is_type(output_type) {
                    Some(core_output_type) => core_output_type,
                    None => {
                        self.truncate_scopes(initial_size);
                        return (error_term(), Arc::new(Value::Error));
                    }
                };
                self.truncate_scopes(initial_size);

                let mut core_type = core_output_type;
                for (input_name, input_type) in core_inputs.into_iter().rev() {
                    core_type = core::Term::new(
                        Location::merge(input_name.location, output_type.location),
                        core::TermData::FunctionType(
                            Some(input_name.data),
                            Arc::new(input_type),
                            Arc::new(core_type),
                        ),
                    );
                }

                (core_type, Arc::new(Value::TypeType))
            }
            TermData::FunctionArrowType(input_type, output_type) => {
                let core_input_type = match self.is_type(input_type) {
                    Some(core_input_type) => core_input_type,
                    None => return (error_term(), Arc::new(Value::Error)),
                };
                let core_input_type_value = self.eval(&core_input_type);

                self.push_variable(None, core_input_type_value);
                let (core_term, r#type) = match self.is_type(output_type) {
                    Some(core_output_type) => (
                        core::Term::new(
                            term.location,
                            core::TermData::FunctionType(
                                None,
                                Arc::new(core_input_type),
                                Arc::new(core_output_type),
                            ),
                        ),
                        Arc::new(Value::TypeType),
                    ),
                    None => (error_term(), Arc::new(Value::Error)),
                };
                self.pop_scope();

                (core_term, r#type)
            }
            TermData::FunctionTerm(_, _) => {
                self.report(SurfaceToCoreMessage::AmbiguousTerm {
                    location: term.location,
                    term: AmbiguousTerm::FunctionTerm,
                });
                (error_term(), Arc::new(Value::Error))
            }
            TermData::FunctionElim(head_term, input_terms) => {
                let mut head_location = head_term.location;
                let (mut core_head_term, mut head_type) = self.synth_type(head_term);
                let mut input_terms = input_terms.iter();

                while let Some(input) = input_terms.next() {
                    match head_type.force(self.globals) {
                        Value::FunctionType(_, input_type, output_closure) => {
                            head_location = input.location;
                            let core_input = self.check_type(input, &input_type);
                            let core_input_value = self.eval(&core_input);
                            core_head_term = core::Term::new(
                                Location::merge(head_location, input.location),
                                core::TermData::FunctionElim(
                                    Arc::new(core_head_term),
                                    Arc::new(core_input),
                                ),
                            );
                            head_type = output_closure.apply(self.globals, core_input_value);
                        }
                        Value::Error => return (error_term(), Arc::new(Value::Error)),
                        _ => {
                            let head_type = self.read_back_to_surface(&head_type);
                            let unexpected_input_terms =
                                input_terms.map(|arg| arg.location).collect();
                            self.report(SurfaceToCoreMessage::TooManyInputsInFunctionElim {
                                head_location,
                                head_type,
                                unexpected_input_terms,
                            });
                            return (error_term(), Arc::new(Value::Error));
                        }
                    }
                }

                (core_head_term, head_type)
            }

            TermData::RecordTerm(term_entries) if term_entries.is_empty() => {
                let labels = Arc::new([]);
                let entries = Arc::new([]);

                let record_term_data = core::TermData::RecordTerm(labels.clone(), entries.clone());
                let record_type_data = core::TermData::RecordType(labels.clone(), entries.clone());
                let term_data = core::TermData::Ann(
                    Arc::new(core::Term::new(term.location, record_term_data)),
                    Arc::new(core::Term::new(term.location, record_type_data)),
                );
                let closure = RecordClosure::new(core::Env::new(), entries);

                (
                    core::Term::new(term.location, term_data),
                    Arc::from(Value::RecordType(labels, closure)),
                )
            }
            TermData::RecordTerm(_) => {
                self.report(SurfaceToCoreMessage::AmbiguousTerm {
                    location: term.location,
                    term: AmbiguousTerm::RecordTerm,
                });
                (error_term(), Arc::new(Value::Error))
            }
            TermData::RecordType(type_entries) => {
                use std::collections::btree_map::Entry;

                let initial_size = self.size();
                let mut duplicate_labels = Vec::new();
                let mut seen_labels = BTreeMap::new();
                let mut labels = Vec::with_capacity(type_entries.len());
                let mut core_types = Vec::with_capacity(type_entries.len());

                for (label, name, entry_type) in type_entries {
                    match seen_labels.entry(label.data.as_str()) {
                        Entry::Vacant(entry) => match self.is_type(entry_type) {
                            Some(core_type) => {
                                let param_name = name.as_ref().unwrap_or(label);
                                let core_type = Arc::new(core_type);
                                let core_type_value = self.eval(&core_type);

                                labels.push(label.data.clone());
                                core_types.push(core_type);
                                self.push_variable(Some(&param_name.data), core_type_value);
                                entry.insert(label.location);
                            }
                            None => {
                                self.truncate_scopes(initial_size);
                                return (error_term(), Arc::new(Value::Error));
                            }
                        },
                        Entry::Occupied(entry) => {
                            let seen_range = *entry.get();
                            let current_range = label.location;
                            duplicate_labels.push((label.data.clone(), seen_range, current_range));
                            self.is_type(entry_type);
                        }
                    }
                }

                if !duplicate_labels.is_empty() {
                    self.report(SurfaceToCoreMessage::InvalidRecordType { duplicate_labels });
                }

                self.truncate_scopes(initial_size);
                (
                    core::Term::new(
                        term.location,
                        core::TermData::RecordType(labels.into(), core_types.into()),
                    ),
                    Arc::new(Value::TypeType),
                )
            }
            TermData::RecordElim(head_term, label) => {
                let (core_head_term, head_type) = self.synth_type(head_term);

                match self.record_elim_type(&core_head_term, &head_type, &label.data) {
                    Some(entry_type) => match entry_type.as_ref() {
                        Value::Error => (error_term(), entry_type),
                        _ => {
                            let head_term = Arc::new(core_head_term);
                            let label = label.data.clone();
                            let term_data = core::TermData::RecordElim(head_term, label);
                            (core::Term::new(term.location, term_data), entry_type)
                        }
                    },
                    None => {
                        let head_type = self.read_back_to_surface(&head_type);
                        self.report(SurfaceToCoreMessage::LabelNotFound {
                            head_location: head_term.location,
                            label_location: label.location,
                            expected_label: label.data.clone(),
                            head_type,
                        });
                        (error_term(), Arc::new(Value::Error))
                    }
                }
            }

            TermData::SequenceTerm(_) => {
                self.report(SurfaceToCoreMessage::AmbiguousTerm {
                    location: term.location,
                    term: AmbiguousTerm::Sequence,
                });
                (error_term(), Arc::new(Value::Error))
            }

            TermData::NumberTerm(_) => {
                self.report(SurfaceToCoreMessage::AmbiguousTerm {
                    location: term.location,
                    term: AmbiguousTerm::NumberLiteral,
                });
                (error_term(), Arc::new(Value::Error))
            }
            TermData::CharTerm(data) => (
                self.parse_char(term.location, data),
                Arc::new(Value::global("Char", [])),
            ),
            TermData::StringTerm(data) => (
                self.parse_string(term.location, data),
                Arc::new(Value::global("String", [])),
            ),

            TermData::Error => (error_term(), Arc::new(Value::Error)),
        }
    }

    fn parse_float<T: Float + From<u8>>(
        &mut self,
        location: Location,
        data: &str,
        make_constant: fn(T) -> core::Constant,
    ) -> core::Term {
        let term_data = literal::State::new(location, data, &self.message_tx)
            .number_to_float()
            .map(make_constant)
            .map_or(core::TermData::Error, core::TermData::from);

        core::Term::new(location, term_data)
    }

    fn parse_unsigned<T: PrimInt + Unsigned>(
        &mut self,
        location: Location,
        source: &str,
        make_constant: fn(T) -> core::Constant,
    ) -> core::Term {
        let term_data = literal::State::new(location, source, &self.message_tx)
            .number_to_unsigned_int()
            .map(make_constant)
            .map_or(core::TermData::Error, core::TermData::from);

        core::Term::new(location, term_data)
    }

    fn parse_signed<T: PrimInt + Signed>(
        &mut self,
        location: Location,
        source: &str,
        make_constant: fn(T) -> core::Constant,
    ) -> core::Term {
        let term_data = literal::State::new(location, source, &self.message_tx)
            .number_to_signed_int()
            .map(make_constant)
            .map_or(core::TermData::Error, core::TermData::from);

        core::Term::new(location, term_data)
    }

    fn parse_char(&mut self, location: Location, source: &str) -> core::Term {
        let term_data = literal::State::new(location, source, &self.message_tx)
            .quoted_to_unicode_char()
            .map(core::Constant::Char)
            .map_or(core::TermData::Error, core::TermData::from);

        core::Term::new(location, term_data)
    }

    fn parse_string(&mut self, location: Location, source: &str) -> core::Term {
        let term_data = literal::State::new(location, source, &self.message_tx)
            .quoted_to_utf8_string()
            .map(core::Constant::String)
            .map_or(core::TermData::Error, core::TermData::from);

        core::Term::new(location, term_data)
    }
}
