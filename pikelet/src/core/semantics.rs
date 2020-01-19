//! The operational semantics of the language.

use std::sync::Arc;

use crate::core::{Closure, Elim, Globals, Head, LocalSize, Locals, Term, UniverseOffset, Value};

/// Fully normalize a term.
pub fn normalize_term(
    globals: &Globals,
    universe_offset: UniverseOffset,
    values: &mut Locals<Arc<Value>>,
    term: &Term,
    r#type: &Value,
) -> Term {
    let value = eval_term(globals, universe_offset, values, term);
    read_back_nf(globals, values.size(), &value, r#type)
}

/// Evaluate a term into a value in weak-head normal form.
pub fn eval_term(
    globals: &Globals,
    universe_offset: UniverseOffset,
    values: &mut Locals<Arc<Value>>,
    term: &Term,
) -> Arc<Value> {
    match term {
        Term::Universe(level) => Arc::new(Value::universe(
            (*level + universe_offset).unwrap(), // FIXME: Handle overflow
        )),
        Term::Global(name) => match globals.get(name) {
            Some((_, Some(term))) => eval_term(globals, universe_offset, values, term),
            Some((r#type, None)) => {
                let r#type = eval_term(globals, universe_offset, values, r#type);
                Arc::new(Value::global(name, universe_offset, r#type))
            }
            None => Arc::new(Value::Error),
        },
        Term::Local(index) => match values.get(*index) {
            Some(value) => value.clone(),
            None => Arc::new(Value::Error),
        },
        Term::Constant(constant) => Arc::new(Value::Constant(constant.clone())),
        Term::Sequence(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|entry_term| eval_term(globals, universe_offset, values, entry_term))
                .collect();

            Arc::new(Value::Sequence(value_entries))
        }
        Term::Ann(term, _) => eval_term(globals, universe_offset, values, term),
        Term::RecordType(type_entries) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let r#type = eval_term(globals, universe_offset, values, r#type);
                    (name.clone(), r#type)
                })
                .collect();

            Arc::new(Value::RecordType(type_entries))
        }
        Term::RecordTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|(name, term)| {
                    let term = eval_term(globals, universe_offset, values, term);
                    (name.clone(), term)
                })
                .collect();

            Arc::new(Value::RecordTerm(value_entries))
        }
        Term::RecordElim(head, name) => {
            match eval_term(globals, universe_offset, values, head).as_ref() {
                Value::RecordTerm(term_entries) => match term_entries.get(name) {
                    Some(value) => value.clone(),
                    None => Arc::new(Value::Error),
                },
                Value::Elim(head, elims, r#type) => {
                    let type_entries = match r#type.as_ref() {
                        Value::RecordType(type_entries) => type_entries,
                        _ => return Arc::new(Value::Error),
                    };
                    let entry_type = match type_entries.iter().find(|(n, _)| n == name) {
                        Some((_, entry_type)) => entry_type,
                        None => return Arc::new(Value::Error),
                    };

                    let mut elims = elims.clone(); // TODO: Avoid clone?
                    elims.push(Elim::Record(name.clone()));
                    Arc::new(Value::Elim(head.clone(), elims, entry_type.clone()))
                }
                _ => Arc::new(Value::Error),
            }
        }
        Term::FunctionType(param_type, body_type) => {
            let param_type = eval_term(globals, universe_offset, values, param_type);
            let body_type = eval_term(globals, universe_offset, values, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        Term::FunctionTerm(param_name, body) => Arc::new(Value::FunctionTerm(
            param_name.clone(),
            Closure::new(
                universe_offset,
                values.clone(), // FIXME: This clone could be expensive
                body.clone(),
            ),
        )),
        Term::FunctionElim(head, argument) => {
            match eval_term(globals, universe_offset, values, head).as_ref() {
                Value::FunctionTerm(_, body_closure) => {
                    let argument = eval_term(globals, universe_offset, values, argument);
                    apply_closure(globals, body_closure, argument)
                }
                Value::Elim(head, elims, r#type) => {
                    let (param_type, body_type) = match r#type.as_ref() {
                        Value::FunctionType(param_type, body_type) => (param_type, body_type),
                        _ => return Arc::new(Value::Error),
                    };

                    let mut elims = elims.clone(); // TODO: Avoid clone?
                    elims.push(Elim::Function(
                        eval_term(globals, universe_offset, values, argument),
                        param_type.clone(),
                    ));
                    Arc::new(Value::Elim(head.clone(), elims, body_type.clone()))
                }
                _ => Arc::new(Value::Error),
            }
        }
        Term::Lift(term, offset) => {
            let universe_offset = (universe_offset + *offset).unwrap(); // FIXME: Handle overflow
            eval_term(globals, universe_offset, values, term)
        }
        Term::Error => Arc::new(Value::Error),
    }
}

/// Apply a closure to an argument.
pub fn apply_closure(globals: &Globals, closure: &Closure, argument: Arc<Value>) -> Arc<Value> {
    let mut values = closure.values.clone();
    values.push(argument);
    eval_term(globals, closure.universe_offset, &mut values, &closure.term)
}

/// Instantiate a closure in an environment of the given size.
pub fn instantiate_closure(
    globals: &Globals,
    local_size: LocalSize,
    r#type: Arc<Value>,
    closure: &Closure,
) -> Arc<Value> {
    let argument = Arc::from(Value::local(local_size.next_level(), r#type));
    apply_closure(globals, closure, argument)
}

/// Read-back an eliminator into the term syntax.
pub fn read_back_elim(
    globals: &Globals,
    local_size: LocalSize,
    head: &Head,
    spine: &[Elim],
) -> Term {
    let head = match head {
        Head::Global(name, shift) => Term::Global(name.clone()).lift(*shift),
        Head::Local(level) => Term::Local(local_size.index(*level)),
    };

    spine.iter().fold(head, |head, elim| match elim {
        Elim::Record(name) => Term::RecordElim(Arc::new(head), name.clone()),
        Elim::Function(argument, argument_type) => Term::FunctionElim(
            Arc::new(head),
            Arc::new(read_back_nf(globals, local_size, argument, argument_type)),
        ),
    })
}

/// Read-back a normal form into the term syntax.
///
/// This is type-directed to allow us to perform [eta-conversion].
///
/// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
pub fn read_back_nf(
    globals: &Globals,
    local_size: LocalSize,
    value: &Value,
    r#type: &Value,
) -> Term {
    match (value, r#type) {
        (Value::Universe(level), Value::Universe(_)) => Term::Universe(*level),
        (Value::Elim(head, spine, _), _) => read_back_elim(globals, local_size, head, spine),
        (Value::Constant(constant), _) => Term::Constant(constant.clone()),
        (Value::Sequence(value_entries), Value::Elim(Head::Global(name, _), elims, _)) => {
            match (name.as_ref(), elims.as_slice()) {
                ("Array", [Elim::Function(_, _), Elim::Function(entry_type, _)])
                | ("List", [Elim::Function(entry_type, _)]) => {
                    let term_entries = value_entries
                        .iter()
                        .map(|value_entry| {
                            Arc::new(read_back_nf(globals, local_size, value_entry, entry_type))
                        })
                        .collect();

                    Term::Sequence(term_entries)
                }
                _ => Term::Error, // TODO: Report error
            }
        }
        (Value::RecordType(type_entries), Value::Universe(_)) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let r#type = Arc::new(read_back_type(globals, local_size, r#type));
                    (name.clone(), r#type)
                })
                .collect();

            Term::RecordType(type_entries)
        }
        (Value::RecordTerm(value_entries), Value::RecordType(type_entries)) => {
            let term_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let value = &value_entries[name];
                    let term = Arc::new(read_back_nf(globals, local_size, value, r#type));
                    (name.clone(), term)
                })
                .collect();

            Term::RecordTerm(term_entries)
        }
        (Value::FunctionType(param_type, body_type), Value::Universe(_)) => Term::FunctionType(
            Arc::new(read_back_type(globals, local_size, param_type)),
            Arc::new(read_back_type(globals, local_size, body_type)),
        ),
        (
            Value::FunctionTerm(param_name_hint, body_closure),
            Value::FunctionType(param_type, body_type),
        ) => {
            let body = instantiate_closure(globals, local_size, param_type.clone(), body_closure);
            let body = read_back_nf(globals, LocalSize(local_size.0 + 1), &body, body_type);

            Term::FunctionTerm(param_name_hint.clone(), Arc::new(body))
        }
        (Value::Universe(_), _)
        | (Value::Sequence(_), _)
        | (Value::RecordType(_), _)
        | (Value::RecordTerm(_), _)
        | (Value::FunctionType(_, _), _)
        | (Value::FunctionTerm(_, _), _) => Term::Error, // TODO: Report error
        (Value::Error, _) => Term::Error,
    }
}

/// Read-back a type into the term syntax.
pub fn read_back_type(globals: &Globals, local_size: LocalSize, r#type: &Value) -> Term {
    match r#type {
        Value::Universe(level) => Term::Universe(*level),
        Value::Elim(head, spine, _) => read_back_elim(globals, local_size, head, spine),
        Value::RecordType(type_entries) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let r#type = Arc::new(read_back_type(globals, local_size, r#type));
                    (name.clone(), r#type)
                })
                .collect();

            Term::RecordType(type_entries)
        }
        Value::FunctionType(param_type, body_type) => Term::FunctionType(
            Arc::new(read_back_type(globals, local_size, param_type)),
            Arc::new(read_back_type(globals, local_size, body_type)),
        ),
        Value::Constant(_)
        | Value::Sequence(_)
        | Value::RecordTerm(_)
        | Value::FunctionTerm(_, _) => {
            Term::Error // TODO: Report error
        }
        Value::Error => Term::Error,
    }
}

/// Check that one type is a subtype of another type.
pub fn is_subtype(
    globals: &Globals,
    local_size: LocalSize,
    value0: &Value,
    value1: &Value,
) -> bool {
    match (value0, value1) {
        (Value::Universe(level0), Value::Universe(level1)) => level0 <= level1,
        (Value::Elim(head0, spine0, type0), Value::Elim(head1, spine1, type1)) => {
            read_back_elim(globals, local_size, head0, spine0)
                == read_back_elim(globals, local_size, head1, spine1)
                && is_subtype(globals, local_size, type0, type1)
        }
        (Value::RecordType(type_entries0), Value::RecordType(type_entries1)) => {
            type_entries0.len() == type_entries1.len()
                && Iterator::zip(type_entries0.iter(), type_entries1.iter()).all(
                    |((name0, type0), (name1, type1))| {
                        name0 == name1 && is_subtype(globals, local_size, type0, type1)
                    },
                )
        }
        (
            Value::FunctionType(param_type0, body_type0),
            Value::FunctionType(param_type1, body_type1),
        ) => {
            is_subtype(globals, local_size, param_type1, param_type0)
                && is_subtype(globals, local_size, body_type0, body_type1)
        }
        // Errors are always treated as subtypes, regardless of what they are compared with.
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
