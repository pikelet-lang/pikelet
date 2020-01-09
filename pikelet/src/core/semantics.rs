//! The operational semantics of the language.

use std::sync::Arc;

use crate::core::{Elim, Globals, Head, Locals, Term, Value};

/// Evaluate a term into a value in weak-head normal form.
pub fn eval_term(globals: &Globals, locals: &mut Locals, term: &Term) -> Arc<Value> {
    match term {
        Term::Universe(level) => Arc::new(Value::universe(
            (*level + locals.universe_offset()).unwrap(), // FIXME: Handle overflow
        )),
        Term::Global(name) => match globals.get(name) {
            Some((_, Some(term))) => eval_term(globals, locals, term),
            Some((r#type, None)) => {
                let r#type = eval_term(globals, locals, r#type);
                Arc::new(Value::global(name, locals.universe_offset(), r#type))
            }
            None => Arc::new(Value::Error),
        },
        Term::Constant(constant) => Arc::new(Value::Constant(constant.clone())),
        Term::Sequence(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|entry_term| eval_term(globals, locals, entry_term))
                .collect();

            Arc::new(Value::Sequence(value_entries))
        }
        Term::Ann(term, _) => eval_term(globals, locals, term),
        Term::RecordType(type_entries) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| (name.clone(), eval_term(globals, locals, r#type)))
                .collect();

            Arc::new(Value::RecordType(type_entries))
        }
        Term::RecordTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|(name, term)| (name.clone(), eval_term(globals, locals, term)))
                .collect();

            Arc::new(Value::RecordTerm(value_entries))
        }
        Term::RecordElim(head, name) => match eval_term(globals, locals, head).as_ref() {
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
        },
        Term::FunctionType(param_type, body_type) => {
            let param_type = eval_term(globals, locals, param_type);
            let body_type = eval_term(globals, locals, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        Term::FunctionElim(head, argument) => match eval_term(globals, locals, head).as_ref() {
            // TODO: Value::FunctionTerm(body)
            Value::Elim(head, elims, r#type) => {
                let (param_type, body_type) = match r#type.as_ref() {
                    Value::FunctionType(param_type, body_type) => (param_type, body_type),
                    _ => return Arc::new(Value::Error),
                };

                let mut elims = elims.clone(); // TODO: Avoid clone?
                elims.push(Elim::Function(
                    eval_term(globals, locals, argument),
                    param_type.clone(),
                ));
                Arc::new(Value::Elim(head.clone(), elims, body_type.clone()))
            }
            _ => Arc::new(Value::Error),
        },
        Term::Lift(term, offset) => {
            let previous_offset = locals.universe_offset();
            locals.set_universe_offset((previous_offset + *offset).unwrap()); // FIXME: Handle overflow
            let value = eval_term(globals, locals, term);
            locals.set_universe_offset(previous_offset);
            value
        }
        Term::Error => Arc::new(Value::Error),
    }
}

/// Read-back an eliminator into the term syntax.
pub fn read_back_elim(/* TODO: level, */ head: &Head, spine: &[Elim]) -> Term {
    let head = match head {
        Head::Global(name, shift) => Term::Global(name.clone()).lift(*shift),
    };

    spine.iter().fold(head, |head, elim| match elim {
        Elim::Record(name) => Term::RecordElim(Arc::new(head), name.clone()),
        Elim::Function(argument, argument_type) => Term::FunctionElim(
            Arc::new(head),
            Arc::new(read_back_nf(argument, argument_type)),
        ),
    })
}

/// Read-back a normal form into the term syntax.
///
/// This is type-directed to allow us to perform [eta-conversion].
///
/// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
pub fn read_back_nf(/* TODO: level, */ value: &Value, r#type: &Value) -> Term {
    match (value, r#type) {
        (Value::Universe(level), Value::Universe(_)) => Term::Universe(*level),
        (Value::Elim(head, spine, _), _) => read_back_elim(head, spine),
        (Value::Constant(constant), _) => Term::Constant(constant.clone()),
        (Value::Sequence(value_entries), Value::Elim(head, elims, _)) => match head {
            Head::Global(name, _) => match (name.as_ref(), elims.as_slice()) {
                ("Array", [Elim::Function(_, _), Elim::Function(entry_type, _)])
                | ("List", [Elim::Function(entry_type, _)]) => {
                    let term_entries = value_entries
                        .iter()
                        .map(|value_entry| Arc::new(read_back_nf(value_entry, entry_type)))
                        .collect();

                    Term::Sequence(term_entries)
                }
                _ => Term::Error, // TODO: Report error
            },
        },
        (Value::RecordType(type_entries), Value::Universe(_)) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let r#type = Arc::new(read_back_type(r#type));
                    (name.clone(), r#type)
                })
                .collect();

            Term::RecordType(type_entries)
        }
        (Value::RecordTerm(value_entries), Value::RecordType(type_entries)) => {
            let term_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let term = Arc::new(read_back_nf(&value_entries[name], r#type));
                    (name.clone(), term)
                })
                .collect();

            Term::RecordTerm(term_entries)
        }
        (Value::FunctionType(param_type, body_type), Value::Universe(_)) => Term::FunctionType(
            Arc::new(read_back_type(param_type)),
            Arc::new(read_back_type(body_type)),
        ),
        (Value::Universe(_), _)
        | (Value::Sequence(_), _)
        | (Value::RecordType(_), _)
        | (Value::RecordTerm(_), _)
        | (Value::FunctionType(_, _), _) => Term::Error, // TODO: Report error
        (Value::Error, _) => Term::Error,
    }
}

/// Read-back a type into the term syntax.
pub fn read_back_type(/* TODO: level, */ r#type: &Value) -> Term {
    match r#type {
        Value::Universe(level) => Term::Universe(*level),
        Value::Elim(head, spine, _) => read_back_elim(head, spine),
        Value::RecordType(type_entries) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| (name.clone(), Arc::new(read_back_type(r#type))))
                .collect();

            Term::RecordType(type_entries)
        }
        Value::FunctionType(param_type, body_type) => Term::FunctionType(
            Arc::new(read_back_type(param_type)),
            Arc::new(read_back_type(body_type)),
        ),
        Value::Constant(_) | Value::Sequence(_) | Value::RecordTerm(_) => Term::Error, // TODO: Report error
        Value::Error => Term::Error,
    }
}

/// Fully normalize a term.
pub fn normalize_term(globals: &Globals, locals: &mut Locals, term: &Term, r#type: &Value) -> Term {
    read_back_nf(&eval_term(globals, locals, term), r#type)
}

/// Check that one type is a subtype of another type.
pub fn is_subtype(value0: &Value, value1: &Value) -> bool {
    match (value0, value1) {
        (Value::Universe(level0), Value::Universe(level1)) => level0 <= level1,
        (Value::Elim(head0, spine0, type0), Value::Elim(head1, spine1, type1)) => {
            read_back_elim(head0, spine0) == read_back_elim(head1, spine1)
                && is_subtype(type0, type1)
        }
        (Value::RecordType(type_entries0), Value::RecordType(type_entries1)) => {
            type_entries0.len() == type_entries1.len()
                && Iterator::zip(type_entries0.iter(), type_entries1.iter()).all(
                    |((name0, type0), (name1, type1))| name0 == name1 && is_subtype(type0, type1),
                )
        }
        (
            Value::FunctionType(param_type0, body_type0),
            Value::FunctionType(param_type1, body_type1),
        ) => is_subtype(param_type1, param_type0) && is_subtype(body_type0, body_type1),
        // Errors are always treated as subtypes, regardless of what they are compared with.
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
