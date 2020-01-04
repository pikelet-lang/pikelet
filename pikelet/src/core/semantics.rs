//! The operational semantics of the language.

use std::sync::Arc;

use crate::core::{Elim, Globals, Head, Locals, Term, Value};

/// Evaluate a term into a value in weak-head normal form.
pub fn eval_term(globals: &Globals, locals: &mut Locals, term: &Term) -> Value {
    match term {
        Term::Universe(level) => Value::universe((*level + locals.universe_offset()).unwrap()), // FIXME: Handle overflow
        Term::Global(name) => match globals.get(name) {
            Some((_, Some(term))) => eval_term(globals, locals, term),
            Some((r#type, None)) => {
                let r#type = eval_term(globals, locals, r#type);
                Value::global(name, locals.universe_offset(), r#type)
            }
            None => Value::Error,
        },
        Term::Constant(constant) => Value::Constant(constant.clone()),
        Term::Sequence(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|entry_term| Arc::new(eval_term(globals, locals, entry_term)))
                .collect();

            Value::Sequence(value_entries)
        }
        Term::Ann(term, _) => eval_term(globals, locals, term),
        Term::RecordType(type_entries) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| (name.clone(), Arc::new(eval_term(globals, locals, r#type))))
                .collect();

            Value::RecordType(type_entries)
        }
        Term::RecordTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|(name, term)| (name.clone(), Arc::new(eval_term(globals, locals, term))))
                .collect();

            Value::RecordTerm(value_entries)
        }
        Term::RecordElim(head, name) => match eval_term(globals, locals, head) {
            Value::RecordTerm(term_entries) => match term_entries.get(name) {
                Some(value) => (**value).clone(), // TODO: return `Arc<Value>`?
                None => Value::Error,
            },
            Value::Neutral(head, mut elims, r#type) => match r#type.as_ref() {
                Value::RecordType(type_entries) => {
                    match type_entries.iter().find(|(n, _)| n == name) {
                        Some((_, entry_type)) => {
                            elims.push(Elim::Record(name.clone()));
                            Value::Neutral(head, elims, entry_type.clone())
                        }
                        None => Value::Error,
                    }
                }
                _ => Value::Error,
            },
            _ => Value::Error,
        },
        Term::ArrayType(len, entry_type) => Value::ArrayType(
            Arc::new(eval_term(globals, locals, len)),
            Arc::new(eval_term(globals, locals, entry_type)),
        ),
        Term::ListType(r#type) => Value::ListType(Arc::new(eval_term(globals, locals, r#type))),
        Term::Lift(term, offset) => {
            let previous_offset = locals.universe_offset();
            locals.set_universe_offset((previous_offset + *offset).unwrap()); // FIXME: Handle overflow
            let value = eval_term(globals, locals, term);
            locals.set_universe_offset(previous_offset);
            value
        }
        Term::Error => Value::Error,
    }
}

/// Read-back a neutral value into the term syntax.
pub fn read_back_neutral(/* TODO: level, */ head: &Head, spine: &[Elim]) -> Term {
    let head = match head {
        Head::Global(name, shift) => Term::Global(name.clone()).lift(*shift),
    };

    spine.iter().fold(head, |head, elim| match elim {
        Elim::Record(name) => Term::RecordElim(Arc::new(head), name.clone()),
    })
}

/// Read-back a normal form into the term syntax.
///
/// This is type-directed to allow us to perform eta-conversion.
pub fn read_back_nf(/* TODO: level, */ value: &Value, r#type: &Value) -> Term {
    match (value, r#type) {
        (Value::Universe(level), Value::Universe(_)) => Term::Universe(*level),
        (Value::Neutral(head, spine, _), _) => read_back_neutral(head, spine),
        (Value::Constant(constant), _) => Term::Constant(constant.clone()),
        (Value::Sequence(value_entries), Value::ArrayType(_, entry_type)) => Term::Sequence(
            value_entries
                .iter()
                .map(|value_entry| Arc::new(read_back_nf(value_entry, entry_type)))
                .collect(),
        ),
        (Value::Sequence(value_entries), Value::ListType(entry_type)) => {
            let term_entries = value_entries
                .iter()
                .map(|value_entry| Arc::new(read_back_nf(value_entry, entry_type)))
                .collect();

            Term::Sequence(term_entries)
        }
        (Value::RecordType(type_entries), Value::Universe(level)) => {
            let type_entries = type_entries
                .iter()
                .map(|(name, r#type)| {
                    let r#type = Arc::new(read_back_nf(r#type, &Value::Universe(*level)));
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
        (Value::ArrayType(len, entry_type), Value::Universe(level)) => {
            let u32_type = Value::global("U32", 0, Value::universe(0));
            Term::ArrayType(
                Arc::new(read_back_nf(len, &u32_type)),
                Arc::new(read_back_nf(entry_type, &Value::Universe(*level))),
            )
        }
        (Value::ListType(entry_type), Value::Universe(level)) => {
            Term::ListType(Arc::new(read_back_nf(entry_type, &Value::Universe(*level))))
        }
        (Value::Universe(_), _)
        | (Value::Sequence(_), _)
        | (Value::RecordType(_), _)
        | (Value::RecordTerm(_), _)
        | (Value::ArrayType(_, _), _)
        | (Value::ListType(_), _)
        | (Value::Error, _) => Term::Error,
    }
}

/// Fully normalize a term.
pub fn normalize(globals: &Globals, locals: &mut Locals, term: &Term, r#type: &Value) -> Term {
    read_back_nf(&eval_term(globals, locals, term), r#type)
}

/// Check that two values are equal.
pub fn is_subtype(value0: &Value, value1: &Value) -> bool {
    match (value0, value1) {
        (Value::Universe(level0), Value::Universe(level1)) => level0 <= level1,
        (Value::Neutral(head0, spine0, type0), Value::Neutral(head1, spine1, type1)) => {
            read_back_neutral(head0, spine0) == read_back_neutral(head1, spine1)
                && is_subtype(type0, type1)
        }
        (Value::Constant(constant0), Value::Constant(constant1)) => constant0 == constant1,
        (Value::Sequence(value_entries0), Value::Sequence(value_entries1)) => {
            value_entries0.len() == value_entries1.len()
                && Iterator::zip(value_entries0.iter(), value_entries1.iter())
                    .all(|(term0, term1)| is_subtype(term0, term1))
        }
        (Value::RecordType(type_entries0), Value::RecordType(type_entries1)) => {
            type_entries0.len() == type_entries1.len()
                && Iterator::zip(type_entries0.iter(), type_entries1.iter()).all(
                    |((name0, type0), (name1, type1))| name0 == name1 && is_subtype(type0, type1),
                )
        }
        (Value::RecordTerm(value_entries0), Value::RecordTerm(value_entries1)) => {
            value_entries0.len() == value_entries1.len()
                && Iterator::zip(value_entries0.iter(), value_entries1.iter()).all(
                    |((name0, term0), (name1, term1))| name0 == name1 && is_subtype(term0, term1),
                )
        }
        (Value::ArrayType(len0, entry_type0), Value::ArrayType(len1, entry_type1)) => {
            is_subtype(len0, len1) && is_subtype(entry_type0, entry_type1)
        }
        (Value::ListType(entry_type0), Value::ListType(entry_type1)) => {
            is_subtype(entry_type0, entry_type1)
        }
        // Errors are always treated as subtypes, regardless of what they are compared with.
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
