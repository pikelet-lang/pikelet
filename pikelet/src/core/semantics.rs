//! The operational semantics of the language.

use std::sync::Arc;

use crate::core::{
    Closure, Elim, Globals, Head, LocalSize, Locals, Term, UniverseLevel, UniverseOffset, Value,
};

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
        Term::RecordElim(head, field_name) => {
            let head = eval_term(globals, universe_offset, values, head);
            eval_record_elim(&head, field_name)
        }
        Term::FunctionType(param_type, body_type) => {
            let param_type = eval_term(globals, universe_offset, values, param_type);
            let body_type = eval_term(globals, universe_offset, values, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        Term::FunctionTerm(param_name, body) => Arc::new(Value::FunctionTerm(
            param_name.clone(),
            Closure::new(universe_offset, values.clone(), body.clone()),
        )),
        Term::FunctionElim(head, argument) => {
            let head = eval_term(globals, universe_offset, values, head);
            let argument = eval_term(globals, universe_offset, values, argument);
            eval_fun_elim(globals, &head, argument)
        }
        Term::Lift(term, offset) => {
            let universe_offset = (universe_offset + *offset).unwrap(); // FIXME: Handle overflow
            eval_term(globals, universe_offset, values, term)
        }
        Term::Error => Arc::new(Value::Error),
    }
}

/// Instantiate a closure in an environment of the given size.
pub fn instantiate_closure(
    globals: &Globals,
    local_size: LocalSize,
    r#type: Arc<Value>,
    closure: &Closure,
) -> Arc<Value> {
    let argument = Arc::from(Value::local(local_size.next_level(), r#type));
    eval_closure_elim(globals, closure, argument)
}

/// Eliminate a closure.
pub fn eval_closure_elim(globals: &Globals, closure: &Closure, argument: Arc<Value>) -> Arc<Value> {
    let mut values = closure.values.clone();
    values.push(argument);
    eval_term(globals, closure.universe_offset, &mut values, &closure.term)
}

/// Eliminate a record term.
pub fn eval_record_elim(head: &Value, name: &str) -> Arc<Value> {
    match head {
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
            elims.push(Elim::Record(name.to_owned()));
            Arc::new(Value::Elim(head.clone(), elims, entry_type.clone()))
        }
        _ => Arc::new(Value::Error),
    }
}

/// Eliminate a function term.
pub fn eval_fun_elim(globals: &Globals, head: &Value, argument: Arc<Value>) -> Arc<Value> {
    match head {
        Value::FunctionTerm(_, body_closure) => eval_closure_elim(globals, body_closure, argument),
        Value::Elim(head, elims, r#type) => {
            let (param_type, body_type) = match r#type.as_ref() {
                Value::FunctionType(param_type, body_type) => (param_type, body_type),
                _ => return Arc::new(Value::Error),
            };

            let mut elims = elims.clone(); // TODO: Avoid clone?
            elims.push(Elim::Function(argument, param_type.clone()));
            Arc::new(Value::Elim(head.clone(), elims, body_type.clone()))
        }
        _ => Arc::new(Value::Error),
    }
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

/// Check that one elimination is equal to another elimination.
pub fn is_equal_elim(
    globals: &Globals,
    local_size: LocalSize,
    (head0, spine0): (&Head, &[Elim]),
    (head1, spine1): (&Head, &[Elim]),
) -> bool {
    head0 == head1
        && spine0.len() == spine1.len()
        && Iterator::zip(spine0.iter(), spine1.iter()).all(|(elim0, elim1)| match (elim0, elim1) {
            (Elim::Function(argument0, type0), Elim::Function(argument1, type1)) => {
                is_equal_nf(globals, local_size, (argument0, type0), (argument1, type1))
            }
            (Elim::Record(field0), Elim::Record(field1)) => field0 == field1,
            (_, _) => false,
        })
}

/// Check that one normal form is a equal of another normal form.
pub fn is_equal_nf(
    globals: &Globals,
    local_size: LocalSize,
    (value0, type0): (&Value, &Value),
    (value1, type1): (&Value, &Value),
) -> bool {
    // TODO: avoid allocation of intermediate term, as in smalltt and blott,
    // for example, see: https://github.com/jozefg/blott/blob/9eadd6f1eb3ecb28fd66a25bc56c19041d98f722/src/lib/nbe.ml#L200-L242

    read_back_nf(globals, local_size, value0, type0)
        == read_back_nf(globals, local_size, value1, type1)
}

/// Compare two types.
fn compare_types(
    globals: &Globals,
    local_size: LocalSize,
    value0: &Value,
    value1: &Value,
    compare: &impl Fn(UniverseLevel, UniverseLevel) -> bool,
) -> bool {
    match (value0, value1) {
        (Value::Universe(level0), Value::Universe(level1)) => compare(*level0, *level1),
        (Value::Elim(head0, spine0, _), Value::Elim(head1, spine1, _)) => {
            is_equal_elim(globals, local_size, (head0, spine0), (head1, spine1))
        }
        (Value::RecordType(type_entries0), Value::RecordType(type_entries1)) => {
            type_entries0.len() == type_entries1.len()
                && Iterator::zip(type_entries0.iter(), type_entries1.iter()).all(
                    |((name0, type0), (name1, type1))| {
                        name0 == name1 && compare_types(globals, local_size, type0, type1, compare)
                    },
                )
        }
        (
            Value::FunctionType(param_type0, body_type0),
            Value::FunctionType(param_type1, body_type1),
        ) => {
            compare_types(globals, local_size, param_type1, param_type0, compare)
                && compare_types(globals, local_size, body_type0, body_type1, compare)
        }
        // Errors are always treated as subtypes, regardless of what they are compared with.
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}

/// Check that one type is a equal to another type.
pub fn is_equal_type(
    globals: &Globals,
    local_size: LocalSize,
    value0: &Value,
    value1: &Value,
) -> bool {
    compare_types(globals, local_size, value0, value1, &|l0, l1| l0 == l1)
}

/// Check that one type is a subtype of another type.
pub fn is_subtype(
    globals: &Globals,
    local_size: LocalSize,
    value0: &Value,
    value1: &Value,
) -> bool {
    compare_types(globals, local_size, value0, value1, &|l0, l1| l0 <= l1)
}
