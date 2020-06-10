//! The operational semantics of the language.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::lang::core::{
    Constant, Globals, LocalLevel, LocalSize, Locals, Term, UniverseLevel, UniverseOffset,
};

/// Values in the core language.
#[derive(Clone, Debug)]
pub enum Value {
    /// The type of types.
    Universe(UniverseLevel),
    /// A suspended elimination (neutral value).
    ///
    /// This is a value that cannot be reduced further as a result of being
    /// stuck on some head. Instead we maintain a 'spine' of eliminators so that
    /// we may perform further reduction later on.
    ///
    /// A type annotation is maintained in order to allow for type-directed
    /// [eta-conversion] to take place during read-back.
    ///
    /// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
    Elim(Head, Vec<Elim>, Arc<Value>),
    /// Constants.
    Constant(Constant),
    /// Ordered sequences.
    Sequence(Vec<Arc<Value>>),
    /// Record types.
    RecordType(RecordTypeClosure),
    /// Record terms.
    RecordTerm(BTreeMap<String, Arc<Value>>),
    /// Function types.
    FunctionType(Arc<Value>, Arc<Value>),
    /// Function terms (lambda abstractions).
    FunctionTerm(String, Closure),
    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a universe at the given level.
    pub fn universe(level: impl Into<UniverseLevel>) -> Value {
        Value::Universe(level.into())
    }

    /// Create a global variable.
    pub fn global(
        name: impl Into<String>,
        offset: impl Into<UniverseOffset>,
        r#type: impl Into<Arc<Value>>,
    ) -> Value {
        Value::Elim(
            Head::Global(name.into(), offset.into()),
            Vec::new(),
            r#type.into(),
        )
    }

    /// Create a local variable.
    pub fn local(level: impl Into<LocalLevel>, r#type: impl Into<Arc<Value>>) -> Value {
        Value::Elim(Head::Local(level.into()), Vec::new(), r#type.into())
    }
}

/// The head of an elimination.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Head {
    /// Global variables.
    Global(String, UniverseOffset),
    /// Local variables.
    Local(LocalLevel),
}

/// An eliminator, to be used in the spine of an elimination.
#[derive(Clone, Debug)]
pub enum Elim {
    /// Record eliminators (field access).
    Record(String),
    /// Function eliminatiors (function application).
    Function(Arc<Value>, Arc<Value>),
}

/// Closure, capturing the current universe offset and the current locals in scope.
#[derive(Clone, Debug)]
pub struct Closure {
    universe_offset: UniverseOffset,
    values: Locals<Arc<Value>>,
    term: Arc<Term>,
}

impl Closure {
    pub fn new(
        universe_offset: UniverseOffset,
        values: Locals<Arc<Value>>,
        term: Arc<Term>,
    ) -> Closure {
        Closure {
            universe_offset,
            values,
            term,
        }
    }

    /// Eliminate a closure.
    pub fn elim(&self, globals: &Globals, argument: Arc<Value>) -> Arc<Value> {
        let mut values = self.values.clone();
        values.push(argument);
        eval_term(globals, self.universe_offset, &mut values, &self.term)
    }
}

/// Record type closure, capturing the current universe offset and the current locals in scope.
#[derive(Clone, Debug)]
pub struct RecordTypeClosure {
    universe_offset: UniverseOffset,
    values: Locals<Arc<Value>>,
    entries: Arc<[(String, Arc<Term>)]>,
}

impl RecordTypeClosure {
    pub fn new(
        universe_offset: UniverseOffset,
        values: Locals<Arc<Value>>,
        entries: Arc<[(String, Arc<Term>)]>,
    ) -> RecordTypeClosure {
        RecordTypeClosure {
            universe_offset,
            values,
            entries,
        }
    }

    /// Apply a callback to each of the entry types in the record closure.
    pub fn entries<'closure>(
        &'closure self,
        globals: &Globals,
        mut on_entry: impl FnMut(&'closure str, Arc<Value>) -> Arc<Value>,
    ) {
        let universe_offset = self.universe_offset;
        let mut values = self.values.clone();
        for (entry_name, entry_type) in self.entries.iter() {
            let entry_type = eval_term(globals, universe_offset, &mut values, entry_type);
            values.push(on_entry(entry_name, entry_type));
        }
    }
}

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
        Term::RecordType(type_entries) => Arc::new(Value::RecordType(RecordTypeClosure::new(
            universe_offset,
            values.clone(),
            type_entries.clone(),
        ))),
        Term::RecordTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|(entry_name, entry_term)| {
                    let entry_term = eval_term(globals, universe_offset, values, entry_term);
                    (entry_name.clone(), entry_term)
                })
                .collect();

            Arc::new(Value::RecordTerm(value_entries))
        }
        Term::RecordElim(head, name) => {
            let head = eval_term(globals, universe_offset, values, head);
            eval_record_elim(globals, &head, name)
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

/// Return the type of the record elimination.
pub fn record_elim_type(
    globals: &Globals,
    head_value: &Value,
    name: &str,
    closure: &RecordTypeClosure,
) -> Option<Arc<Value>> {
    let universe_offset = closure.universe_offset;
    let mut values = closure.values.clone();
    for (entry_name, entry_type) in closure.entries.iter() {
        if name == entry_name {
            return Some(eval_term(globals, universe_offset, &mut values, entry_type));
        }
        values.push(eval_record_elim(globals, head_value, entry_name));
    }
    None
}

/// Eliminate a record term.
pub fn eval_record_elim(globals: &Globals, head_value: &Value, name: &str) -> Arc<Value> {
    match head_value {
        Value::RecordTerm(term_entries) => match term_entries.get(name) {
            Some(value) => value.clone(),
            None => Arc::new(Value::Error),
        },
        Value::Elim(head, elims, r#type) => {
            if let Value::RecordType(closure) = r#type.as_ref() {
                if let Some(entry_type) = record_elim_type(globals, head_value, name, closure) {
                    let mut elims = elims.clone(); // FIXME: Avoid clone of elims?
                    elims.push(Elim::Record(name.to_owned()));
                    return Arc::new(Value::Elim(head.clone(), elims, entry_type));
                }
            }
            Arc::new(Value::Error)
        }
        _ => Arc::new(Value::Error),
    }
}

/// Eliminate a function term.
pub fn eval_fun_elim(globals: &Globals, head_value: &Value, argument: Arc<Value>) -> Arc<Value> {
    match head_value {
        Value::FunctionTerm(_, body_closure) => body_closure.elim(globals, argument),
        Value::Elim(head, elims, r#type) => {
            if let Value::FunctionType(param_type, body_type) = r#type.as_ref() {
                let mut elims = elims.clone(); // FIXME: Avoid clone of elims?
                elims.push(Elim::Function(argument, param_type.clone()));
                return Arc::new(Value::Elim(head.clone(), elims, body_type.clone()));
            }
            Arc::new(Value::Error)
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
        (_, Value::Universe(_)) => read_back_type(globals, local_size, value),
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
        (Value::RecordTerm(_), Value::RecordType(closure)) => {
            let mut term_entries = BTreeMap::new();

            closure.entries(globals, |entry_name, entry_type| {
                let entry_value = eval_record_elim(globals, value, entry_name);
                let entry_term = read_back_nf(globals, local_size, &entry_value, &entry_type);
                term_entries.insert(entry_name.to_owned(), Arc::new(entry_term));
                entry_value
            });

            Term::RecordTerm(term_entries)
        }
        (
            Value::FunctionTerm(param_name_hint, body_closure),
            Value::FunctionType(param_type, body_type),
        ) => {
            let argument = Arc::from(Value::local(local_size.next_level(), param_type.clone()));
            let body = body_closure.elim(globals, argument);
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
        Value::RecordType(closure) => {
            let mut local_size = local_size;
            let mut type_entries = Vec::with_capacity(closure.entries.len());

            closure.entries(globals, |entry_name, entry_type| {
                type_entries.push((
                    entry_name.to_owned(),
                    Arc::new(read_back_type(globals, local_size, &entry_type)),
                ));

                let local_level = local_size.next_level();
                local_size = local_size.increment();

                Arc::new(Value::local(local_level, entry_type))
            });

            Term::RecordType(type_entries.into())
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
        (Value::RecordType(closure0), Value::RecordType(closure1)) => {
            let mut local_size = local_size;
            let universe_offset0 = closure0.universe_offset;
            let universe_offset1 = closure1.universe_offset;
            let mut values0 = closure0.values.clone();
            let mut values1 = closure1.values.clone();

            closure0.entries.len() == closure1.entries.len()
                && Iterator::zip(closure0.entries.iter(), closure1.entries.iter()).all(
                    |((entry_name0, entry_type0), (entry_name1, entry_type1))| {
                        entry_name0 == entry_name1 && {
                            let entry_type0 =
                                eval_term(globals, universe_offset0, &mut values0, entry_type0);
                            let entry_type1 =
                                eval_term(globals, universe_offset1, &mut values1, entry_type1);
                            let cmp = compare_types(
                                globals,
                                local_size,
                                &entry_type0,
                                &entry_type1,
                                compare,
                            );

                            let local_level = local_size.next_level();
                            values0.push(Arc::new(Value::local(local_level, entry_type0)));
                            values1.push(Arc::new(Value::local(local_level, entry_type1)));
                            local_size = local_size.increment();

                            cmp
                        }
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
