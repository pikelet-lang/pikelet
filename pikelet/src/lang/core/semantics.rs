//! The operational semantics of the language, implemented using [normalisation-by-evaluation].
//!
//! [normalisation-by-evaluation]: https://en.wikipedia.org/wiki/Normalisation_by_evaluation

use contracts::debug_ensures;
use once_cell::sync::OnceCell;
use std::cell::RefCell;
use std::sync::Arc;

use crate::lang::core::{Constant, Env, EnvSize, Globals, Term, TermData, VarLevel};

/// Values in the core language.
#[derive(Clone, Debug)]
pub enum Value {
    /// A computation that is stuck on a [head value][Head] that cannot be
    /// reduced further in the current scope. We maintain a 'spine' of
    /// [eliminators][Elim], that can be applied if the head becomes unstuck
    /// later on.
    ///
    /// This is more commonly known as a 'neutral value' in the type theory
    /// literature.
    Stuck(Head, Vec<Elim>),
    /// A computation that was previously stuck a on [head value][Head], but is
    /// now unstuck due to its definition now being known.
    ///
    /// This is sometimes called a 'glued value'.
    ///
    /// It's useful to keep the head and spine of eliminations around from the
    /// [stuck value][Value::Stuck] in order to reduce the size-blowup that
    /// can result from deeply-normalizing terms. This can be useful for:
    ///
    /// - improving the performance of conversion checking
    /// - making it easier to understand read-back types in diagnostic messages
    ///
    /// See the following for more information:
    ///
    /// - [AndrasKovacs/smalltt](https://github.com/AndrasKovacs/smalltt/)
    /// - [ollef/sixty](https://github.com/ollef/sixty/)
    /// - [Non-deterministic normalization-by-evaluation](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784)
    /// - [Example of the blowup that can occur when reading back values](https://twitter.com/brendanzab/status/1283278258818002944)
    Unstuck(Head, Vec<Elim>, Arc<LazyValue>),

    /// The type of types.
    TypeType,

    /// Function types.
    ///
    /// Also known as: pi type, dependent product type.
    FunctionType(Option<String>, Arc<Value>, FunctionClosure),
    /// Function terms.
    ///
    /// Also known as: lambda abstraction, anonymous function.
    FunctionTerm(String, FunctionClosure),

    /// Record types.
    RecordType(Arc<[String]>, RecordClosure),
    /// Record terms.
    RecordTerm(Arc<[String]>, RecordClosure),

    /// Array terms.
    ArrayTerm(Vec<Arc<Value>>),
    /// List terms.
    ListTerm(Vec<Arc<Value>>),

    /// Constants.
    Constant(Constant),

    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a global variable.
    pub fn global(name: impl Into<String>, elims: impl Into<Vec<Elim>>) -> Value {
        Value::Stuck(Head::Global(name.into()), elims.into())
    }

    /// Create a variable.
    pub fn var(var_level: impl Into<VarLevel>, elims: impl Into<Vec<Elim>>) -> Value {
        Value::Stuck(Head::Var(var_level.into()), elims.into())
    }

    /// Attempt to match against a stuck global.
    ///
    /// This can help to clean up pattern matches in lieu of
    /// [`match_default_bindings`](https://github.com/rust-lang/rust/issues/42640).
    pub fn try_global(&self) -> Option<(&str, &[Elim])> {
        match self {
            Value::Stuck(Head::Global(name), elims) => Some((name, elims)),
            _ => None,
        }
    }

    /// Force any unstuck values.
    pub fn force(&self, globals: &Globals) -> &Value {
        match self {
            Value::Unstuck(_, _, value) => Value::force(LazyValue::force(value, globals), globals),
            value => value,
        }
    }
}

/// The head of a [stuck value][Value::Stuck].
///
/// This cannot currently be reduced in the current scope due to its definition
/// not being known. Once it becomes known, the head may be 'remembered' in an
/// [unstuck value][Value::Unstuck].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Head {
    /// Global variables.
    Global(String),
    /// Local variables.
    Var(VarLevel),
}

/// An eliminator that is part of the spine of a [stuck value][`Value::Stuck`].
///
/// It might also be 'remembered' in an [unstuck value][Value::Unstuck].
#[derive(Clone, Debug)]
pub enum Elim {
    /// Function eliminators.
    ///
    /// This eliminator can be applied to a [`Value`] with the
    /// [`apply_function_elim`] function.
    ///
    /// Also known as: function application.
    Function(Arc<LazyValue>),
    /// Record eliminators.
    ///
    /// This eliminator can be applied to a [`Value`] with the
    /// [`apply_record_elim`] function.
    ///
    /// Also known as: record projections, field lookup.
    Record(String),
}

/// Function closure, capturing the current values in scope.
#[derive(Clone, Debug)]
pub struct FunctionClosure {
    values: Env<Arc<Value>>,
    term: Arc<Term>,
}

impl FunctionClosure {
    pub fn new(values: Env<Arc<Value>>, term: Arc<Term>) -> FunctionClosure {
        FunctionClosure { values, term }
    }

    /// Apply an input to the function closure.
    pub fn apply(&self, globals: &Globals, input: Arc<Value>) -> Arc<Value> {
        let mut values = self.values.clone();
        values.push(input);
        eval(globals, &mut values, &self.term)
    }
}

/// Record closure, capturing the current values in scope.
#[derive(Clone, Debug)]
pub struct RecordClosure {
    values: Env<Arc<Value>>,
    entries: Arc<[Arc<Term>]>,
}

impl RecordClosure {
    pub fn new(values: Env<Arc<Value>>, entries: Arc<[Arc<Term>]>) -> RecordClosure {
        RecordClosure { values, entries }
    }

    /// Apply a callback to each of the entries in the record closure.
    pub fn for_each_entry(
        &self,
        globals: &Globals,
        mut on_entry: impl FnMut(Arc<Value>) -> Arc<Value>,
    ) {
        let mut values = self.values.clone();

        for entry_term in self.entries.iter() {
            let entry_value = eval(globals, &mut values, entry_term);
            values.push(on_entry(entry_value));
        }
    }
}

/// Initialization operation for lazy values.
///
/// We need to use a [defunctionalized] representation because Rust does not allow
/// closures of type `dyn (Clone + FnOnce() -> Arc<Value>)`.
///
/// [defunctionalized]: https://en.wikipedia.org/wiki/Defunctionalization
#[derive(Clone, Debug)]
enum LazyInit {
    EvalTerm(Env<Arc<Value>>, Arc<Term>),
    ApplyElim(Arc<LazyValue>, Elim),
}

/// A lazily initialized value.
#[derive(Clone, Debug)]
pub struct LazyValue {
    /// Initialization operation. Will be set to `None` if `cell` is forced.
    init: RefCell<Option<LazyInit>>,
    /// A once-cell to hold the lazily initialized value.
    cell: OnceCell<Arc<Value>>,
}

impl LazyValue {
    /// Eagerly construct the lazy value.
    pub fn new(value: Arc<Value>) -> LazyValue {
        LazyValue {
            init: RefCell::new(None),
            cell: OnceCell::from(value),
        }
    }

    /// Lazily evaluate a term using the given universe offset and environment.
    pub fn eval(values: Env<Arc<Value>>, term: Arc<Term>) -> LazyValue {
        LazyValue {
            init: RefCell::new(Some(LazyInit::EvalTerm(values, term))),
            cell: OnceCell::new(),
        }
    }

    /// Lazily apply an elimination.
    pub fn apply_elim(head: Arc<LazyValue>, elim: Elim) -> LazyValue {
        LazyValue {
            init: RefCell::new(Some(LazyInit::ApplyElim(head, elim))),
            cell: OnceCell::new(),
        }
    }

    /// Force the evaluation of a lazy value.
    pub fn force(&self, globals: &Globals) -> &Arc<Value> {
        self.cell.get_or_init(|| match self.init.replace(None) {
            Some(LazyInit::EvalTerm(mut values, term)) => eval(globals, &mut values, &term),
            Some(LazyInit::ApplyElim(head, elim)) => match elim {
                Elim::Record(label) => record_elim(globals, head.force(globals).clone(), &label),
                Elim::Function(input) => function_elim(globals, head.force(globals).clone(), input),
            },
            None => panic!("Lazy instance has previously been poisoned"),
        })
    }
}

/// Fully normalize a [`Term`] using [normalization by evaluation].
///
/// [`Term`]: crate::lang::core::Term
/// [normalization by evaluation]: https://en.wikipedia.org/wiki/Normalisation_by_evaluation
#[debug_ensures(values.size() == old(values.size()))]
pub fn normalize(globals: &Globals, values: &mut Env<Arc<Value>>, term: &Term) -> Term {
    let value = eval(globals, values, term);
    read_back(globals, values.size(), Unfold::Always, &value)
}

/// Evaluate a [`Term`] into a [`Value`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [`Term`]: crate::lang::core::Term
#[debug_ensures(values.size() == old(values.size()))]
pub fn eval(globals: &Globals, values: &mut Env<Arc<Value>>, term: &Term) -> Arc<Value> {
    match &term.data {
        TermData::Global(name) => match globals.get(name) {
            Some((_, Some(term))) => {
                let head = Head::Global(name.into());
                let value = LazyValue::eval(values.clone(), term.clone());
                Arc::new(Value::Unstuck(head, Vec::new(), Arc::new(value)))
            }
            Some((_, None)) | None => {
                let head = Head::Global(name.into());
                Arc::new(Value::Stuck(head, Vec::new()))
            }
        },
        TermData::Var(var_index) => match values.get(*var_index) {
            Some(value) => value.clone(),
            // FIXME: Local gluing is kind of broken right now :(
            // Some(value) => {
            //     let head = Head::Local(values.index_to_level(*var_index).unwrap()); // TODO: Handle overflow
            //     let value = LazyValue::new(value.clone());
            //     Arc::new(Value::Unstuck(head, Vec::new(), Arc::new(value)))
            // }
            None => {
                let head = Head::Var(values.index_to_level(*var_index).unwrap()); // TODO: Handle overflow
                Arc::new(Value::Stuck(head, Vec::new()))
            }
        },

        TermData::Ann(term, _) => eval(globals, values, term),

        TermData::TypeType => Arc::new(Value::TypeType),

        TermData::RecordType(labels, types) => Arc::new(Value::RecordType(
            labels.clone(),
            RecordClosure::new(values.clone(), types.clone()),
        )),
        TermData::RecordTerm(labels, terms) => Arc::new(Value::RecordTerm(
            labels.clone(),
            RecordClosure::new(values.clone(), terms.clone()),
        )),
        TermData::RecordElim(head, label) => {
            let head = eval(globals, values, head);
            record_elim(globals, head, label)
        }

        TermData::FunctionType(input_name_hint, input_type, output_type) => {
            Arc::new(Value::FunctionType(
                input_name_hint.clone(),
                eval(globals, values, input_type),
                FunctionClosure::new(values.clone(), output_type.clone()),
            ))
        }
        TermData::FunctionTerm(input_name, output_term) => Arc::new(Value::FunctionTerm(
            input_name.clone(),
            FunctionClosure::new(values.clone(), output_term.clone()),
        )),
        TermData::FunctionElim(head, input) => {
            let head = eval(globals, values, head);
            let input = LazyValue::eval(values.clone(), input.clone());
            function_elim(globals, head, Arc::new(input))
        }

        TermData::ArrayTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|entry_term| eval(globals, values, entry_term))
                .collect();

            Arc::new(Value::ArrayTerm(value_entries))
        }
        TermData::ListTerm(term_entries) => {
            let value_entries = term_entries
                .iter()
                .map(|entry_term| eval(globals, values, entry_term))
                .collect();

            Arc::new(Value::ListTerm(value_entries))
        }

        TermData::Constant(constant) => Arc::new(Value::Constant(constant.clone())),

        TermData::Error => Arc::new(Value::Error),
    }
}

/// Return the type of the record elimination.
pub fn record_elim_type(
    globals: &Globals,
    term_values: &mut Env<Arc<Value>>,
    head_term: &Term,
    head_type: &Arc<Value>,
    label: &str,
) -> Option<Arc<Value>> {
    match head_type.force(globals) {
        Value::RecordType(labels, closure) => {
            // Evaluate the head of the record elimination now that we know we
            // are dealing with a dependent record type.
            let head_value = eval(globals, term_values, head_term);
            // Type values, which will be used to substitute the values
            // associated with earlier entries into subsequent entry types.
            let mut type_values = closure.values.clone();

            // Iterate over the entry labels their and associated types.
            for (entry_label, entry_type) in Iterator::zip(labels.iter(), closure.entries.iter()) {
                if entry_label == label {
                    // We've reached the entry associated with the requested
                    // `label`, so evaluate the entry type using the current
                    // `type_values` and return it.
                    let entry_type = eval(globals, &mut type_values, entry_type);
                    return Some(entry_type);
                } else {
                    // We have not yet reached the entry associated with the
                    // requested `label`, so lookup the value associated with
                    // the `entry_label` in the `head_value` and store it in
                    // the `type_values`, ready for substitution later on.
                    let entry_value = record_elim(globals, head_value.clone(), entry_label);
                    type_values.push(entry_value);
                }
            }

            // No entry corresponding to the request `label` was found in the
            // `head_value`.
            None
        }
        Value::Error => Some(Arc::new(Value::Error)),
        _ => None,
    }
}

/// Apply a record term elimination.
fn record_elim(globals: &Globals, mut head_value: Arc<Value>, label: &str) -> Arc<Value> {
    match Arc::make_mut(&mut head_value) {
        Value::Stuck(_, spine) => {
            spine.push(Elim::Record(label.to_owned()));
            head_value
        }
        Value::Unstuck(_, spine, value) => {
            spine.push(Elim::Record(label.to_owned()));
            *value = Arc::new(LazyValue::apply_elim(
                value.clone(),
                Elim::Record(label.to_owned()),
            ));
            head_value
        }

        Value::RecordTerm(labels, closure) => {
            let mut labels = labels.iter();
            let mut values = closure.values.clone();

            for entry_value in closure.entries.iter() {
                let entry_value = eval(globals, &mut values, entry_value);
                match labels.next() {
                    Some(next_label) if next_label == label => return entry_value,
                    Some(_) | None => values.push(entry_value),
                }
            }

            Arc::new(Value::Error)
        }

        _ => Arc::new(Value::Error),
    }
}

/// Apply a function term elimination.
fn function_elim(
    globals: &Globals,
    mut head_value: Arc<Value>,
    input: Arc<LazyValue>,
) -> Arc<Value> {
    match Arc::make_mut(&mut head_value) {
        Value::Stuck(_, spine) => {
            spine.push(Elim::Function(input));
            head_value
        }
        Value::Unstuck(_, spine, value) => {
            spine.push(Elim::Function(input.clone()));
            *value = Arc::new(LazyValue::apply_elim(value.clone(), Elim::Function(input)));
            head_value
        }

        Value::FunctionTerm(_, output_closure) => {
            output_closure.apply(globals, input.force(globals).clone())
        }

        _ => Arc::new(Value::Error),
    }
}

/// Describes how definitions should be unfolded to when reading back values.
#[derive(Copy, Clone, Debug)]
pub enum Unfold {
    /// Never unfold definitions.
    ///
    /// This avoids generating bloated terms, which can be detrimental for
    /// performance and difficult for humans to read. Examples of where this
    /// might be useful include:
    ///
    /// - elaborating partially annotated surface terms into core terms that
    ///   require explicit type annotations
    /// - displaying terms in diagnostic messages to the user
    Never,
    /// Always unfold globals and variables.
    ///
    /// This is useful for fully normalizing terms.
    Always,
}

/// Read-back a stuck value into the term syntax.
fn read_back_stuck(
    globals: &Globals,
    env_size: EnvSize,
    unfold: Unfold,
    head: &Head,
    spine: &[Elim],
) -> Term {
    let head = match head {
        Head::Global(name) => Term::generated(TermData::Global(name.clone())),
        Head::Var(var_level) => {
            let var_index = env_size.level_to_index(*var_level).unwrap();
            Term::generated(TermData::Var(var_index)) // TODO: Handle overflow
        }
    };

    spine.iter().fold(head, |head, elim| match elim {
        Elim::Function(input) => {
            let input = read_back(globals, env_size, unfold, input.force(globals));
            Term::generated(TermData::FunctionElim(Arc::new(head), Arc::new(input)))
        }
        Elim::Record(label) => Term::generated(TermData::RecordElim(Arc::new(head), label.clone())),
    })
}

/// Read-back a value into the term syntax.
pub fn read_back(globals: &Globals, env_size: EnvSize, unfold: Unfold, value: &Value) -> Term {
    match value {
        Value::Stuck(head, spine) => read_back_stuck(globals, env_size, unfold, head, spine),
        Value::Unstuck(head, spine, value) => match unfold {
            Unfold::Never => read_back_stuck(globals, env_size, unfold, head, spine),
            Unfold::Always => read_back(globals, env_size, unfold, value.force(globals)),
        },

        Value::TypeType => Term::generated(TermData::TypeType),

        Value::FunctionType(input_name_hint, input_type, output_closure) => {
            let var = Arc::new(Value::var(env_size.next_level(), []));
            let input_type = read_back(globals, env_size, unfold, input_type);
            let output_type = output_closure.apply(globals, var);
            let output_type = read_back(globals, env_size.next_size(), unfold, &output_type);

            Term::generated(TermData::FunctionType(
                input_name_hint.clone(),
                Arc::new(input_type),
                Arc::new(output_type),
            ))
        }
        Value::FunctionTerm(input_name_hint, output_closure) => {
            let var = Arc::new(Value::var(env_size.next_level(), []));
            let output_term = output_closure.apply(globals, var);
            let output_term = read_back(globals, env_size.next_size(), unfold, &output_term);

            Term::generated(TermData::FunctionTerm(
                input_name_hint.clone(),
                Arc::new(output_term),
            ))
        }

        Value::RecordType(labels, closure) => {
            let mut env_size = env_size;
            let mut types = Vec::with_capacity(closure.entries.len());

            closure.for_each_entry(globals, |entry_type| {
                let entry_type = read_back(globals, env_size, unfold, &entry_type);
                types.push(Arc::new(entry_type));

                let var_level = env_size.next_level();
                env_size = env_size.next_size();

                Arc::new(Value::var(var_level, []))
            });

            Term::generated(TermData::RecordType(labels.clone(), types.into()))
        }
        Value::RecordTerm(labels, closure) => {
            let mut env_size = env_size;
            let mut terms = Vec::with_capacity(closure.entries.len());

            closure.for_each_entry(globals, |entry_term| {
                let entry_term = read_back(globals, env_size, unfold, &entry_term);
                terms.push(Arc::new(entry_term));

                let var_level = env_size.next_level();
                env_size = env_size.next_size();

                Arc::new(Value::var(var_level, []))
            });

            Term::generated(TermData::RecordTerm(labels.clone(), terms.into()))
        }

        Value::ArrayTerm(value_entries) => {
            let term_entries = value_entries
                .iter()
                .map(|value_entry| Arc::new(read_back(globals, env_size, unfold, value_entry)))
                .collect();

            Term::generated(TermData::ArrayTerm(term_entries))
        }
        Value::ListTerm(value_entries) => {
            let term_entries = value_entries
                .iter()
                .map(|value_entry| Arc::new(read_back(globals, env_size, unfold, value_entry)))
                .collect();

            Term::generated(TermData::ListTerm(term_entries))
        }

        Value::Constant(constant) => Term::generated(TermData::from(constant.clone())),

        Value::Error => Term::generated(TermData::Error),
    }
}

/// Check that one stuck value is equal to another stuck value.
fn is_equal_stuck(
    globals: &Globals,
    env_size: EnvSize,
    (head0, spine0): (&Head, &[Elim]),
    (head1, spine1): (&Head, &[Elim]),
) -> bool {
    if head0 != head1 || spine0.len() != spine1.len() {
        return false;
    }

    for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
        match (elim0, elim1) {
            (Elim::Function(input0), Elim::Function(input1)) => {
                let input0 = input0.force(globals);
                let input1 = input1.force(globals);

                if !is_equal(globals, env_size, input0, input1) {
                    return false;
                }
            }
            (Elim::Record(label0), Elim::Record(label1)) if label0 == label1 => {}
            (_, _) => return false,
        }
    }

    true
}

/// Check that one function closure is equal to another function closure.
fn is_equal_function_closure(
    globals: &Globals,
    env_size: EnvSize,
    closure0: &FunctionClosure,
    closure1: &FunctionClosure,
) -> bool {
    let var = Arc::new(Value::var(env_size.next_level(), []));
    let term0 = closure0.apply(globals, var.clone());
    let term1 = closure1.apply(globals, var);
    is_equal(globals, env_size.next_size(), &term0, &term1)
}

/// Check that one record closure is equal to another record closure.
fn is_equal_record_closure(
    globals: &Globals,
    mut env_size: EnvSize,
    closure0: &RecordClosure,
    closure1: &RecordClosure,
) -> bool {
    if closure0.entries.len() != closure1.entries.len() {
        return false;
    }

    let mut values0 = closure0.values.clone();
    let mut values1 = closure1.values.clone();

    for (entry0, entry1) in Iterator::zip(closure0.entries.iter(), closure1.entries.iter()) {
        let entry0 = eval(globals, &mut values0, entry0);
        let entry1 = eval(globals, &mut values1, entry1);

        if !is_equal(globals, env_size, &entry0, &entry1) {
            return false;
        }

        let var = Arc::new(Value::var(env_size.next_level(), []));
        values0.push(var.clone());
        values1.push(var);
        env_size = env_size.next_size();
    }

    true
}

/// Check that one value is [computationally equal] to another value.
///
/// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
pub fn is_equal(globals: &Globals, env_size: EnvSize, value0: &Value, value1: &Value) -> bool {
    match (value0, value1) {
        (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
            is_equal_stuck(globals, env_size, (head0, spine0), (head1, spine1))
        }
        (Value::Unstuck(head0, spine0, value0), Value::Unstuck(head1, spine1, value1)) => {
            if is_equal_stuck(globals, env_size, (head0, spine0), (head1, spine1)) {
                // No need to force computation if the stuck values are the same!
                return true;
            }

            let value0 = value0.force(globals);
            let value1 = value1.force(globals);
            is_equal(globals, env_size, value0, value1)
        }
        (Value::Unstuck(_, _, value0), value1) => {
            is_equal(globals, env_size, value0.force(globals), value1)
        }
        (value0, Value::Unstuck(_, _, value1)) => {
            is_equal(globals, env_size, value0, value1.force(globals))
        }

        (Value::TypeType, Value::TypeType) => true,

        (
            Value::FunctionType(_, input_type0, output_closure0),
            Value::FunctionType(_, input_type1, output_closure1),
        ) => {
            is_equal(globals, env_size, input_type1, input_type0)
                && is_equal_function_closure(globals, env_size, output_closure0, output_closure1)
        }
        (Value::FunctionTerm(_, output_closure0), Value::FunctionTerm(_, output_closure1)) => {
            is_equal_function_closure(globals, env_size, output_closure0, output_closure1)
        }

        (Value::RecordType(labels0, closure0), Value::RecordType(labels1, closure1))
        | (Value::RecordTerm(labels0, closure0), Value::RecordTerm(labels1, closure1)) => {
            labels0 == labels1 && is_equal_record_closure(globals, env_size, closure0, closure1)
        }

        (Value::ArrayTerm(value_entries0), Value::ArrayTerm(value_entries1))
        | (Value::ListTerm(value_entries0), Value::ListTerm(value_entries1)) => {
            if value_entries0.len() != value_entries1.len() {
                return false;
            }

            Iterator::zip(value_entries0.iter(), value_entries1.iter()).all(
                |(value_entry0, value_entry1)| {
                    is_equal(globals, env_size, value_entry0, value_entry1)
                },
            )
        }

        (Value::Constant(constant0), Value::Constant(constant1)) => constant0 == constant1,

        // Errors are always treated as equal, regardless of what they are compared with.
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
