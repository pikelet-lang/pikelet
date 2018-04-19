//! The core syntax of the language

use codespan::{ByteIndex, ByteSpan};
use nameless::{self, Bind, Embed, Ignore, Name, Var};
use rpds::List;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use syntax::pretty::ToDoc;

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, BoundTerm)]
pub struct Level(pub u32);

impl Level {
    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw primitive constants
///
/// These are either the literal values or the types that describe them.
///
/// We could church encode all the things, but that would be prohibitively
/// expensive computationally!
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm)]
pub enum RawConstant {
    String(String),
    Char(char),
    Int(u64),
    Float(f64),
}

impl fmt::Display for RawConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// Primitive constants
///
/// These are either the literal values or the types that describe them.
///
/// We could church encode all the things, but that would be prohibitively
/// expensive computationally!
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm)]
pub enum Constant {
    Bool(bool),
    String(String),
    Char(char),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    BoolType,
    StringType,
    CharType,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    I8Type,
    I16Type,
    I32Type,
    I64Type,
    F32Type,
    F64Type,
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// A module definition
pub struct RawModule {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<RawDefinition>,
}

impl fmt::Display for RawModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// Top level definitions
pub struct RawDefinition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: Rc<RawTerm>,
    /// An optional type annotation to aid in type inference
    pub ann: Rc<RawTerm>,
}

impl fmt::Display for RawDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// A record label
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct Label(pub String);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(Ignore<ByteSpan>, Rc<RawTerm>, Rc<RawTerm>),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Constants
    Constant(Ignore<ByteSpan>, RawConstant),
    /// A hole
    Hole(Ignore<ByteSpan>),
    /// A variable
    Var(Ignore<ByteSpan>, Var),
    /// Dependent function types
    Pi(
        Ignore<ByteSpan>,
        Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// Lambda abstractions
    Lam(
        Ignore<ByteSpan>,
        Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>,
    ),
    /// Term application
    App(Rc<RawTerm>, Rc<RawTerm>),
    /// If expression
    If(Ignore<ByteIndex>, Rc<RawTerm>, Rc<RawTerm>, Rc<RawTerm>),
    /// Dependent record types
    RecordType(Ignore<ByteSpan>, Label, Rc<RawTerm>, Rc<RawTerm>),
    /// Dependent record
    Record(Ignore<ByteSpan>, Label, Rc<RawTerm>, Rc<RawTerm>),
    /// The unit type
    EmptyRecordType(Ignore<ByteSpan>),
    /// The element of the unit type
    EmptyRecord(Ignore<ByteSpan>),
    /// Field projection
    Proj(Ignore<ByteSpan>, Rc<RawTerm>, Ignore<ByteSpan>, Label),
}

impl RawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self {
            RawTerm::Ann(span, _, _)
            | RawTerm::Universe(span, _)
            | RawTerm::Hole(span)
            | RawTerm::Constant(span, _)
            | RawTerm::Var(span, _)
            | RawTerm::Pi(span, _)
            | RawTerm::Lam(span, _)
            | RawTerm::RecordType(span, _, _, _)
            | RawTerm::Record(span, _, _, _)
            | RawTerm::EmptyRecordType(span)
            | RawTerm::EmptyRecord(span)
            | RawTerm::Proj(span, _, _, _) => span.0,
            RawTerm::App(ref fn_term, ref arg) => fn_term.span().to(arg.span()),
            RawTerm::If(start, _, _, ref if_false) => ByteSpan::new(start.0, if_false.span().end()),
        }
    }
}

impl fmt::Display for RawTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

impl RawTerm {
    // TODO: Move to nameless crate
    fn visit_vars<F: FnMut(&Var)>(&self, on_var: &mut F) {
        match *self {
            RawTerm::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            RawTerm::Universe(_, _) | RawTerm::Hole(_) | RawTerm::Constant(_, _) => {},
            RawTerm::Var(_, ref var) => on_var(var),
            RawTerm::Pi(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
            },
            RawTerm::Lam(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
            },
            RawTerm::App(ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
            RawTerm::If(_, ref cond, ref if_true, ref if_false) => {
                cond.visit_vars(on_var);
                if_true.visit_vars(on_var);
                if_false.visit_vars(on_var);
            },
            RawTerm::RecordType(_, _, ref ann, ref rest) => {
                ann.visit_vars(on_var);
                rest.visit_vars(on_var);
                return;
            },
            RawTerm::Record(_, _, ref expr, ref rest) => {
                expr.visit_vars(on_var);
                rest.visit_vars(on_var);
                return;
            },
            RawTerm::EmptyRecordType(_) => return,
            RawTerm::EmptyRecord(_) => return,
            RawTerm::Proj(_, ref expr, _, _) => {
                expr.visit_vars(on_var);
                return;
            },
        };
    }

    // TODO: move to nameless crate
    pub fn free_vars(&self) -> HashSet<Name> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_, _) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

/// A typechecked and elaborated module
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

/// A typechecked and elaborated definition
pub struct Definition {
    /// The name of the definition
    pub name: String,
    /// The elaborated value
    pub term: Rc<Term>,
    /// The type of the definition
    pub ann: Rc<Type>,
}

/// The core term syntax
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(Ignore<ByteSpan>, Rc<Term>, Rc<Term>),
    /// Universes
    Universe(Ignore<ByteSpan>, Level),
    /// Constants
    Constant(Ignore<ByteSpan>, Constant),
    /// A variable
    Var(Ignore<ByteSpan>, Var),
    /// Dependent function types
    Pi(Ignore<ByteSpan>, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Lambda abstractions
    Lam(Ignore<ByteSpan>, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Term application
    App(Rc<Term>, Rc<Term>),
    /// If expression
    If(Ignore<ByteIndex>, Rc<Term>, Rc<Term>, Rc<Term>),
    /// Dependent record types
    RecordType(Ignore<ByteSpan>, Label, Rc<Term>, Rc<Term>),
    /// Dependent record
    Record(Ignore<ByteSpan>, Label, Rc<Term>, Rc<Term>),
    /// The unit type
    EmptyRecordType(Ignore<ByteSpan>),
    /// The element of the unit type
    EmptyRecord(Ignore<ByteSpan>),
    /// Field projection
    Proj(Ignore<ByteSpan>, Rc<Term>, Ignore<ByteSpan>, Label),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Ann(span, _, _)
            | Term::Universe(span, _)
            | Term::Constant(span, _)
            | Term::Var(span, _)
            | Term::Lam(span, _)
            | Term::Pi(span, _)
            | Term::RecordType(span, _, _, _)
            | Term::Record(span, _, _, _)
            | Term::EmptyRecordType(span)
            | Term::EmptyRecord(span)
            | Term::Proj(span, _, _, _) => span.0,
            Term::App(ref fn_term, ref arg) => fn_term.span().to(arg.span()),
            Term::If(start, _, _, ref if_false) => ByteSpan::new(start.0, if_false.span().end()),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

impl Term {
    // TODO: Move to nameless crate
    fn visit_vars<F: FnMut(&Var)>(&self, on_var: &mut F) {
        match *self {
            Term::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            Term::Universe(_, _) | Term::Constant(_, _) => {},
            Term::Var(_, ref var) => on_var(var),
            Term::Pi(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
            },
            Term::Lam(_, ref scope) => {
                (scope.unsafe_pattern.1).0.visit_vars(on_var);
                scope.unsafe_body.visit_vars(on_var);
            },
            Term::App(ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
            Term::If(_, ref cond, ref if_true, ref if_false) => {
                cond.visit_vars(on_var);
                if_true.visit_vars(on_var);
                if_false.visit_vars(on_var);
            },
            Term::RecordType(_, _, ref ann, ref rest) => {
                ann.visit_vars(on_var);
                rest.visit_vars(on_var);
                return;
            },
            Term::Record(_, _, ref expr, ref rest) => {
                expr.visit_vars(on_var);
                rest.visit_vars(on_var);
                return;
            },
            Term::EmptyRecordType(_) => return,
            Term::EmptyRecord(_) => return,
            Term::Proj(_, ref expr, _, _) => {
                expr.visit_vars(on_var);
                return;
            },
        };
    }

    // TODO: move to nameless crate
    pub fn free_vars(&self) -> HashSet<Name> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_, _) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

/// Values
///
/// These are either in _weak head normal form_ (they cannot be reduced further)
/// or are _neutral terms_ (there is a possibility of reducing further depending
/// on the bindings given in the context)
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Value {
    /// Universes
    Universe(Level),
    /// Constants
    Constant(Constant),
    /// A pi type
    Pi(Bind<(Name, Embed<Rc<Value>>), Rc<Value>>),
    /// A lambda abstraction
    Lam(Bind<(Name, Embed<Rc<Value>>), Rc<Value>>),
    /// Dependent record types
    RecordType(Label, Rc<Value>, Rc<Value>),
    /// Dependent record
    Record(Label, Rc<Value>, Rc<Value>),
    /// The unit type
    EmptyRecordType,
    /// The element of the unit type
    EmptyRecord,
    /// Neutral terms
    Neutral(Rc<Neutral>),
}

impl Value {
    pub fn lookup_record_ty(&self, label: &Label) -> Option<Rc<Value>> {
        fn lookup_next(value: &Value, label: &Label) -> Result<Rc<Value>, Option<Rc<Value>>> {
            if let Value::RecordType(ref curr_label, ref value, ref body) = *value {
                if curr_label == label {
                    Ok(value.clone())
                } else {
                    Err(Some(body.clone()))
                }
            } else {
                Err(None)
            }
        }

        let mut current = lookup_next(self, label);
        loop {
            current = match current {
                Ok(term) => return Some(term),
                Err(Some(term)) => lookup_next(&*term, label),
                Err(None) => return None,
            };
        }
    }

    pub fn lookup_record(&self, label: &Label) -> Option<Rc<Value>> {
        fn lookup_next(value: &Value, label: &Label) -> Result<Rc<Value>, Option<Rc<Value>>> {
            if let Value::Record(ref curr_label, ref value, ref body) = *value {
                if curr_label == label {
                    Ok(value.clone())
                } else {
                    Err(Some(body.clone()))
                }
            } else {
                Err(None)
            }
        }

        let mut current = lookup_next(self, label);
        loop {
            current = match current {
                Ok(term) => return Some(term),
                Err(Some(term)) => lookup_next(&*term, label),
                Err(None) => return None,
            };
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// Neutral terms
///
/// These might be able to be reduced further depending on the bindings in the
/// context
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Neutral {
    /// Variables
    Var(Var),
    /// RawTerm application
    App(Rc<Neutral>, Rc<Term>),
    /// If expression
    If(Rc<Neutral>, Rc<Term>, Rc<Term>),
    /// Field projection
    Proj(Rc<Neutral>, Label),
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// Types are at the term level, so this is just an alias
pub type Type = Value;

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::Neutral(Rc::new(src))
    }
}

impl<'a> From<&'a Value> for Term {
    fn from(src: &'a Value) -> Term {
        match *src {
            Value::Universe(level) => Term::Universe(Ignore::default(), level),
            Value::Constant(ref c) => Term::Constant(Ignore::default(), c.clone()),
            Value::Pi(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Pi(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::Lam(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Lam(
                    Ignore::default(),
                    nameless::bind(param, Rc::new(Term::from(&*body))),
                )
            },
            Value::RecordType(ref label, ref ann, ref rest) => Term::RecordType(
                Ignore::default(),
                label.clone(),
                Rc::new(Term::from(&**ann)),
                Rc::new(Term::from(&**rest)),
            ),
            Value::Record(ref label, ref expr, ref rest) => Term::Record(
                Ignore::default(),
                label.clone(),
                Rc::new(Term::from(&**expr)),
                Rc::new(Term::from(&**rest)),
            ),
            Value::EmptyRecordType => Term::EmptyRecordType(Ignore::default()).into(),
            Value::EmptyRecord => Term::EmptyRecord(Ignore::default()).into(),
            Value::Neutral(ref n) => Term::from(&**n),
        }
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        match *src {
            Neutral::Var(ref var) => Term::Var(Ignore::default(), var.clone()),
            Neutral::App(ref fn_expr, ref arg_expr) => {
                Term::App(Rc::new(Term::from(&**fn_expr)), arg_expr.clone())
            },
            Neutral::If(ref cond, ref if_true, ref if_false) => Term::If(
                Ignore::default(),
                Rc::new(Term::from(&**cond)),
                if_true.clone(),
                if_false.clone(),
            ),
            Neutral::Proj(ref expr, ref name) => Term::Proj(
                Ignore::default(),
                Rc::new(Term::from(&**expr)),
                Ignore::default(),
                name.clone(),
            ).into(),
        }
    }
}

/// An entry in the context
#[derive(Debug, Clone, PartialEq)]
pub enum ContextEntry {
    /// A type claim
    Claim(Name, Rc<Type>),
    /// A value definition
    Definition(Name, Rc<Term>),
}

/// A list of binders that have been accumulated during typechecking
#[derive(Clone, PartialEq)]
pub struct Context {
    pub entries: List<ContextEntry>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            entries: List::new(),
        }
    }

    pub fn claim(&self, name: Name, ty: Rc<Type>) -> Context {
        Context {
            entries: self.entries.push_front(ContextEntry::Claim(name, ty)),
        }
    }

    pub fn define(&self, name: Name, term: Rc<Term>) -> Context {
        Context {
            entries: self.entries
                .push_front(ContextEntry::Definition(name, term)),
        }
    }

    pub fn lookup_claim(&self, name: &Name) -> Option<&Rc<Type>> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                ContextEntry::Claim(ref n, ref ty) if n == name => Some(ty),
                ContextEntry::Claim(_, _) | ContextEntry::Definition(_, _) => None,
            })
            .next()
    }

    pub fn lookup_definition(&self, name: &Name) -> Option<&Rc<Term>> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                ContextEntry::Definition(ref n, ref term) if n == name => Some(term),
                ContextEntry::Definition(_, _) | ContextEntry::Claim(_, _) => None,
            })
            .next()
    }
}

impl Default for Context {
    fn default() -> Context {
        let universe0 = Rc::new(Value::Universe(Level(0)));
        let constant = |c| Rc::new(Term::Constant(Ignore::default(), c));
        let constant_val = |c| Rc::new(Value::Constant(c));

        Context::new()
            .claim(Name::user("true"), constant_val(Constant::BoolType))
            .define(Name::user("true"), constant(Constant::Bool(true)))
            .claim(Name::user("false"), constant_val(Constant::BoolType))
            .define(Name::user("false"), constant(Constant::Bool(false)))
            .claim(Name::user("Bool"), universe0.clone())
            .define(Name::user("Bool"), constant(Constant::BoolType))
            .claim(Name::user("String"), universe0.clone())
            .define(Name::user("String"), constant(Constant::StringType))
            .claim(Name::user("Char"), universe0.clone())
            .define(Name::user("Char"), constant(Constant::CharType))
            .claim(Name::user("U8"), universe0.clone())
            .define(Name::user("U8"), constant(Constant::U8Type))
            .claim(Name::user("U16"), universe0.clone())
            .define(Name::user("U16"), constant(Constant::U16Type))
            .claim(Name::user("U32"), universe0.clone())
            .define(Name::user("U32"), constant(Constant::U32Type))
            .claim(Name::user("U64"), universe0.clone())
            .define(Name::user("U64"), constant(Constant::U64Type))
            .claim(Name::user("I8"), universe0.clone())
            .define(Name::user("I8"), constant(Constant::I8Type))
            .claim(Name::user("I16"), universe0.clone())
            .define(Name::user("I16"), constant(Constant::I16Type))
            .claim(Name::user("I32"), universe0.clone())
            .define(Name::user("I32"), constant(Constant::I32Type))
            .claim(Name::user("I64"), universe0.clone())
            .define(Name::user("I64"), constant(Constant::I64Type))
            .claim(Name::user("F32"), universe0.clone())
            .define(Name::user("F32"), constant(Constant::F32Type))
            .claim(Name::user("F64"), universe0.clone())
            .define(Name::user("F64"), constant(Constant::F64Type))
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtContextEntries<'a>(&'a List<ContextEntry>);

        impl<'a> fmt::Debug for FmtContextEntries<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("entries", &FmtContextEntries(&self.entries))
            .finish()
    }
}
