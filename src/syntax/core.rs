//! The core syntax of the language

use codespan::ByteSpan;
use nameless::{self, Bind, BoundTerm, Embed, Name, Var};
use rpds::List;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::usize;

use syntax::pretty::{self, ToDoc};

/// Source metadata that should be ignored when checking for alpha equality
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourceMeta {
    pub span: ByteSpan,
}

impl BoundTerm for SourceMeta {
    fn term_eq(&self, _: &SourceMeta) -> bool {
        true
    }
}

impl Default for SourceMeta {
    fn default() -> SourceMeta {
        SourceMeta {
            span: ByteSpan::default(),
        }
    }
}

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
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Raw terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(SourceMeta, Rc<RawTerm>, Rc<RawTerm>),
    /// Universes
    Universe(SourceMeta, Level),
    /// Constants
    Constant(SourceMeta, RawConstant),
    /// A hole
    Hole(SourceMeta),
    /// A variable
    Var(SourceMeta, Var),
    /// Dependent function types
    Pi(SourceMeta, Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>),
    /// Lambda abstractions
    Lam(SourceMeta, Bind<(Name, Embed<Rc<RawTerm>>), Rc<RawTerm>>),
    /// RawTerm application
    App(SourceMeta, Rc<RawTerm>, Rc<RawTerm>),
}

impl RawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self {
            RawTerm::Ann(meta, _, _)
            | RawTerm::Universe(meta, _)
            | RawTerm::Hole(meta)
            | RawTerm::Constant(meta, _)
            | RawTerm::Var(meta, _)
            | RawTerm::Pi(meta, _)
            | RawTerm::Lam(meta, _)
            | RawTerm::App(meta, _, _) => meta.span,
        }
    }
}

impl fmt::Display for RawTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

impl RawTerm {
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
            RawTerm::App(_, ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
        };
    }

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
    Ann(SourceMeta, Rc<Term>, Rc<Term>),
    /// Universes
    Universe(SourceMeta, Level),
    /// Constants
    Constant(SourceMeta, Constant),
    /// A variable
    Var(SourceMeta, Var),
    /// Dependent function types
    Pi(SourceMeta, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Lambda abstractions
    Lam(SourceMeta, Bind<(Name, Embed<Rc<Term>>), Rc<Term>>),
    /// Term application
    App(SourceMeta, Rc<Term>, Rc<Term>),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Ann(meta, _, _)
            | Term::Universe(meta, _)
            | Term::Constant(meta, _)
            | Term::Var(meta, _)
            | Term::Lam(meta, _)
            | Term::Pi(meta, _)
            | Term::App(meta, _, _) => meta.span,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Normal forms
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
    /// Neutral terms
    Neutral(Rc<Neutral>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
        let meta = SourceMeta::default();

        match *src {
            Value::Universe(level) => Term::Universe(meta, level),
            Value::Constant(ref c) => Term::Constant(meta, c.clone()),
            Value::Pi(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Pi(meta, nameless::bind(param, Rc::new(Term::from(&*body))))
            },
            Value::Lam(ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                let param = (name, Embed(Rc::new(Term::from(&*param_ann))));

                Term::Lam(meta, nameless::bind(param, Rc::new(Term::from(&*body))))
            },
            Value::Neutral(ref n) => Term::from(&**n),
        }
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        let meta = SourceMeta::default();

        match *src {
            Neutral::Var(ref var) => Term::Var(meta, var.clone()),
            Neutral::App(ref fn_expr, ref arg_expr) => {
                Term::App(meta, Rc::new(Term::from(&**fn_expr)), arg_expr.clone())
            },
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
        let constant = |c: Constant| Rc::new(Term::Constant(SourceMeta::default(), c));

        Context::new()
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
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
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
