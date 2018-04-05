//! The concrete syntax of the language

use codespan::{ByteIndex, ByteOffset, ByteSpan};
use std::fmt;
use std::usize;

use syntax::pretty::{self, ToDoc};

/// Commands entered in the REPL
#[derive(Debug, Clone)]
pub enum ReplCommand {
    /// Evaluate a term
    ///
    /// ```text
    /// <term>
    /// ```
    Eval(Box<Term>),
    /// Print some help about using the REPL
    ///
    /// ```text
    /// :?
    /// :h
    /// :help
    /// ```
    Help,
    /// Add a declaration to the REPL environment
    ///
    /// ```text
    ///:let <name> = <term>
    /// ```
    Let(String, Box<Term>),
    ///  No command
    NoOp,
    /// Quit the REPL
    ///
    /// ```text
    /// :q
    /// :quit
    /// ```
    Quit,
    /// Print the type of the term
    ///
    /// ```text
    /// :t <term>
    /// :type <term>
    /// ```
    TypeOf(Box<Term>),
    /// Repl commands that could not be parsed correctly
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

/// Modules
#[derive(Debug, Clone, PartialEq)]
pub enum Module {
    /// A module definition:
    ///
    /// ```text
    /// module my-module;
    ///
    /// <declarations>
    /// ```
    Valid {
        name: (ByteIndex, String),
        declarations: Vec<Declaration>,
    },
    /// Modules commands that could not be parsed correctly
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Top level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Imports a module into the current scope
    ///
    /// ```text
    /// import foo;
    /// import foo as my-foo;
    /// import foo as my-foo (..);
    /// ```
    Import {
        span: ByteSpan,
        name: (ByteIndex, String),
        rename: Option<(ByteIndex, String)>,
        exposing: Option<Exposing>,
    },
    /// Claims that a term abides by the given type
    ///
    /// ```text
    /// foo : some-type
    /// ```
    Claim {
        name: (ByteIndex, String),
        ann: Term,
    },
    /// Declares the body of a term
    ///
    /// ```text
    /// foo = some-body
    /// foo x (y : some-type) = some-body
    ///     where {
    ///         some-subdef : some-type
    ///         some-subdef = some-body
    ///     }
    /// ```
    Definition {
        span: ByteSpan,
        name: String,
        params: LamParams,
        body: Term,
        wheres: Vec<Declaration>,
    },
    /// Declarations that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Declaration {
    /// Return the span of source code that this declaration originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Declaration::Import { span, .. } | Declaration::Definition { span, .. } => span,
            Declaration::Claim { ref name, ref ann } => ByteSpan::new(name.0, ann.span().end()),
            Declaration::Error(span) => span,
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// A list of the definitions imported from a module
#[derive(Debug, Clone, PartialEq)]
pub enum Exposing {
    /// Import all the definitions in the module into the current scope
    ///
    /// ```text
    /// (..)
    /// ```
    All(ByteSpan),
    /// Import an exact set of definitions into the current scope
    ///
    /// ```text
    /// (foo, bar as baz)
    /// ```
    Exact(
        ByteSpan,
        Vec<((ByteIndex, String), Option<(ByteIndex, String)>)>,
    ),
    /// Exposing declarations that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl fmt::Display for Exposing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// String literals
    String(String),
    /// Character literals
    Char(char),
    /// Integer literals
    Int(u64),
    /// Floating point literals
    Float(f64),
}

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (e)
    /// ```
    Parens(ByteSpan, Box<Term>),
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(Box<Term>, Box<Term>),
    /// Type of types
    ///
    /// ```text
    /// Type
    /// ```
    Universe(ByteSpan, Option<u32>),
    /// Literal values
    Literal(ByteSpan, Literal),
    /// Holes
    ///
    /// ```text
    /// _
    /// ```
    Hole(ByteSpan),
    /// Variables
    ///
    /// ```text
    /// x
    /// ```
    Var(ByteIndex, String),
    /// Lambda abstractions
    ///
    /// ```text
    /// \x => t
    /// \x y => t
    /// \x : t1 => t2
    /// \(x : t1) y (z : t2) => t3
    /// \(x y : t1) => t3
    /// ```
    Lam(ByteIndex, LamParams, Box<Term>),
    /// Dependent function types
    ///
    /// ```text
    /// (x : t1) -> t2
    /// (x y : t1) -> t2
    /// ```
    Pi(ByteIndex, PiParams, Box<Term>),
    /// Non-Dependent function types
    ///
    /// ```text
    /// t1 -> t2
    /// ```
    Arrow(Box<Term>, Box<Term>),
    /// Term application
    ///
    /// ```text
    /// e1 e2
    /// ```
    App(Box<Term>, Vec<Term>),
    /// Let binding
    ///
    /// ```text
    /// let x : I32
    ///     x = 1
    /// in
    ///     x
    /// ```
    Let(ByteIndex, Vec<Declaration>, Box<Term>),
    /// Record types
    ///
    /// ```text
    /// Record { x : t1, .. }
    /// ```
    RecordType(ByteSpan, Vec<(ByteIndex, String, Box<Term>)>),
    /// Record values
    ///
    /// ```text
    /// record { x = t1, .. }
    /// ```
    Record(ByteSpan, Vec<(ByteIndex, String, Box<Term>)>),
    /// Record field projection
    ///
    /// ```text
    /// e.l
    /// ```
    Proj(Box<Term>, ByteIndex, String),
    /// Terms that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Term {
    /// Return the span of source code that this term originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Parens(span, _)
            | Term::Universe(span, _)
            | Term::Literal(span, _)
            | Term::Hole(span)
            | Term::RecordType(span, _)
            | Term::Record(span, _)
            | Term::Error(span) => span,
            Term::Var(start, ref name) => ByteSpan::from_offset(start, ByteOffset::from_str(name)),
            Term::Pi(start, _, ref body)
            | Term::Lam(start, _, ref body)
            | Term::Let(start, _, ref body) => ByteSpan::new(start, body.span().end()),
            Term::Ann(ref term, ref ty) => term.span().to(ty.span()),
            Term::Arrow(ref ann, ref body) => ann.span().to(body.span()),
            Term::App(ref fn_term, ref arg) => fn_term.span().to(arg[arg.len() - 1].span()),
            Term::Proj(ref term, label_start, ref label) => term.span()
                .with_end(label_start + ByteOffset::from_str(label)),
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

/// The parameters to a lambda abstraction
pub type LamParams = Vec<(Vec<(ByteIndex, String)>, Option<Box<Term>>)>;

/// The parameters to a dependent function type
pub type PiParams = Vec<(Vec<(ByteIndex, String)>, Term)>;
