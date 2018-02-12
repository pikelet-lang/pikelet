//! The concrete syntax of the language

/// Commands entered in the REPL
#[derive(Debug, Clone)]
pub enum ReplCommand<S> {
    /// Evaluate a term
    ///
    /// ```text
    /// <term>
    /// ```
    Eval(Box<Term<S>>),
    /// Print some help on the REPL
    ///
    /// ```text
    /// :?
    /// :h
    /// :help
    /// ```
    Help,
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
    TypeOf(Box<Term<S>>),
}

/// A module definition:
///
/// ```text
/// module my-module;
///
/// <declarations>
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Module<S> {
    /// The name of the module
    pub name: (S, String),
    /// The declarations contained in the module
    pub declarations: Vec<Declaration<S>>,
}

/// Top level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration<S> {
    /// Imports a module into the current scope
    ///
    /// ```text
    /// import foo;
    /// import foo as my-foo;
    /// import foo as my-foo (..);
    /// ```
    Import {
        span: S,
        name: (S, String),
        rename: Option<(S, String)>,
        exposing: Option<Exposing<S>>,
    },
    /// Claims that a term abides by the given type
    ///
    /// ```text
    /// foo : some-type
    /// ```
    Claim {
        span: S,
        name: (S, String),
        ann: Term<S>,
    },
    /// Declares the body of a term
    ///
    /// ```text
    /// foo = some-body
    /// foo x (y : some-type) = some-body
    /// ```
    Definition {
        span: S,
        name: (S, String),
        params: LamParams<S>,
        body: Term<S>,
    },
}

/// A list of the definitions imported from a module
#[derive(Debug, Clone, PartialEq)]
pub enum Exposing<S> {
    /// Import all the definitions in the module into the current scope
    ///
    /// ```text
    /// (..)
    /// ```
    All(S),
    /// Import an exact set of definitions into the current scope
    ///
    /// ```text
    /// (foo, bar as baz)
    /// ```
    Exact(S, Vec<((S, String), Option<(S, String)>)>),
}

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term<S> {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (e)
    /// ```
    Parens(S, Box<Term<S>>),
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(S, Box<Term<S>>, Box<Term<S>>),
    /// Type of types
    ///
    /// ```text
    /// Type
    /// ```
    Universe(S, Option<u32>),
    /// Variables
    ///
    /// ```text
    /// x
    /// ```
    Var(S, String),
    /// Lambda abstractions
    ///
    /// ```text
    /// \x => t
    /// \x y => t
    /// \x : t1 => t2
    /// \(x : t1) y (z : t2) => t3
    /// \(x y : t1) => t3
    /// ```
    Lam(S, LamParams<S>, Box<Term<S>>),
    /// Dependent function types
    ///
    /// ```text
    /// (x : t1) -> t2
    /// (x y : t1) -> t2
    /// ```
    Pi(S, PiParams<S>, Box<Term<S>>),
    /// Non-Dependent function types
    ///
    /// ```text
    /// t1 -> t2
    /// ```
    Arrow(S, Box<Term<S>>, Box<Term<S>>),
    /// Term application
    ///
    /// ```text
    /// e1 e2
    /// ```
    App(S, Box<Term<S>>, Box<Term<S>>),
}

/// The parameters to a lambda abstraction
pub type LamParams<S> = Vec<(Vec<(S, String)>, Option<Box<Term<S>>>)>;

/// The parameters to a dependent function type
pub type PiParams<S> = (Vec<(S, String)>, Box<Term<S>>);
