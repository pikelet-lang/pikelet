//! The concrete syntax of the language

/// Commands entered in the REPL
#[derive(Debug, Clone)]
pub enum ReplCommand {
    /// Evaluate a term
    ///
    /// ```text
    /// <term>
    /// ```
    Eval(Box<Term>),
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
    TypeOf(Box<Term>),
}

/// A module definition
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The declarations contained in the module
    pub declarations: Vec<Declaration>,
}

/// Top level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Claims that a term abides by the given type
    Claim(String, Term),
    /// Declares the body of a term
    Definition(String, Vec<(String, Option<Box<Term>>)>, Term),
}

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (e)
    /// ```
    Parens(Box<Term>),
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
    Universe,
    /// Variables
    ///
    /// ```text
    /// x
    /// ```
    Var(String),
    /// Lambda abstractions
    ///
    /// ```text
    /// \x => t
    /// \x : t1 => t2
    /// ```
    Lam(Vec<(String, Option<Box<Term>>)>, Box<Term>),
    /// Dependent function types
    ///
    /// ```text
    /// (x : t1) -> t2
    /// ```
    Pi(Vec<String>, Box<Term>, Box<Term>),
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
    App(Box<Term>, Box<Term>),
}
