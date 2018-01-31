use std::str::FromStr;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError(pub String);

#[derive(Debug, Clone)]
pub enum ReplCommand {
    Eval(Box<Term>),
    Help,
    NoOp,
    Quit,
    TypeOf(Box<Term>),
}

impl FromStr for ReplCommand {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<ReplCommand, ParseError> {
        grammar::parse_ReplCommand(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

/// A module definition
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The declarations contained in the module
    pub declarations: Vec<Declaration>,
}

impl FromStr for Module {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Module, ParseError> {
        grammar::parse_Module(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

/// Top level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Claims that a term abides by the given type
    Claim(String, Term),
    /// Declares the body of a term
    Definition(String, Vec<(String, Option<Box<Term>>)>, Term),
}

impl FromStr for Declaration {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Declaration, ParseError> {
        grammar::parse_Declaration(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

/// The AST of the concrete syntax
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(String),
    Type,
    Ann(Box<Term>, Box<Term>),
    Lam(Vec<(String, Option<Box<Term>>)>, Box<Term>),
    Pi(String, Box<Term>, Box<Term>),
    Arrow(Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl FromStr for Term {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Term, ParseError> {
        grammar::parse_Term(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn pi_bad_ident() {
        let parse_result = Term::from_str("((x : Type) : Type) -> Type");

        assert_eq!(
            parse_result,
            Err(ParseError(String::from("identifier expected in pi type"))),
        );
    }
}
