//! Parser utilities

use std::str::FromStr;

use syntax::concrete;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/syntax/parse/grammar.rs"));
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError(pub String);

impl FromStr for concrete::ReplCommand {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::ReplCommand, ParseError> {
        grammar::parse_ReplCommand(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

impl FromStr for concrete::Module {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Module, ParseError> {
        grammar::parse_Module(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

impl FromStr for concrete::Declaration {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Declaration, ParseError> {
        grammar::parse_Declaration(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

impl FromStr for concrete::Term {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<concrete::Term, ParseError> {
        grammar::parse_Term(src).map_err(|e| ParseError(format!("{}", e)))
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn pi_bad_ident() {
        let parse_result = concrete::Term::from_str("((x : Type) : Type) -> Type");

        assert_eq!(
            parse_result,
            Err(ParseError(String::from("identifier expected in pi type"))),
        );
    }
}
