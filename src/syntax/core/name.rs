use nameless::{FreeName, GenId, Named};
use std::fmt;

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// Names originating from user input
    User(String),
    /// A generated id with an optional string that may have come from user input
    Gen(Named<Option<String>, GenId>),
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }

    pub fn name(&self) -> Option<&str> {
        match *self {
            Name::User(ref name) => Some(name),
            Name::Gen(Named { ref name, .. }) => name.as_ref().map(String::as_str),
        }
    }
}

impl FreeName for Name {
    type Hint = String;

    fn fresh(hint: Option<String>) -> Name {
        Name::Gen(Named::new(hint, GenId::fresh())) // FIXME
    }

    fn hint(&self) -> Option<String> {
        match *self {
            Name::User(ref name) => Some(name.clone()),
            Name::Gen(Named { ref name, .. }) => name.clone(),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Gen(ref gen) => match gen.name {
                None => write!(f, "{}", gen.inner),
                Some(ref name) => write!(f, "{}{}", name, gen.inner),
            },
        }
    }
}
