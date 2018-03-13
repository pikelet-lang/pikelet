use nameless::{AlphaEq, FreeName, GenId, Named};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl<'a> From<&'a str> for Ident {
    fn from(src: &'a str) -> Ident {
        Ident(String::from(src))
    }
}

impl From<String> for Ident {
    fn from(src: String) -> Ident {
        Ident(src)
    }
}

impl AlphaEq for Ident {
    fn alpha_eq(&self, other: &Ident) -> bool {
        self == other
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash, AlphaEq)]
pub enum Name {
    /// Names originating from user input
    User(Ident),
    /// A generated id with an optional string that may have come from user input
    Gen(Named<Option<Ident>, GenId>),
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<Ident>>(name: S) -> Name {
        Name::User(name.into())
    }

    pub fn name(&self) -> Option<&Ident> {
        match *self {
            Name::User(ref name) => Some(name),
            Name::Gen(Named { ref name, .. }) => name.as_ref(),
        }
    }
}

impl FreeName for Name {
    fn freshen(&mut self) {
        *self = match *self {
            Name::User(ref name) => Name::Gen(Named::new(Some(name.clone()), GenId::fresh())),
            Name::Gen(_) => return,
        };
    }
}

impl From<GenId> for Name {
    fn from(src: GenId) -> Name {
        Name::Gen(Named::new(None, src))
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
