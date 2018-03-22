use nameless::{Bound, BoundPattern, BoundTerm, GenId, PatternIndex, ScopeState};
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

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// Names originating from user input
    User(Ident),
    /// A generated id with an optional string that may have come from user input
    Gen(Option<Ident>, GenId),
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<Ident>>(name: S) -> Name {
        Name::User(name.into())
    }

    pub fn name(&self) -> Option<&Ident> {
        match *self {
            Name::User(ref name) => Some(name),
            Name::Gen(ref name, _) => name.as_ref(),
        }
    }
}

impl BoundTerm for Name {
    type Free = Name;

    fn term_eq(&self, other: &Name) -> bool {
        match (self, other) {
            (&Name::User(ref lhs), &Name::User(ref rhs)) => lhs == rhs,
            (&Name::Gen(_, ref lhs), &Name::Gen(_, ref rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn close_term<P>(&mut self, _: ScopeState, _: &P)
    where
        P: BoundPattern<Free = Name>,
    {
    }

    fn open_term<P>(&mut self, _: ScopeState, _: &P)
    where
        P: BoundPattern<Free = Name>,
    {
    }
}

impl BoundPattern for Name {
    type Free = Name;

    fn pattern_eq(&self, _other: &Name) -> bool {
        true
    }

    fn freshen(&mut self) -> Vec<Name> {
        *self = match *self {
            Name::User(ref name) => Name::Gen(Some(name.clone()), GenId::fresh()),
            Name::Gen(_, _) => return vec![self.clone()],
        };
        vec![self.clone()]
    }

    fn rename(&mut self, perm: &[Name]) {
        assert_eq!(perm.len(), 1); // FIXME: assert
        *self = perm[0].clone(); // FIXME: double clone
    }

    fn close_pattern<P>(&mut self, _: ScopeState, _: &P)
    where
        P: BoundPattern<Free = Name>,
    {
    }

    fn open_pattern<P>(&mut self, _: ScopeState, _: &P)
    where
        P: BoundPattern<Free = Name>,
    {
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
        match name == self {
            true => Some(Bound {
                scope: state.depth(),
                pattern: PatternIndex(0),
            }),
            false => None,
        }
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free> {
        match name.scope == state.depth() {
            true => {
                assert_eq!(name.pattern, PatternIndex(0));
                Some(self.clone())
            },
            false => None,
        }
    }
}

impl From<GenId> for Name {
    fn from(src: GenId) -> Name {
        Name::Gen(None, src)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Gen(ref name, ref gen_id) => match *name {
                None => write!(f, "{}", gen_id),
                Some(ref name) => write!(f, "{}{}", name, gen_id),
            },
        }
    }
}
