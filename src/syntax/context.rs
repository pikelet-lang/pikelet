use im::Vector;
use moniker::FreeVar;
use std::fmt;

use syntax::core::{RcTerm, RcType, Term};
use syntax::pretty::{self, ToDoc};

/// An entry in the context
#[derive(Debug, Clone)]
pub enum Entry {
    /// A type claim
    Claim(FreeVar<String>, RcType),
    /// A value definition
    Definition(FreeVar<String>, RcTerm),
}

/// A list of binders that have been accumulated during type checking
///
/// We use a persistent vector internally so that we don't need to deal with the
/// error-prone tedium of dealing with a mutable context when entering and
/// exiting binders.
#[derive(Clone, Debug)]
pub struct Context {
    pub entries: Vector<Entry>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            entries: Vector::new(),
        }
    }

    pub fn claim(&self, name: FreeVar<String>, ann: RcType) -> Context {
        let mut entries = self.entries.clone();
        entries.push_front(Entry::Claim(name, ann));
        Context { entries }
    }

    pub fn define_term(&self, name: FreeVar<String>, ann: RcType, term: RcTerm) -> Context {
        let mut entries = self.entries.clone();
        entries.push_front(Entry::Claim(name.clone(), ann));
        entries.push_front(Entry::Definition(name, term));
        Context { entries }
    }

    pub fn lookup_claim(&self, name: &FreeVar<String>) -> Option<RcType> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Claim(ref n, ref ty) if n == name => Some(ty.clone()),
                Entry::Claim(_, _) | Entry::Definition(_, _) => None,
            })
            .next()
    }

    pub fn lookup_definition(&self, name: &FreeVar<String>) -> Option<RcTerm> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Definition(ref n, ref def) if n == name => Some(def.clone()),
                Entry::Definition(_, _) | Entry::Claim(_, _) => None,
            })
            .next()
    }
}

impl Default for Context {
    fn default() -> Context {
        use moniker::{Binder, Embed, GenId, Scope, Var};

        use syntax::core::{Literal, RcTerm, RcValue, Value};
        use syntax::Level;

        let name = FreeVar::user;
        let fresh_binder = || Binder(FreeVar::from(GenId::fresh()));
        let free_var = |n| RcValue::from(Value::from(Var::Free(name(n))));
        let universe0 = RcValue::from(Value::Universe(Level(0)));
        let bool_lit = |val| RcTerm::from(Term::Literal(Literal::Bool(val)));

        Context::new()
            .claim(name("Bool"), universe0.clone())
            .define_term(name("true"), free_var("Bool"), bool_lit(true))
            .define_term(name("false"), free_var("Bool"), bool_lit(false))
            .claim(name("String"), universe0.clone())
            .claim(name("Char"), universe0.clone())
            .claim(name("U8"), universe0.clone())
            .claim(name("U16"), universe0.clone())
            .claim(name("U32"), universe0.clone())
            .claim(name("U64"), universe0.clone())
            .claim(name("I8"), universe0.clone())
            .claim(name("I16"), universe0.clone())
            .claim(name("I32"), universe0.clone())
            .claim(name("I64"), universe0.clone())
            .claim(name("F32"), universe0.clone())
            .claim(name("F64"), universe0.clone())
            .claim(
                name("Array"),
                RcValue::from(Value::Pi(Scope::new(
                    (fresh_binder(), Embed(free_var("U64"))),
                    RcValue::from(Value::Pi(Scope::new(
                        (fresh_binder(), Embed(universe0.clone())),
                        universe0.clone(),
                    ))),
                ))),
            )
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}
