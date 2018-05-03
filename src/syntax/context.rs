use im::Vector;
use nameless::{Ignore, Name};
use std::fmt;
use std::rc::Rc;

use syntax::core::{Term, Type};
use syntax::pretty::{self, ToDoc};

/// An entry in the context
#[derive(Debug, Clone, PartialEq)]
pub enum Entry {
    /// A type claim
    Claim(Name, Rc<Type>),
    /// A value definition
    Definition(Name, Rc<Term>),
}

/// A list of binders that have been accumulated during typechecking
///
/// We use a persistent vector internally so that we don't need to deal with the
/// error-prone tedium of dealing with a mutable context when entering and
/// exiting binders.
#[derive(Clone, PartialEq)]
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

    pub fn claim(&self, name: Name, ty: Rc<Type>) -> Context {
        Context {
            entries: self.entries.push_front(Entry::Claim(name, ty)),
        }
    }

    pub fn define(&self, name: Name, term: Rc<Term>) -> Context {
        Context {
            entries: self.entries.push_front(Entry::Definition(name, term)),
        }
    }

    pub fn lookup_claim(&self, name: &Name) -> Option<Rc<Type>> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Claim(ref n, ref ty) if n == name => Some(ty.clone()),
                Entry::Claim(_, _) | Entry::Definition(_, _) => None,
            })
            .next()
    }

    pub fn lookup_definition(&self, name: &Name) -> Option<Rc<Term>> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Definition(ref n, ref term) if n == name => Some(term.clone()),
                Entry::Definition(_, _) | Entry::Claim(_, _) => None,
            })
            .next()
    }
}

impl Default for Context {
    fn default() -> Context {
        use syntax::core::{Constant, Level, Value};

        let universe0 = Rc::new(Value::Universe(Level(0)));
        let constant = |c| Rc::new(Term::Constant(Ignore::default(), c));
        let constant_val = |c| Rc::new(Value::Constant(c));

        Context::new()
            .claim(Name::user("true"), constant_val(Constant::BoolType))
            .define(Name::user("true"), constant(Constant::Bool(true)))
            .claim(Name::user("false"), constant_val(Constant::BoolType))
            .define(Name::user("false"), constant(Constant::Bool(false)))
            .claim(Name::user("Bool"), universe0.clone())
            .define(Name::user("Bool"), constant(Constant::BoolType))
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
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtContextEntries<'a>(&'a Vector<Entry>);

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
