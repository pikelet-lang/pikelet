//! The core syntax of the language

use std::fmt;
use std::rc::Rc;

use concrete;
use pretty::{self, ToDoc};
use var::{Debruijn, Name, Named, Var};

#[cfg(test)]
mod tests;

/// A module definition
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

/// Top level definitions
pub struct Definition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: RcTerm,
    /// An optional type annotation to aid in type inference
    pub ann: Option<RcTerm>,
}

/// The core term syntax
///
/// ```text
/// e,τ ::= e:τ
///       | Type
///       | x
///       | λx:τ₁.τ₂
///       | Πx:τ₁.τ₂
///       | τ₁ τ₂
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Type of types
    Type,
    /// A variable
    Var(Var),
    /// Lambda abstractions
    Lam(Named<Option<RcTerm>>, RcTerm),
    /// Dependent function types
    Pi(Named<RcTerm>, RcTerm),
    /// Term application
    App(RcTerm, RcTerm),
}

impl From<Var> for Term {
    fn from(src: Var) -> Term {
        Term::Var(src)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// Normal forms
///
/// ```text
/// v,ρ ::= Type
///       | x
///       | λx:ρ₁.ρ₂
///       | Πx:ρ₁.ρ₂
///       | ρ₁ ρ₂
///```
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The type of types
    Type,
    /// Variables
    Var(Var),
    /// A lambda abstraction
    Lam(Named<Option<RcValue>>, RcValue),
    /// A pi type
    Pi(Named<RcValue>, RcValue),
    /// Term application
    App(RcValue, RcValue),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

// Wrapper types

macro_rules! make_wrapper {
    ($name:ident, $wrapper:ident, $inner:ty) => {
        #[derive(Clone, PartialEq)]
        pub struct $name {
            pub inner: $wrapper<$inner>,
        }

        impl From<$inner> for $name {
            fn from(src: $inner) -> $name {
                $name {
                    inner: $wrapper::new(src),
                }
            }
        }

        impl $crate::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut $crate::std::fmt::Formatter) -> $crate::std::fmt::Result {
                $crate::std::fmt::Debug::fmt(&self.inner, f)
            }
        }

        impl $crate::std::fmt::Display for $name {
            fn fmt(&self, f: &mut $crate::std::fmt::Formatter) -> $crate::std::fmt::Result {
                $crate::std::fmt::Display::fmt(&self.inner, f)
            }
        }
    };
}

make_wrapper!(RcTerm, Rc, Term);
make_wrapper!(RcValue, Rc, Value);

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

// Abstraction and instantiation

impl RcTerm {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Term::Ann(ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
            },
            Term::Type => {},
            Term::Var(ref mut var) => var.close_at(level, name),
            Term::Lam(Named(_, None), ref mut body) => body.close_at(level.succ(), name),
            Term::Lam(Named(_, Some(ref mut ty)), ref mut body) => {
                ty.close_at(level, name);
                body.close_at(level.succ(), name);
            },
            Term::Pi(Named(_, ref mut ty), ref mut body) => {
                ty.close_at(level, name);
                body.close_at(level.succ(), name);
            },
            Term::App(ref mut f, ref mut x) => {
                f.close_at(level, name);
                x.close_at(level, name);
            },
        }
    }
}

impl RcValue {
    pub fn open(&self, x: &RcValue) -> RcValue {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcValue) -> RcValue {
        match *self.inner {
            Value::Type => self.clone(),
            Value::Var(ref var) => match var.open_at(level) {
                true => x.clone(),
                false => self.clone(),
            },
            Value::Lam(Named(ref name, ref param_ty), ref body) => {
                let param_ty = param_ty.as_ref().map(|param_ty| param_ty.open_at(level, x));
                let body = body.open_at(level.succ(), x);

                Value::Lam(Named(name.clone(), param_ty), body).into()
            },
            Value::Pi(Named(ref name, ref param_ty), ref body) => {
                let param_ty = param_ty.open_at(level, x);
                let body = body.open_at(level.succ(), x);

                Value::Pi(Named(name.clone(), param_ty), body).into()
            },
            Value::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Value::App(fn_expr, arg).into()
            },
        }
    }
}

// Conversions from the parse tree

fn lam_from_concrete(
    params: &[(String, Option<Box<concrete::Term>>)],
    body: &concrete::Term,
) -> RcTerm {
    let mut term = RcTerm::from_concrete(body);

    for &(ref name, ref ann) in params.iter().rev() {
        let name = Name::User(name.clone());
        term.close(&name);
        term = match *ann {
            None => Term::Lam(Named(name, None), term).into(),
            Some(ref ann) => {
                let ann = RcTerm::from_concrete(ann).into();
                Term::Lam(Named(name, Some(ann)), term).into()
            },
        };
    }

    term
}

impl Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    pub fn from_concrete(module: &concrete::Module) -> Module {
        use std::collections::BTreeMap;
        use std::collections::btree_map::Entry;

        // The type claims that we have encountered so far! We'll use these when
        // we encounter their corresponding definitions later as type annotations
        let mut claims = BTreeMap::new();
        // The definitions, desugared from the concrete syntax
        let mut definitions = Vec::<Definition>::new();

        for declaration in &module.declarations {
            match *declaration {
                // We've enountered a claim! Let's try to add it to the claims
                // that we've seen so far...
                concrete::Declaration::Claim(ref name, ref term) => {
                    match claims.entry(name) {
                        // Oh no! We've already seen a claim for this name!
                        Entry::Occupied(_) => panic!(), // FIXME: Better error
                        // This name does not yet have a claim associated with it
                        Entry::Vacant(mut entry) => {
                            let mut term = RcTerm::from_concrete(term);

                            for (level, definition) in definitions.iter().rev().enumerate() {
                                term.close_at(
                                    Debruijn(level as u32),
                                    &Name::user(definition.name.clone()),
                                );
                            }

                            entry.insert(term)
                        },
                    };
                },
                // We've encountered a definition. Let's desugar it!
                concrete::Declaration::Definition(ref name, ref params, ref term) => {
                    let name = name.clone();
                    let mut term = lam_from_concrete(params, term);
                    let ann = claims.remove(&name);

                    for (level, definition) in definitions.iter().rev().enumerate() {
                        term.close_at(Debruijn(level as u32), &Name::user(definition.name.clone()));
                    }

                    definitions.push(Definition { name, term, ann });
                },
            }
        }

        // FIXME: Better error
        assert!(claims.is_empty());

        Module {
            name: module.name.clone(),
            definitions,
        }
    }
}

impl RcTerm {
    /// Convert a term in the concrete syntax into a core term
    pub fn from_concrete(term: &concrete::Term) -> RcTerm {
        match *term {
            concrete::Term::Parens(ref term) => RcTerm::from_concrete(term),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = RcTerm::from_concrete(expr).into();
                let ty = RcTerm::from_concrete(ty).into();

                Term::Ann(expr, ty).into()
            },
            concrete::Term::Type => Term::Type.into(),
            concrete::Term::Var(ref x) => Term::Var(Var::Free(Name::User(x.clone()))).into(),
            concrete::Term::Lam(ref params, ref body) => lam_from_concrete(params, body),
            concrete::Term::Pi(ref name, ref ann, ref body) => {
                let name = Name::User(name.clone());
                let mut body = RcTerm::from_concrete(body);
                body.close(&name);

                Term::Pi(Named(name, RcTerm::from_concrete(ann).into()), body).into()
            },
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = Name::Abstract;
                let mut body = RcTerm::from_concrete(body);
                body.close(&name);

                Term::Pi(Named(name, RcTerm::from_concrete(ann).into()), body).into()
            },
            concrete::Term::App(ref fn_expr, ref arg) => {
                let fn_expr = RcTerm::from_concrete(fn_expr).into();
                let arg = RcTerm::from_concrete(arg).into();

                Term::App(fn_expr, arg).into()
            },
        }
    }
}
