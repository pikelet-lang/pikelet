//! The core syntax of the language

use rpds::List;
use std::fmt;
use std::rc::Rc;

use syntax::concrete;
use syntax::pretty::{self, ToDoc};
use syntax::var::{Debruijn, GenId, Named, Var};

#[cfg(test)]
mod tests;

/// The name of a free variable
#[derive(Debug, Clone)]
pub enum Name {
    /// Names originating from user input
    User(String),
    /// A generated id with an optional string that may have come from user input
    Gen(Named<Option<String>, GenId>),
    /// Abstract names, `_`
    ///
    /// These are generally used in non-dependent function types, ie:
    ///
    /// ```text
    /// t1 -> t2 -> t3
    /// ```
    ///
    /// will be stored as:
    ///
    /// ```text
    /// (_ : t1) -> (_ : t2) -> t3
    /// ```
    ///
    /// They should never actually appear as variables in terms.
    ///
    /// Comparing two abstract names will always return false because we cannot
    /// be sure what they actually refer to. For example, in the type
    /// shown above, `_` could refer to either `t1` or `t2`.
    Abstract,
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }

    /// Generate a new, globally unique name
    pub fn fresh<S: Into<String>>(name: Option<S>) -> Name {
        Name::Gen(Named(name.map(S::into), GenId::fresh()))
    }

    pub fn name(&self) -> Option<&str> {
        match *self {
            Name::User(ref name) | Name::Gen(Named(Some(ref name), _)) => Some(name),
            Name::Gen(Named(None, _)) | Name::Abstract => None,
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        match (self, other) {
            (&Name::User(ref lhs), &Name::User(ref rhs)) => lhs == rhs,
            (&Name::Gen(ref lhs), &Name::Gen(ref rhs)) => lhs == rhs,
            (&Name::Abstract, &Name::Abstract) | (_, _) => false,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Gen(Named(None, ref id)) => write!(f, "{}", id),
            Name::Gen(Named(Some(ref name), ref id)) => write!(f, "{}{}", name, id),
            Name::Abstract => write!(f, "_"),
        }
    }
}

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
/// e,ρ ::= e:ρ         1. annotated terms
///       | Type        2. universes
///       | x           3. variables
///       | λx:ρ₁.ρ₂    4. lambda abstractions
///       | Πx:ρ₁.ρ₂    5. dependent function types
///       | ρ₁ ρ₂       6. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm), // 1.
    /// Universes
    Universe, // 2.
    /// A variable
    Var(Var<Name>), // 3.
    /// Lambda abstractions
    Lam(TermLam), // 4.
    /// Dependent function types
    Pi(TermPi), // 5.
    /// Term application
    App(RcTerm, RcTerm), // 6.
}

impl From<Var<Name>> for Term {
    fn from(src: Var<Name>) -> Term {
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

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct TermLam {
    pub unsafe_param: Named<Name, Option<RcTerm>>,
    pub unsafe_body: RcTerm,
}

impl TermLam {
    pub fn bind(param: Named<Name, Option<RcTerm>>, mut body: RcTerm) -> TermLam {
        body.close(&param.0);

        TermLam {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, Option<RcTerm>>, RcTerm) {
        let fv = Name::fresh(self.unsafe_param.0.name());
        self.unsafe_param.0 = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Term::Var(Var::Free(fv)).into()),
        )
    }
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct TermPi {
    pub unsafe_param: Named<Name, RcTerm>,
    pub unsafe_body: RcTerm,
}

impl TermPi {
    pub fn bind(param: Named<Name, RcTerm>, mut body: RcTerm) -> TermPi {
        body.close(&param.0);

        TermPi {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, RcTerm>, RcTerm) {
        let fv = Name::fresh(self.unsafe_param.0.name());
        self.unsafe_param.0 = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Term::Var(Var::Free(fv)).into()),
        )
    }
}

/// Normal forms
///
/// ```text
/// v,τ ::= Type        1. universes
///       | x           2. variables
///       | λx:τ₁.τ₂    3. lambda abstractions
///       | Πx:τ₁.τ₂    4. dependent function types
///       | τ₁ τ₂       5. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Universes
    Universe, // 1.
    /// Variables
    Var(Var<Name>), // 2.
    /// A lambda abstraction
    Lam(ValueLam), // 3.
    /// A pi type
    Pi(ValuePi), // 4.
    /// Term application
    App(RcValue, RcValue), // 5.
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct ValueLam {
    pub unsafe_param: Named<Name, Option<RcValue>>,
    pub unsafe_body: RcValue,
}

impl ValueLam {
    pub fn bind(param: Named<Name, Option<RcValue>>, mut body: RcValue) -> ValueLam {
        body.close(&param.0);

        ValueLam {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, Option<RcValue>>, RcValue) {
        let fv = Name::fresh(self.unsafe_param.0.name());
        self.unsafe_param.0 = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
        )
    }
}

// TODO: Reduce boilderplate with a name binding abstraction
#[derive(Debug, Clone, PartialEq)]
pub struct ValuePi {
    pub unsafe_param: Named<Name, RcValue>,
    pub unsafe_body: RcValue,
}

impl ValuePi {
    pub fn bind(param: Named<Name, RcValue>, mut body: RcValue) -> ValuePi {
        body.close(&param.0);

        ValuePi {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, RcValue>, RcValue) {
        let fv = Name::fresh(self.unsafe_param.0.name());
        self.unsafe_param.0 = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
        )
    }
}

// TODO: Would be nice for this to be more polymorphic
pub fn unbind2(
    mut lam: TermLam,
    mut pi: ValuePi,
) -> (
    Named<Name, Option<RcTerm>>,
    RcTerm,
    Named<Name, RcValue>,
    RcValue,
) {
    let fv = Name::fresh(lam.unsafe_param.0.name());
    lam.unsafe_param.0 = fv.clone();
    pi.unsafe_param.0 = fv.clone();
    (
        lam.unsafe_param,
        lam.unsafe_body
            .open(&Term::Var(Var::Free(fv.clone())).into()),
        pi.unsafe_param,
        pi.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
    )
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

impl RcTerm {
    pub fn universe() -> RcTerm {
        Term::Universe.into()
    }
}

impl RcValue {
    pub fn universe() -> RcValue {
        Value::Universe.into()
    }
}

/// A binder that introduces a variable into the context
///
/// ```text
/// b ::= λx:τ           1. lambda abstraction
///     | Πx:τ           2. dependent function
///     | let x:τ = v    3. let binding
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam(Option<RcType>), // 1.
    /// A type introduced after entering a pi type
    Pi(RcType), // 2.
    /// A value and type binding that was introduced by passing over a let binding
    Let(RcValue, RcType), // 3.
}

/// A list of binders that have been accumulated during typechecking
///
/// ```text
/// Γ ::= ε           1. empty context
///     | Γ,b         2. context extension
/// ```
#[derive(Clone, PartialEq)]
pub struct Context {
    pub binders: List<(Name, Binder)>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, name: Name, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front((name, binder)),
        }
    }

    pub fn lookup_binder(&self, name: &Name) -> Option<&Binder> {
        self.binders
            .iter()
            .find(|&&(ref n, _)| n == name)
            .map(|&(_, ref b)| b)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtBinders<'a>(&'a List<(Name, Binder)>);

        impl<'a> fmt::Debug for FmtBinders<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("binders", &FmtBinders(&self.binders))
            .finish()
    }
}

// Nameplate!

impl RcTerm {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Term::Ann(ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
                return;
            },
            Term::Universe => return,
            Term::Var(Var::Free(ref n)) if n == name => {
                Term::Var(Var::Bound(Named(n.clone(), level))).into()
            },
            Term::Var(Var::Bound(_)) | Term::Var(Var::Free(_)) => return,
            Term::Lam(ref mut lam) => {
                lam.unsafe_param
                    .1
                    .as_mut()
                    .map(|param| param.close_at(level, name));
                lam.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Term::Pi(ref mut pi) => {
                pi.unsafe_param.1.close_at(level, name);
                pi.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Term::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
                return;
            },
        };
    }

    pub fn open(&self, x: &RcTerm) -> RcTerm {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcTerm) -> RcTerm {
        match *self.inner {
            Term::Ann(ref expr, ref ty) => {
                let expr = expr.open_at(level, x);
                let ty = ty.open_at(level, x);

                Term::App(expr.clone(), ty.clone()).into()
            },
            Term::Universe => self.clone(),
            Term::Var(Var::Bound(Named(_, index))) if index == level => x.clone(),
            Term::Var(Var::Bound(_)) | Term::Var(Var::Free(_)) => self.clone(),
            Term::Lam(ref lam) => {
                let param_ty = lam.unsafe_param
                    .1
                    .as_ref()
                    .map(|param_ty| param_ty.open_at(level, x));
                let body = lam.unsafe_body.open_at(level.succ(), x);

                Term::Lam(TermLam {
                    unsafe_param: Named(lam.unsafe_param.0.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Term::Pi(ref pi) => {
                let param_ty = pi.unsafe_param.1.open_at(level, x);
                let body = pi.unsafe_body.open_at(level.succ(), x);

                Term::Pi(TermPi {
                    unsafe_param: Named(pi.unsafe_param.0.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Term::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Term::App(fn_expr, arg).into()
            },
        }
    }

    pub fn subst(&mut self, name: &Name, x: &RcTerm) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Term::Ann(ref mut expr, ref mut ty) => {
                expr.subst(name, x);
                ty.subst(name, x);
                return;
            },
            Term::Universe => return,
            Term::Var(Var::Free(ref n)) if n == name => x.clone(),
            Term::Var(Var::Free(_)) | Term::Var(Var::Bound(_)) => return,
            Term::Lam(ref mut lam) => {
                lam.unsafe_param
                    .1
                    .as_mut()
                    .map(|param| param.subst(name, x));
                lam.unsafe_body.subst(name, x);
                return;
            },
            Term::Pi(ref mut pi) => {
                pi.unsafe_param.1.subst(name, x);
                pi.unsafe_body.subst(name, x);
                return;
            },
            Term::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.subst(name, x);
                arg_expr.subst(name, x);
                return;
            },
        };
    }
}

impl RcValue {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Value::Universe => return,
            Value::Var(Var::Free(ref n)) if n == name => {
                Value::Var(Var::Bound(Named(n.clone(), level))).into()
            },
            Value::Var(Var::Bound(_)) | Value::Var(Var::Free(_)) => return,
            Value::Lam(ref mut lam) => {
                lam.unsafe_param
                    .1
                    .as_mut()
                    .map(|param| param.close_at(level, name));
                lam.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Value::Pi(ref mut pi) => {
                pi.unsafe_param.1.close_at(level, name);
                pi.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Value::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
                return;
            },
        };
    }

    pub fn open(&self, x: &RcValue) -> RcValue {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcValue) -> RcValue {
        match *self.inner {
            Value::Universe => self.clone(),
            Value::Var(Var::Bound(Named(_, index))) if index == level => x.clone(),
            Value::Var(Var::Bound(_)) | Value::Var(Var::Free(_)) => self.clone(),
            Value::Lam(ref lam) => {
                let param_ty = lam.unsafe_param
                    .1
                    .as_ref()
                    .map(|param_ty| param_ty.open_at(level, x));
                let body = lam.unsafe_body.open_at(level.succ(), x);

                Value::Lam(ValueLam {
                    unsafe_param: Named(lam.unsafe_param.0.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Value::Pi(ref pi) => {
                let param_ty = pi.unsafe_param.1.open_at(level, x);
                let body = pi.unsafe_body.open_at(level.succ(), x);

                Value::Pi(ValuePi {
                    unsafe_param: Named(pi.unsafe_param.0.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Value::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Value::App(fn_expr, arg).into()
            },
        }
    }

    pub fn subst(&mut self, name: &Name, x: &RcValue) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Value::Universe => return,
            Value::Var(Var::Free(ref n)) if n == name => x.clone(),
            Value::Var(Var::Free(_)) | Value::Var(Var::Bound(_)) => return,
            Value::Lam(ref mut lam) => {
                lam.unsafe_param
                    .1
                    .as_mut()
                    .map(|param| param.subst(name, x));
                lam.unsafe_body.subst(name, x);
                return;
            },
            Value::Pi(ref mut pi) => {
                pi.unsafe_param.1.subst(name, x);
                pi.unsafe_body.subst(name, x);
                return;
            },
            Value::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.subst(name, x);
                arg_expr.subst(name, x);
                return;
            },
        };
    }
}

// Conversions from the concrete syntax

fn lam_from_concrete(
    params: &[(String, Option<Box<concrete::Term>>)],
    body: &concrete::Term,
) -> RcTerm {
    let mut term = RcTerm::from_concrete(body);

    for &(ref name, ref ann) in params.iter().rev() {
        let name = Name::User(name.clone());
        term = match *ann {
            None => Term::Lam(TermLam::bind(Named(name, None), term)).into(),
            Some(ref ann) => {
                let ann = RcTerm::from_concrete(ann);
                Term::Lam(TermLam::bind(Named(name, Some(ann)), term)).into()
            },
        };
    }

    term
}

fn pi_from_concrete(param_names: &[String], ann: &concrete::Term, body: &concrete::Term) -> RcTerm {
    let ann = RcTerm::from_concrete(ann);
    let mut term = RcTerm::from_concrete(body);

    for name in param_names.iter().rev() {
        // This could be wrong... :/
        term = Term::Pi(TermPi::bind(
            Named(Name::User(name.clone()), ann.clone()),
            term,
        )).into();
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
                concrete::Declaration::Import(_, _, _) => unimplemented!("import declarations"),
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
            concrete::Term::Universe => RcTerm::universe(),
            concrete::Term::Var(ref x) => Term::Var(Var::Free(Name::User(x.clone()))).into(),
            concrete::Term::Lam(ref params, ref body) => lam_from_concrete(params, body),
            concrete::Term::Pi(ref names, ref ann, ref body) => pi_from_concrete(names, ann, body),
            concrete::Term::Arrow(ref ann, ref body) => Term::Pi(TermPi::bind(
                Named(Name::Abstract, RcTerm::from_concrete(ann).into()),
                RcTerm::from_concrete(body),
            )).into(),
            concrete::Term::App(ref fn_expr, ref arg) => {
                let fn_expr = RcTerm::from_concrete(fn_expr).into();
                let arg = RcTerm::from_concrete(arg).into();

                Term::App(fn_expr, arg).into()
            },
        }
    }
}
