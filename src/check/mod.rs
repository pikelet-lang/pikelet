//! Contexts and type checking

use rpds::List;

use core::{Module, RcTerm, RcType, RcValue, Term, Value};
use var::{Debruijn, Name, Named, Var};

#[cfg(test)]
mod tests;

/// A typechecked and elaborated module
pub struct CheckedModule {
    pub name: String,
    pub definitions: Vec<CheckedDefinition>,
}

/// A typechecked and elaborated definition
pub struct CheckedDefinition {
    pub name: String,
    pub term: RcValue,
    pub ann: RcType,
}

/// Typecheck and elaborate a module
pub fn check_module(module: &Module) -> Result<CheckedModule, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(module.definitions.len());

    for definition in &module.definitions {
        let ann = match definition.ann {
            // We don't have a type annotation available to us! Instead we will attempt to infer it
            // based on the body of the definition
            None => context.infer(&definition.term)?,
            // We have a type annotation! Evaluate it to its normal form, then check that it matches
            // the body of the definition
            Some(ref ann) => {
                let ann = context.normalize(&ann);
                context.check(&definition.term, &ann)?;
                ann
            },
        };
        // Evaluate the body of the definition
        let term = context.normalize(&definition.term);
        let name = definition.name.clone();
        // Add the definition to the context
        context = context.extend(Binder::Let(term.clone(), ann.clone()));

        definitions.push(CheckedDefinition { name, term, ann })
    }

    Ok(CheckedModule {
        name: module.name.clone(),
        definitions,
    })
}

/// An error produced during typechecking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    IllegalApplication,
    TypeAnnotationsNeeded,
    ExpectedFunction {
        lam_expr: RcTerm,
        expected: RcType,
    },
    Mismatch {
        expr: RcTerm,
        found: RcType,
        expected: RcType,
    },
    UnboundVariable(Name),
}

/// A binder that introduces a variable into the context
#[derive(Debug, Clone)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam(Option<RcType>),
    /// A type introduced after entering a pi type
    Pi(RcType),
    /// A value and type binding that was introduced by passing over a let binding
    Let(RcValue, RcType),
}

impl Binder {
    /// Return the value associated with a binder
    pub fn value(&self) -> Option<&RcValue> {
        match *self {
            Binder::Lam(_) | Binder::Pi(_) => None,
            Binder::Let(_, ref ty) => Some(ty),
        }
    }

    /// Return the type associated with a binder
    pub fn ty(&self) -> Option<&RcType> {
        match *self {
            Binder::Lam(ref ty) => ty.as_ref(),
            Binder::Pi(ref ty) | Binder::Let(_, ref ty) => Some(ty),
        }
    }
}

/// A list of binders that have been accumulated during typechecking
#[derive(Debug, Clone)]
pub struct Context {
    // Γ ::= ε           1. empty context
    //     | Γ,x:τ       2. context extension
    binders: List<Binder>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front(binder),
        }
    }

    /// Look up a binder based on the given Debruijn index
    pub fn lookup_binder(&self, index: Debruijn) -> Option<&Binder> {
        self.binders.iter().nth(index.0 as usize)
    }

    /// Evaluates a core term to its normal form
    pub fn normalize(&self, term: &RcTerm) -> RcValue {
        // FIXME: judgements do not mention the context :/
        // e ⇓ v
        match *term.inner {
            //  1.  e ⇓ v
            // ────────────────── (EVAL/ANN)
            //      e : ρ ⇓ v
            Term::Ann(ref expr, _) => {
                self.normalize(expr) // 1.
            },

            // ───────────── (EVAL/TYPE)
            //  Type ⇓ Type
            Term::Type => Value::Type.into(),

            Term::Var(ref var) => match *var {
                // ─────── (EVAL/Var)
                //  x ⇓ x
                Var::Free(_) => Value::Var(var.clone()).into(),
                Var::Bound(Named(_, index)) => match self.lookup_binder(index) {
                    Some(binder) => match binder.value() {
                        Some(value) => value.clone(),
                        None => Value::Var(var.clone()).into(),
                    },
                    None => panic!("ICE: index {} out of bounds", index),
                },
            },

            //  1. e ⇓ v
            // ───────────────── (EVAL/LAM)
            //     λx.e ⇓ λx→v
            Term::Lam(Named(ref name, None), ref body_expr) => {
                let binder = Binder::Lam(None);
                let body_expr = self.extend(binder).normalize(body_expr); // 1.

                Value::Lam(Named(name.clone(), None), body_expr).into()
            },

            //  1.  ρ ⇓ τ
            //  2.  e ⇓ v
            // ──────────────────────── (EVAL/LAM-ANN)
            //      λx:ρ→e ⇓ λx:τ→v
            Term::Lam(Named(ref name, Some(ref param_ty)), ref body_expr) => {
                let param_ty = self.normalize(param_ty); // 1.
                let binder = Binder::Lam(Some(param_ty.clone()));
                let body_expr = self.extend(binder).normalize(body_expr); // 2.

                Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into()
            },

            //  1.  ρ₁ ⇓ τ₁
            //  2.  ρ₂ ⇓ τ₂
            // ─────────────────────────── (EVAL/PI-ANN)
            //      (x:ρ₁)→ρ₂ ⇓ (x:τ₁)→τ₂
            Term::Pi(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = self.normalize(param_ty); // 1.
                let binder = Binder::Pi(param_ty.clone());
                let body_expr = self.extend(binder).normalize(body_expr); // 2.

                Value::Pi(Named(name.clone(), param_ty), body_expr).into()
            },

            // Perform [β-reduction](https://en.wikipedia.org/wiki/Lambda_calculus#β-reduction),
            // ie. apply functions to their arguments
            //
            //  1.  e₁ ⇓ λx→v₁
            //  2.  v₁[x↦e₂] ⇓ v₂
            // ───────────────────── (EVAL/APP)
            //      e₁ e₂ ⇓ v₂
            Term::App(ref fn_expr, ref arg) => {
                let fn_expr = self.normalize(fn_expr); // 1.
                let arg = self.normalize(arg); // 2.

                match *fn_expr.inner {
                    Value::Lam(_, ref body) => body.open(&arg),
                    _ => Value::App(fn_expr.clone(), arg).into(),
                }
            },
        }
    }

    /// Check that the given term has the expected type
    pub fn check(&self, term: &RcTerm, expected: &RcType) -> Result<(), TypeError> {
        // Γ ⊢ e :↓ τ
        match *term.inner {
            //  1.  Γ, x:τ₁ ⊢ e :↓ τ₂
            // ─────────────────────────────── (CHECK/LAM)
            //      Γ ⊢ λx→e :↓ (x:τ₁)→τ₂
            Term::Lam(Named(_, None), ref body_expr) => match *expected.inner {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    let binder = Binder::Pi(param_ty.clone());
                    self.extend(binder).check(body_expr, ret_ty) // 1.
                },
                _ => Err(TypeError::ExpectedFunction {
                    lam_expr: term.clone(),
                    expected: expected.clone(),
                }),
            },

            //  1.  Γ ⊢ e :↑ τ
            // ─────────────────── (CHECK/INFER)
            //      Γ ⊢ e :↓ τ
            _ => {
                let inferred_ty = self.infer(term)?; // 1.
                match &inferred_ty == expected {
                    true => Ok(()),
                    false => Err(TypeError::Mismatch {
                        expr: term.clone(),
                        found: inferred_ty,
                        expected: expected.clone(),
                    }),
                }
            },
        }
    }

    /// Infer the type of the given term
    pub fn infer(&self, term: &RcTerm) -> Result<RcType, TypeError> {
        // Γ ⊢ e :↑ τ
        match *term.inner {
            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ ⇓ τ
            //  3.  Γ ⊢ e :↓ τ
            // ───────────────────────── (INFER/ANN)
            //      Γ ⊢ (e : ρ) :↑ τ
            Term::Ann(ref expr, ref ty) => {
                self.check(ty, &Value::Type.into())?; // 1.
                let simp_ty = self.normalize(&ty); // 2.
                self.check(expr, &simp_ty)?; // 3.
                Ok(simp_ty)
            },

            // ─────────────────── (INFER/TYPE)
            //  Γ ⊢ TYPE :↑ Type
            Term::Type => Ok(Value::Type.into()),

            Term::Lam(Named(_, None), _) => {
                // TODO: More error info
                Err(TypeError::TypeAnnotationsNeeded)
            },

            //  1.  Γ ⊢ ρ :↓ Type
            //  2.  ρ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ e :↑ τ₂
            // ─────────────────────────────── (INFER/LAM)
            //      Γ ⊢ λx:ρ→e :↑ (x:τ₁)→τ₂
            Term::Lam(Named(ref param_name, Some(ref param_ty)), ref body_expr) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.normalize(&param_ty); // 2.
                let binder = Binder::Pi(simp_param_ty.clone());
                let body_ty = self.extend(binder).infer(body_expr)?; // 3.

                Ok(Value::Pi(Named(param_name.clone(), simp_param_ty), body_ty).into())
            },

            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ₁ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ ρ₂ :↓ Type
            // ────────────────────────────── (INFER/PI)
            //      Γ ⊢ (x:ρ₁)→ρ₂ :↑ Type
            Term::Pi(Named(_, ref param_ty), ref body_ty) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.normalize(&param_ty); // 2.
                let binder = Binder::Pi(simp_param_ty);
                self.extend(binder).check(body_ty, &Value::Type.into())?; // 3.
                Ok(Value::Type.into())
            },

            //  1.  Γ(x) = τ
            // ────────────────────────────── (INFER/VAR)
            //      Γ ⊢ x :↑ τ
            Term::Var(ref var) => match *var {
                Var::Free(ref name) => Err(TypeError::UnboundVariable(name.clone())),
                Var::Bound(Named(_, index)) => match self.lookup_binder(index) {
                    Some(binder) => match binder.ty() {
                        Some(ty) => Ok(ty.clone()), // 1.
                        None => Err(TypeError::TypeAnnotationsNeeded),
                    },
                    None => panic!("ICE: index {} out of bounds", index),
                },
            },

            //  1.  Γ ⊢ e₁ :↑ (x:τ₁)→τ₂
            //  2.  Γ ⊢ e₂ :↓ τ₁
            //  3.  τ₂[x↦e₂] ⇓ τ₃
            // ────────────────────────────── (INFER/APP)
            //      Γ ⊢ e₁ e₂ :↑ τ₃
            Term::App(ref fn_expr, ref arg_expr) => {
                let fn_type = self.infer(fn_expr)?; // 1.
                match *fn_type.inner {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        self.check(arg_expr, param_ty)?; // 2.
                        let body_ty = ret_ty.open(&self.normalize(&arg_expr)); // 3.
                        Ok(body_ty)
                    },
                    // TODO: More error info
                    _ => Err(TypeError::IllegalApplication),
                }
            },
        }
    }
}
