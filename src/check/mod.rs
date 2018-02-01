//! Contexts and type checking

use rpds::List;
use std::fmt;

use core::{Module, RcTerm, RcType, RcValue, Term, Value};
use pretty::{self, ToDoc};
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
                let ann = context.normalize(&ann)?;
                context.check(&definition.term, &ann)?;
                ann
            },
        };
        // Evaluate the body of the definition
        let term = context.normalize(&definition.term)?;
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

/// An internal error. These are bugs!
#[derive(Debug, Clone, PartialEq)]
pub enum InternalError {
    NormalizedUnboundVariable(Name),
    DeBruijnIndexOutOfScope { index: Debruijn, context: Context },
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
    Internal(InternalError),
}

impl From<InternalError> for TypeError {
    fn from(src: InternalError) -> TypeError {
        TypeError::Internal(src)
    }
}

/// A binder that introduces a variable into the context
//
// b ::= λx:τ           1. lambda abstraction
//     | Πx:τ           2. dependent function
//     | let x:τ = v    3. let binding
//
#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam(Option<RcType>), // 1.
    /// A type introduced after entering a pi type
    Pi(RcType), // 2.
    /// A value and type binding that was introduced by passing over a let binding
    Let(RcValue, RcType), // 3.
}

impl Binder {
    /// Return the type associated with a binder
    pub fn ty(&self) -> Option<&RcType> {
        match *self {
            Binder::Lam(ref ty) => ty.as_ref(),
            Binder::Pi(ref ty) | Binder::Let(_, ref ty) => Some(ty),
        }
    }
}

impl fmt::Display for Binder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// A list of binders that have been accumulated during typechecking
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    // Γ ::= ε           1. empty context
    //     | Γ,b         2. context extension
    pub binders: List<Binder>,
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
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
    pub fn lookup_binder(&self, index: Debruijn) -> Result<&Binder, InternalError> {
        self.binders.iter().nth(index.0 as usize).ok_or_else(|| {
            InternalError::DeBruijnIndexOutOfScope {
                index,
                context: self.clone(),
            }
        })
    }

    /// Evaluates a core term to its normal form
    //
    // Γ ⊢ e ⇓ v
    //
    pub fn normalize(&self, term: &RcTerm) -> Result<RcValue, InternalError> {
        match *term.inner {
            //  1.  Γ ⊢ e ⇓ v
            // ─────────────────────── (EVAL/ANN)
            //      Γ ⊢ e:ρ ⇓ v
            Term::Ann(ref expr, _) => {
                self.normalize(expr) // 1.
            },

            // ─────────────────── (EVAL/TYPE)
            //  Γ ⊢ Type ⇓ Type
            Term::Type => Ok(Value::Type.into()),

            Term::Var(ref var) => match *var {
                Var::Free(ref name) => Err(InternalError::NormalizedUnboundVariable(name.clone())),
                Var::Bound(Named(_, index)) => match *self.lookup_binder(index)? {
                    // Can't reduce further - we are in a pi or let binding
                    //
                    //  1.  λx:τ ∈ Γ
                    // ───────────────────── (EVAL/VAR-LAM)
                    //      Γ ⊢ x ⇓ x
                    //
                    //  2.  Πx:τ ∈ Γ
                    // ───────────────────── (EVAL/VAR-PI)
                    //      Γ ⊢ x ⇓ x
                    Binder::Lam(_) | Binder::Pi(_) => Ok(Value::Var(var.clone()).into()),
                    // We have a value in scope, let's use that!
                    //
                    //  1.  let x:τ = v ∈ Γ
                    // ───────────────────── (EVAL/VAR-LET)
                    //      Γ ⊢ x ⇓ v
                    Binder::Let(_, ref value) => Ok(value.clone()),
                },
            },

            //  1. Γ,λx ⊢ e ⇓ v
            // ───────────────────────── (EVAL/LAM)
            //     Γ ⊢ λx.e ⇓ λx.v
            Term::Lam(Named(ref name, None), ref body_expr) => {
                let binder = Binder::Lam(None);
                let body_expr = self.extend(binder).normalize(body_expr)?; // 1.

                Ok(Value::Lam(Named(name.clone(), None), body_expr).into())
            },

            //  1.  Γ ⊢ ρ ⇓ τ
            //  2.  Γ,λx:τ ⊢ e ⇓ v
            // ──────────────────────────────── (EVAL/LAM-ANN)
            //      Γ ⊢ λx:ρ.e ⇓ λx:τ.v
            Term::Lam(Named(ref name, Some(ref param_ty)), ref body_expr) => {
                let param_ty = self.normalize(param_ty)?; // 1.
                let binder = Binder::Lam(Some(param_ty.clone()));
                let body_expr = self.extend(binder).normalize(body_expr)?; // 2.

                Ok(Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into())
            },

            //  1.  Γ ⊢ ρ₁ ⇓ τ₁
            //  2.  Γ,Πx:τ ⊢ ρ₂ ⇓ τ₂
            // ─────────────────────────────────── (EVAL/PI-ANN)
            //      Γ ⊢ Πx:ρ₁.ρ₂ ⇓ Πx:τ₁.τ₂
            Term::Pi(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = self.normalize(param_ty)?; // 1.
                let binder = Binder::Pi(param_ty.clone());
                let body_expr = self.extend(binder).normalize(body_expr)?; // 2.

                Ok(Value::Pi(Named(name.clone(), param_ty), body_expr).into())
            },

            // Perform [β-reduction](https://en.wikipedia.org/wiki/Lambda_calculus#β-reduction),
            // ie. apply functions to their arguments
            //
            //  1.  Γ ⊢ e₁ ⇓ λx.v₁
            //  2.  Γ ⊢ v₁[x↦e₂] ⇓ v₂
            // ───────────────────────────── (EVAL/APP)
            //      Γ ⊢ e₁ e₂ ⇓ v₂
            Term::App(ref fn_expr, ref arg) => {
                let fn_expr = self.normalize(fn_expr)?; // 1.
                let arg = self.normalize(arg)?; // 2.

                match *fn_expr.inner {
                    Value::Lam(_, ref body) => Ok(body.open(&arg)),
                    _ => Ok(Value::App(fn_expr.clone(), arg).into()),
                }
            },
        }
    }

    /// Check that the given term has the expected type
    //
    // Γ ⊢ e :↓ τ
    //
    pub fn check(&self, term: &RcTerm, expected: &RcType) -> Result<(), TypeError> {
        match *term.inner {
            //  1.  Γ,Πx:τ₁ ⊢ e :↓ τ₂
            // ─────────────────────────────── (CHECK/LAM)
            //      Γ ⊢ λx.e :↓ Πx:τ₁.τ₂
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
    //
    // Γ ⊢ e :↑ τ
    //
    pub fn infer(&self, term: &RcTerm) -> Result<RcType, TypeError> {
        match *term.inner {
            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ ⇓ τ
            //  3.  Γ ⊢ e :↓ τ
            // ───────────────────────── (INFER/ANN)
            //      Γ ⊢ (e:ρ) :↑ τ
            Term::Ann(ref expr, ref ty) => {
                self.check(ty, &Value::Type.into())?; // 1.
                let simp_ty = self.normalize(&ty)?; // 2.
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
            //  3.  Γ,Πx:τ₁ ⊢ e :↑ τ₂
            // ─────────────────────────────── (INFER/LAM)
            //      Γ ⊢ λx:ρ.e :↑ Πx:τ₁.τ₂
            Term::Lam(Named(ref param_name, Some(ref param_ty)), ref body_expr) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.normalize(&param_ty)?; // 2.
                let binder = Binder::Pi(simp_param_ty.clone());
                let body_ty = self.extend(binder).infer(body_expr)?; // 3.

                Ok(Value::Pi(Named(param_name.clone(), simp_param_ty), body_ty).into())
            },

            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ₁ ⇓ τ₁
            //  3.  Γ,Πx:τ₁ ⊢ ρ₂ :↓ Type
            // ────────────────────────────── (INFER/PI)
            //      Γ ⊢ Πx:ρ₁.ρ₂ :↑ Type
            Term::Pi(Named(_, ref param_ty), ref body_ty) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.normalize(&param_ty)?; // 2.
                let binder = Binder::Pi(simp_param_ty);
                self.extend(binder).check(body_ty, &Value::Type.into())?; // 3.
                Ok(Value::Type.into())
            },

            //  1.  x:τ ∈ Γ
            // ──────────────────── (INFER/VAR)
            //      Γ ⊢ x :↑ τ
            Term::Var(ref var) => match *var {
                Var::Free(ref name) => Err(TypeError::UnboundVariable(name.clone())),
                Var::Bound(Named(_, index)) => match self.lookup_binder(index)?.ty() {
                    Some(ty) => Ok(ty.clone()), // 1.
                    None => Err(TypeError::TypeAnnotationsNeeded),
                },
            },

            //  1.  Γ ⊢ e₁ :↑ Πx:τ₁.τ₂
            //  2.  Γ ⊢ e₂ :↓ τ₁
            //  3.  τ₂[x↦e₂] ⇓ τ₃
            // ────────────────────────────── (INFER/APP)
            //      Γ ⊢ e₁ e₂ :↑ τ₃
            Term::App(ref fn_expr, ref arg_expr) => {
                let fn_type = self.infer(fn_expr)?; // 1.
                match *fn_type.inner {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        self.check(arg_expr, param_ty)?; // 2.
                        let body_ty = ret_ty.open(&self.normalize(&arg_expr)?); // 3.
                        Ok(body_ty)
                    },
                    // TODO: More error info
                    _ => Err(TypeError::IllegalApplication),
                }
            },
        }
    }
}
