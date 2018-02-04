//! The semantics of the language

use syntax::core::{Binder, Context, Module, RcTerm, RcType, RcValue, Term, Value};
use syntax::var::{Debruijn, Name, Named, Var};

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

fn lookup(context: &Context, index: Debruijn) -> Result<&Binder, InternalError> {
    context
        .lookup_binder(index)
        .ok_or_else(|| InternalError::DeBruijnIndexOutOfScope {
            index,
            context: context.clone(),
        })
}

/// Typecheck and elaborate a module
pub fn check_module(module: &Module) -> Result<CheckedModule, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(module.definitions.len());

    for definition in &module.definitions {
        let ann = match definition.ann {
            // We don't have a type annotation available to us! Instead we will attempt to infer it
            // based on the body of the definition
            None => infer(&context, &definition.term)?,
            // We have a type annotation! Evaluate it to its normal form, then check that it matches
            // the body of the definition
            Some(ref ann) => {
                let ann = normalize(&context, &ann)?;
                check(&context, &definition.term, &ann)?;
                ann
            },
        };
        // Evaluate the body of the definition
        let term = normalize(&context, &definition.term)?;
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

/// Evaluates a core term to its normal form
///
/// ```text
/// Γ ⊢ e ⇓ v
/// ```
pub fn normalize(context: &Context, term: &RcTerm) -> Result<RcValue, InternalError> {
    match *term.inner {
        //  1.  Γ ⊢ e ⇓ v
        // ─────────────────────── (EVAL/ANN)
        //      Γ ⊢ e:ρ ⇓ v
        Term::Ann(ref expr, _) => {
            normalize(context, expr) // 1.
        },

        // ─────────────────── (EVAL/TYPE)
        //  Γ ⊢ Type ⇓ Type
        Term::Type => Ok(Value::Type.into()),

        Term::Var(ref var) => match *var {
            Var::Free(ref name) => Err(InternalError::NormalizedUnboundVariable(name.clone())),
            Var::Bound(Named(_, index)) => match *lookup(context, index)? {
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
            let body_expr = normalize(&context.extend(binder), body_expr)?; // 1.

            Ok(Value::Lam(Named(name.clone(), None), body_expr).into())
        },

        //  1.  Γ ⊢ ρ ⇓ τ
        //  2.  Γ,λx:τ ⊢ e ⇓ v
        // ──────────────────────────────── (EVAL/LAM-ANN)
        //      Γ ⊢ λx:ρ.e ⇓ λx:τ.v
        Term::Lam(Named(ref name, Some(ref param_ty)), ref body_expr) => {
            let param_ty = normalize(context, param_ty)?; // 1.
            let binder = Binder::Lam(Some(param_ty.clone()));
            let body_expr = normalize(&context.extend(binder), body_expr)?; // 2.

            Ok(Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into())
        },

        //  1.  Γ ⊢ ρ₁ ⇓ τ₁
        //  2.  Γ,Πx:τ ⊢ ρ₂ ⇓ τ₂
        // ─────────────────────────────────── (EVAL/PI-ANN)
        //      Γ ⊢ Πx:ρ₁.ρ₂ ⇓ Πx:τ₁.τ₂
        Term::Pi(Named(ref name, ref param_ty), ref body_expr) => {
            let param_ty = normalize(context, param_ty)?; // 1.
            let binder = Binder::Pi(param_ty.clone());
            let body_expr = normalize(&context.extend(binder), body_expr)?; // 2.

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
            let fn_expr = normalize(context, fn_expr)?; // 1.
            let arg = normalize(context, arg)?; // 2.

            match *fn_expr.inner {
                Value::Lam(_, ref body) => Ok(body.open(&arg)),
                _ => Ok(Value::App(fn_expr.clone(), arg).into()),
            }
        },
    }
}

/// Check that the given term has the expected type
///
/// ```text
/// Γ ⊢ e :↓ τ
/// ```
pub fn check(context: &Context, term: &RcTerm, expected: &RcType) -> Result<(), TypeError> {
    match *term.inner {
        //  1.  Γ,Πx:τ₁ ⊢ e :↓ τ₂
        // ─────────────────────────────── (CHECK/LAM)
        //      Γ ⊢ λx.e :↓ Πx:τ₁.τ₂
        Term::Lam(Named(_, None), ref body_expr) => match *expected.inner {
            Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                let binder = Binder::Pi(param_ty.clone());
                check(&context.extend(binder), body_expr, ret_ty) // 1.
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
            let inferred_ty = infer(context, term)?; // 1.
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
///
/// ```text
/// Γ ⊢ e :↑ τ
/// ```
pub fn infer(context: &Context, term: &RcTerm) -> Result<RcType, TypeError> {
    match *term.inner {
        //  1.  Γ ⊢ ρ₁ :↓ Type
        //  2.  ρ ⇓ τ
        //  3.  Γ ⊢ e :↓ τ
        // ───────────────────────── (INFER/ANN)
        //      Γ ⊢ (e:ρ) :↑ τ
        Term::Ann(ref expr, ref ty) => {
            check(context, ty, &Value::Type.into())?; // 1.
            let simp_ty = normalize(context, &ty)?; // 2.
            check(context, expr, &simp_ty)?; // 3.
            Ok(simp_ty)
        },

        // ─────────────────── (INFER/TYPE)
        //  Γ ⊢ TYPE :↑ Type
        Term::Type => Ok(Value::Type.into()),

        //  1.  x:τ ∈ Γ
        // ──────────────────── (INFER/VAR)
        //      Γ ⊢ x :↑ τ
        Term::Var(ref var) => match *var {
            Var::Free(ref name) => Err(TypeError::UnboundVariable(name.clone())),
            Var::Bound(Named(_, index)) => match lookup(context, index)?.ty() {
                Some(ty) => Ok(ty.clone()), // 1.
                None => Err(TypeError::TypeAnnotationsNeeded),
            },
        },

        Term::Lam(Named(_, None), _) => {
            // TODO: More error info
            Err(TypeError::TypeAnnotationsNeeded)
        },

        //  1.  Γ ⊢ ρ :↓ Type
        //  2.  ρ ⇓ τ₁
        //  3.  Γ,λx:τ₁ ⊢ e :↑ τ₂
        // ─────────────────────────────── (INFER/LAM)
        //      Γ ⊢ λx:ρ.e :↑ Πx:τ₁.τ₂
        Term::Lam(Named(ref param_name, Some(ref param_ty)), ref body_expr) => {
            check(context, param_ty, &Value::Type.into())?; // 1.
            let simp_param_ty = normalize(context, &param_ty)?; // 2.
            let binder = Binder::Lam(Some(simp_param_ty.clone()));
            let body_ty = infer(&context.extend(binder), body_expr)?; // 3.

            Ok(Value::Pi(Named(param_name.clone(), simp_param_ty), body_ty).into())
        },

        //  1.  Γ ⊢ ρ₁ :↓ Type
        //  2.  ρ₁ ⇓ τ₁
        //  3.  Γ,Πx:τ₁ ⊢ ρ₂ :↓ Type
        // ────────────────────────────── (INFER/PI)
        //      Γ ⊢ Πx:ρ₁.ρ₂ :↑ Type
        Term::Pi(Named(_, ref param_ty), ref body_ty) => {
            check(context, param_ty, &Value::Type.into())?; // 1.
            let simp_param_ty = normalize(context, &param_ty)?; // 2.
            let binder = Binder::Pi(simp_param_ty);
            check(&context.extend(binder), body_ty, &Value::Type.into())?; // 3.
            Ok(Value::Type.into())
        },

        //  1.  Γ ⊢ e₁ :↑ Πx:τ₁.τ₂
        //  2.  Γ ⊢ e₂ :↓ τ₁
        //  3.  τ₂[x↦e₂] ⇓ τ₃
        // ────────────────────────────── (INFER/APP)
        //      Γ ⊢ e₁ e₂ :↑ τ₃
        Term::App(ref fn_expr, ref arg_expr) => {
            let fn_type = infer(context, fn_expr)?; // 1.
            match *fn_type.inner {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    check(context, arg_expr, param_ty)?; // 2.
                    let body_ty = ret_ty.open(&normalize(context, &arg_expr)?); // 3.
                    Ok(body_ty)
                },
                // TODO: More error info
                _ => Err(TypeError::IllegalApplication),
            }
        },
    }
}
