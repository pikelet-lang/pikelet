//! The semantics of the language
//!
//! The key judgements we define in this module are:
//!
//! - normalization: `Γ ⊢ e ⇓ v`
//! - type checking: `Γ ⊢ e ⇐ τ ⤳ v`
//! - type inference: `Γ ⊢ e ⇒ τ ⤳ v`
//!
//! We take a bidirectional approach to type checking, splitting it into two
//! phases: type checking and type inference. This makes the flow of information
//! through the type checker clear and relatively easy to reason about.

use syntax::core::{self, Binder, Context, Module, Name, RcTerm, RcType, RcValue, Term};
use syntax::core::{Value, ValueLam, ValuePi};
use syntax::var::{Debruijn, Named, Var};

#[cfg(test)]
mod tests;

/// A typechecked and elaborated module
pub struct CheckedModule {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<CheckedDefinition>,
}

/// A typechecked and elaborated definition
pub struct CheckedDefinition {
    /// The name of the definition
    pub name: String,
    /// The elaborated value
    pub term: RcValue,
    /// The type of the definition
    pub ann: RcType,
}

/// Typecheck and elaborate a module
pub fn check_module(module: &Module) -> Result<CheckedModule, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(module.definitions.len());

    for definition in &module.definitions {
        let name = definition.name.clone();
        let (term, ann) = match definition.ann {
            // We don't have a type annotation available to us! Instead we will
            // attempt to infer it based on the body of the definition
            None => infer(&context, &definition.term)?,
            // We have a type annotation! Evaluate it to its normal form, then
            // check that it matches the body of the definition
            Some(ref ann) => {
                let ann = normalize(&context, &ann)?;
                let elab_term = check(&context, &definition.term, &ann)?;
                (elab_term, ann)
            },
        };

        // Add the definition to the context
        context = context.extend(
            Name::user(name.clone()),
            Binder::Let(term.clone(), ann.clone()),
        );

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
    UnsubstitutedDebruijnIndex(Named<Name, Debruijn>),
    UndefinedName(Name),
}

/// An error produced during typechecking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    NotAFunctionType {
        expr: RcTerm,
        found: RcType,
    },
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
    UndefinedName(Name),
    Internal(InternalError),
}

impl From<InternalError> for TypeError {
    fn from(src: InternalError) -> TypeError {
        TypeError::Internal(src)
    }
}

/// Evaluate a term in a context
///
/// Normalizes (evaluates) a core term to its normal form under the assumptions
/// in the context.
///
/// ```text
/// Γ ⊢ e ⇓ v
/// ```
///
/// Here we diverge from the LambdaPi paper by requiring a context to be
/// supplied. This allows us to resolve previously defined terms during
/// normalization.
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
        Term::Universe => Ok(RcValue::universe()),

        Term::Var(ref var) => match *var {
            Var::Bound(ref index) => Err(InternalError::UnsubstitutedDebruijnIndex(index.clone())),
            Var::Free(ref name) => match *context
                .lookup_binder(name)
                .ok_or_else(|| InternalError::UndefinedName(name.clone()))?
            {
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
        //
        //  2.  Γ ⊢ ρ ⇓ τ
        //  3.  Γ,λx:τ ⊢ e ⇓ v
        // ──────────────────────────────── (EVAL/LAM-ANN)
        //      Γ ⊢ λx:ρ.e ⇓ λx:τ.v
        Term::Lam(ref lam) => {
            let (Named(name, ann), body) = lam.clone().unbind();

            let ann = match ann {
                None => None,
                Some(ann) => Some(normalize(context, &ann)?), // 2.
            };
            let body_context = context.extend(name.clone(), Binder::Lam(ann.clone()));
            let body = normalize(&body_context, &body)?; // 1,3.

            Ok(Value::Lam(ValueLam::bind(Named(name.clone(), ann), body)).into())
        },

        //  1.  Γ ⊢ ρ₁ ⇓ τ₁
        //  2.  Γ,Πx:τ ⊢ ρ₂ ⇓ τ₂
        // ─────────────────────────────────── (EVAL/PI-ANN)
        //      Γ ⊢ Πx:ρ₁.ρ₂ ⇓ Πx:τ₁.τ₂
        Term::Pi(ref pi) => {
            let (Named(name, ann), body) = pi.clone().unbind();

            let ann = normalize(context, &ann)?; // 1.
            let body_context = context.extend(name.clone(), Binder::Pi(ann.clone()));
            let body = normalize(&body_context, &body)?; // 2.

            Ok(Value::Pi(ValuePi::bind(Named(name.clone(), ann), body)).into())
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
                Value::Lam(ref lam) => {
                    // FIXME: do a local unbind here
                    let (Named(name, _), mut body) = lam.clone().unbind();
                    body.subst(&name, &arg);
                    Ok(body)
                },
                _ => Ok(Value::App(fn_expr.clone(), arg).into()),
            }
        },
    }
}

/// Type checking of terms
///
/// Under the assumptions in the context, check that the given term has
/// the expected type and return its elaborated form.
///
/// ```text
/// Γ ⊢ e ⇐ τ ⤳ v
/// ```
pub fn check(context: &Context, term: &RcTerm, expected: &RcType) -> Result<RcValue, TypeError> {
    match (&*term.inner, &*expected.inner) {
        // We infer the type of the argument (`τ₁`) of the lambda from the
        // supplied pi type, then 'push' it into the elaborated term, along with
        // the elaborated body (`v`).
        //
        //  1.  Γ,Πx:τ₁ ⊢ e ⇐ τ₂ ⤳ v
        // ────────────────────────────────────── (CHECK/LAM)
        //      Γ ⊢ λx.e ⇐ Πx:τ₁.τ₂ ⤳ λx:τ₁.v
        (&Term::Lam(ref lam), &Value::Pi(ref pi)) => match core::unbind2(lam.clone(), pi.clone()) {
            (Named(lam_name, None), lam_body, Named(pi_name, pi_ann), pi_body) => {
                let body_context = context.extend(pi_name, Binder::Pi(pi_ann.clone()));
                let elab_lam_body = check(&body_context, &lam_body, &pi_body)?; // 1.

                let elab_term =
                    Value::Lam(ValueLam::bind(Named(lam_name, Some(pi_ann)), elab_lam_body)).into();

                return Ok(elab_term);
            },
            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and reunbinding at INFER/LAM
            // (Named(lam_name, Some(lam_ann)), lam_body, Named(pi_name, pi_ann), pi_body) => {},
            _ => {},
        },
        (&Term::Lam(_), _) => {
            return Err(TypeError::ExpectedFunction {
                lam_expr: term.clone(),
                expected: expected.clone(),
            });
        },
        _ => {},
    }

    // Flip the direction of the type checker, comparing the type of the
    // expected term for [alpha equivalence] with the inferred term.
    //
    //  1.  Γ ⊢ e₂ ⇒ τ ⤳ v
    //  2.  e₁ ≡ e₂
    // ─────────────────────── (CHECK/INFER)
    //      Γ ⊢ e₁ ⇐ τ ⤳ v
    //
    // NOTE: We could change 2. to check for subtyping instead of alpha
    // equivalence. This could be useful for implementing a cumulative
    // universe hierarchy.
    //
    // [alpha equivalence]: https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence

    let (elab_term, inferred_ty) = infer(context, term)?; // 1.

    // Because we have invested lots of effort into setting up our
    // locally nameless representation alpha equivalence is easy-peasy!
    match &inferred_ty == expected {
        true => Ok(elab_term),
        false => Err(TypeError::Mismatch {
            expr: term.clone(),
            found: inferred_ty,
            expected: expected.clone(),
        }),
    }
}

/// Type inference of terms
///
/// Under the assumptions in the context, synthesize a type for the given term
/// and return its elaborated form.
///
/// ```text
/// Γ ⊢ e ⇒ τ ⤳ v
/// ```
pub fn infer(context: &Context, term: &RcTerm) -> Result<(RcValue, RcType), TypeError> {
    match *term.inner {
        //  1.  Γ ⊢ ρ ⇐ Type ⤳ τ
        //  2.  ρ ⇓ τ
        //  3.  Γ ⊢ e ⇐ τ ⤳ v
        // ───────────────────────────── (INFER/ANN)
        //      Γ ⊢ (e:ρ) ⇒ τ ⤳ v
        Term::Ann(ref expr, ref ty) => {
            check(context, ty, &RcValue::universe())?; // 1.
            let simp_ty = normalize(context, &ty)?; // 2.
            let elab_expr = check(context, expr, &simp_ty)?; // 3.
            Ok((elab_expr, simp_ty))
        },

        // FIXME: This axiom will lead to [Girard's paradox], meaning that we
        // would be abe to construct a [program that loops forever] We should
        // implement a universe hierarchy to resolve this, where `Type` is
        // indexed by a level `i`.
        //
        // ─────────────────────────── (INFER/TYPE)
        //  Γ ⊢ Type ⇒ Type ⤳ Type
        //
        // [Girard's paradox]: https://ncatlab.org/nlab/show/Burali-Forti%27s+paradox#GirardParadox
        // [program that loops forever]: https://ncatlab.org/nlab/show/looping+combinator
        Term::Universe => Ok((RcValue::universe(), RcValue::universe())),

        Term::Var(ref var) => match *var {
            Var::Bound(ref index) => {
                Err(InternalError::UnsubstitutedDebruijnIndex(index.clone()).into())
            },
            Var::Free(ref name) => match *context
                .lookup_binder(name)
                .ok_or_else(|| TypeError::UndefinedName(name.clone()))?
            {
                //  1.  λx:τ ∈ Γ
                // ─────────────────────── (INFER/VAR-LAM)
                //      Γ ⊢ x ⇒ τ ⤳ x
                //
                //  2.  Πx:τ ∈ Γ
                // ─────────────────────── (INFER/VAR-PI)
                //      Γ ⊢ x ⇒ τ ⤳ x
                Binder::Lam(Some(ref ty)) | Binder::Pi(ref ty) => {
                    Ok((Value::Var(var.clone()).into(), ty.clone()))
                },
                Binder::Lam(None) => Err(TypeError::TypeAnnotationsNeeded),
                //  1.  let x:τ = v ∈ Γ
                // ─────────────────────── (INFER/VAR-LET)
                //      Γ ⊢ x ⇒ τ ⤳ v
                Binder::Let(ref ty, ref value) => Ok((ty.clone(), value.clone())),
            },
        },

        //  1.  Γ ⊢ ρ ⇐ Type ⤳ τ
        //  2.  ρ ⇓ τ₁
        //  3.  Γ,λx:τ₁ ⊢ e ⇒ τ₂ ⤳ v
        // ───────────────────────────────────────── (INFER/LAM)
        //      Γ ⊢ λx:ρ.e ⇒ Πx:τ₁.τ₂ ⤳ λx:τ.v
        Term::Lam(ref lam) => {
            let (Named(name, ann), body) = lam.clone().unbind();

            match ann {
                // TODO: More error info
                None => Err(TypeError::TypeAnnotationsNeeded),
                Some(ann) => {
                    let elab_ann = check(context, &ann, &RcValue::universe())?; // 1.
                    let simp_ann = normalize(context, &ann)?; // 2.
                    let binder = Binder::Lam(Some(simp_ann.clone()));
                    let body_context = context.extend(name.clone(), binder);
                    let (elab_body, body_ty) = infer(&body_context, &body)?; // 3.

                    let elab_lam = ValueLam::bind(Named(name.clone(), Some(elab_ann)), elab_body);
                    let pi_ty = ValuePi::bind(Named(name.clone(), simp_ann), body_ty);
                    Ok((Value::Lam(elab_lam).into(), Value::Pi(pi_ty).into()))
                },
            }
        },

        //  1.  Γ ⊢ ρ₁ ⇐ Type ⤳ τ₁
        //  2.  ρ₁ ⇓ τ₁'
        //  3.  Γ,Πx:τ₁' ⊢ ρ₂ ⇐ Type ⤳ τ₂
        // ────────────────────────────────────────── (INFER/PI)
        //      Γ ⊢ Πx:ρ₁.ρ₂ ⇒ Type ⤳ Πx:τ₁.τ₂
        Term::Pi(ref pi) => {
            let (Named(name, ann), body) = pi.clone().unbind();

            let elab_ann = check(context, &ann, &RcValue::universe())?; // 1.
            let simp_ann = normalize(context, &ann)?; // 2.
            let body_context = context.extend(name.clone(), Binder::Pi(simp_ann));
            let elab_body = check(&body_context, &body, &RcValue::universe())?; // 3.

            let elab_pi = ValuePi::bind(Named(name.clone(), elab_ann), elab_body);
            Ok((Value::Pi(elab_pi).into(), RcValue::universe()))
        },

        //  1.  Γ ⊢ e₁ ⇒ Πx:τ₁.τ₂ ⤳ v₁
        //  2.  Γ ⊢ e₂ ⇐ τ₁ ⤳ v₂
        //  3.  τ₂[x↦e₂] ⇓ τ₃
        // ───────────────────────────────── (INFER/APP)
        //      Γ ⊢ e₁ e₂ ⇒ τ₃ ⤳ v₁ v₂
        Term::App(ref fn_expr, ref arg_expr) => {
            let (elab_fn_expr, fn_type) = infer(context, fn_expr)?; // 1.

            match *fn_type.inner {
                Value::Pi(ref pi) => {
                    let (Named(name, pi_ann), mut pi_body) = pi.clone().unbind();

                    let elab_arg_expr = check(context, arg_expr, &pi_ann)?; // 2.
                    let simp_arg_expr = normalize(context, &arg_expr)?; // 3.
                    pi_body.subst(&name, &simp_arg_expr);

                    Ok((Value::App(elab_fn_expr, elab_arg_expr).into(), pi_body))
                },
                _ => Err(TypeError::NotAFunctionType {
                    expr: fn_expr.clone(),
                    found: fn_type.clone(),
                }),
            }
        },
    }
}
