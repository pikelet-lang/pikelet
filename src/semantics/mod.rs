//! The semantics of the language
//!
//! We take a bidirectional approach to type checking, splitting it into two
//! phases: type checking and type inference. This makes the flow of information
//! through the type checker clear and relatively easy to reason about.
//!
//! # Judgements
//!
//! The semantics of Pikelet are described by the following judgement forms:
//!
//! | name           | notation         | inputs        | outputs  | implementing function  |
//! |----------------|------------------|---------------|----------|------------------------|
//! | normalization  | `Γ ⊢ e ⇓ v`     | `Γ`, `e`      | `v`      | `semantics::normalize` |
//! | type checking  | `Γ ⊢ e ⇐ τ ⤳ v` | `Γ`, `e`, `τ` | `v`      | `semantics::check`     |
//! | type inference | `Γ ⊢ e ⇒ τ ⤳ v` | `Γ`, `e`      | `τ`, `v` | `semantics::infer`     |
//!
//! The judgements rely on the syntax for terms, values, and contexts that were
//! previously defined in `syntax::core`.  Care has been taken to design the
//! judgments in such a way that they are 'syntax-directed', meaning an
//! algorithm can be clearly derived from them.
//!
//! ## Elaboration
//!
//! Elaboration is the process of filling in missing information that the
//! programmer omitted in the original code, generally based on the results
//! of type inference.
//!
//! In Pikelet's judgement forms the elaborated terms are denoted after the
//! wiggly arrow, ie. `⤳`. At the moment not much is added - only the missing
//! type annotations on function parameters. It's unclear at the moment how
//! bidirectional checking could be extended to support more involved
//! elaboration, for example handling implicit arguments like:
//!
//! ```text
//! id : {a : Type} -> a -> a
//! ```
//!
//! Perhaps we'd have to resort to unification-based inference for that! Not
//! sure how that would work in concert with bidirectional checking, and it
//! would be great to hear any advice if folks have any!
//!
//! ## Error Handling
//!
//! If no judgement matches, we return an appropriate error type. Errors that
//! arise in the `check` and `normalize` functions can be converted into
//! `Diagnostic`s to assist in fixing problems. Errors that arise in the
//! `normalize` function are assumed to be bugs in the implementation,
//! because the type checker should catch these problems before hand.
//!
//! # A note on notation
//!
//! We provide [natural deduction][natural-deduction-wikipedia] judgements to
//! define the semantics of our type system. These can be intimidating to read
//! at first, but are a concise way of describing a logical system. The
//! judgements all follow the same general pattern:
//!
//! ```text
//! 1. premise
//! 2. premise
//!    ...
//! n. premise
//! ────────────────
//!    conclusion
//! ```
//!
//! In contrast to traditional mathematical notation, we number the premises
//! to allow use to mark the relevant parts of the code that determine whether
//! these premises are met.
//!
//! Sometimes you will see judgements that do not have any premises:
//!
//! ```text
//! ────────────────
//!    conclusion
//! ````
//!
//! These judgements can be considered ['axioms'][axiom-wikipedia], ie. they are
//! the base cases of our recursive judgements.
//!
//! [natural-deduction-wikipedia]: https://en.wikipedia.org/wiki/Natural_deduction
//! [axiom-wikipedia]: https://en.wikipedia.org/wiki/Axiom

use codespan::ByteSpan;

use syntax::core::{self, Binder, Context, Level, Module, Name, RcTerm, RcType, RcValue, Term};
use syntax::core::{Value, ValueLam, ValuePi};
use syntax::var::{Named, Var};

#[cfg(test)]
mod tests;
mod errors;

pub use self::errors::{InternalError, TypeError};

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
        Term::Ann(_, ref expr, _) => {
            normalize(context, expr) // 1.
        },

        // ─────────────────── (EVAL/TYPE)
        //  Γ ⊢ Type ⇓ Type
        Term::Universe(_, level) => Ok(Value::Universe(level).into()),

        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_binder(name) {
                // Can't reduce further - we are in a pi or let binding!
                // We'll have to hope that these are substituted away later,
                // either in EVAL/APP or INFER/APP. For now we just forward the
                // variable name onward:
                //
                //  1.  λx:τ ∈ Γ
                // ───────────────────── (EVAL/VAR-LAM)
                //      Γ ⊢ x ⇓ x
                //
                //  2.  Πx:τ ∈ Γ
                // ───────────────────── (EVAL/VAR-PI)
                //      Γ ⊢ x ⇓ x
                Some(&Binder::Lam(_)) | Some(&Binder::Pi(_)) => Ok(Value::Var(var.clone()).into()),
                // We have a value in scope, let's use that!
                //
                //  1.  let x:τ = v ∈ Γ
                // ───────────────────── (EVAL/VAR-LET)
                //      Γ ⊢ x ⇓ v
                Some(&Binder::Let(_, ref value)) => Ok(value.clone()),

                None => Err(InternalError::UndefinedName {
                    var_span: term.span(),
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(ref index) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: term.span(),
                name: index.name.clone(),
                index: index.inner,
            }),
        },

        //  1. Γ,λx ⊢ e ⇓ v
        // ───────────────────────── (EVAL/LAM)
        //     Γ ⊢ λx.e ⇓ λx.v
        //
        //  2.  Γ ⊢ ρ ⇓ τ
        //  3.  Γ,λx:τ ⊢ e ⇓ v
        // ──────────────────────────────── (EVAL/LAM-ANN)
        //      Γ ⊢ λx:ρ.e ⇓ λx:τ.v
        Term::Lam(_, ref lam) => {
            let (param, body) = lam.clone().unbind();

            let ann = match param.inner {
                None => None,
                Some(ann) => Some(normalize(context, &ann)?), // 2.
            };
            let body_context = context.extend(param.name.clone(), Binder::Lam(ann.clone()));
            let body = normalize(&body_context, &body)?; // 1,3.

            Ok(Value::Lam(ValueLam::bind(Named::new(param.name.clone(), ann), body)).into())
        },

        //  1.  Γ ⊢ ρ₁ ⇓ τ₁
        //  2.  Γ,Πx:τ ⊢ ρ₂ ⇓ τ₂
        // ─────────────────────────────────── (EVAL/PI-ANN)
        //      Γ ⊢ Πx:ρ₁.ρ₂ ⇓ Πx:τ₁.τ₂
        Term::Pi(_, ref pi) => {
            let (param, body) = pi.clone().unbind();

            let ann = normalize(context, &param.inner)?; // 1.
            let body_context = context.extend(param.name.clone(), Binder::Pi(ann.clone()));
            let body = normalize(&body_context, &body)?; // 2.

            Ok(Value::Pi(ValuePi::bind(Named::new(param.name.clone(), ann), body)).into())
        },

        // Perform [β-reduction](https://en.wikipedia.org/wiki/Lambda_calculus#β-reduction),
        // ie. apply functions to their arguments
        //
        //  1.  Γ ⊢ e₁ ⇓ λx.v₁
        //  2.  Γ ⊢ e₂ ⇓ v₂
        // ───────────────────────────── (EVAL/APP)
        //      Γ ⊢ e₁ e₂ ⇓ v₂[x↦v₂]
        Term::App(_, ref fn_expr, ref arg) => {
            let fn_expr = normalize(context, fn_expr)?; // 1.
            let arg = normalize(context, arg)?; // 2.

            match *fn_expr.inner {
                Value::Lam(ref lam) => {
                    // FIXME: do a local unbind here
                    let (param, mut body) = lam.clone().unbind();
                    body.subst(&param.name, &arg);
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
        (&Term::Lam(_, ref lam), &Value::Pi(ref pi)) => {
            let (lam_param, lam_body, pi_param, pi_body) = core::unbind2(lam.clone(), pi.clone());

            if lam_param.inner.is_none() {
                let body_context =
                    context.extend(pi_param.name, Binder::Pi(pi_param.inner.clone()));
                let elab_lam_body = check(&body_context, &lam_body, &pi_body)?; // 1.

                let elab_term = Value::Lam(ValueLam::bind(
                    Named::new(lam_param.name, Some(pi_param.inner)),
                    elab_lam_body,
                )).into();

                return Ok(elab_term);
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and reunbinding at INFER/LAM
        },
        (&Term::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: term.span(),
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
            span: term.span(),
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
    use std::cmp;

    /// Ensures that the given term is a universe, returning the level of that
    /// universe and its elaborated form.
    ///
    /// ```text
    /// Γ ⊢ ρ ⇒ Typeᵢ ⤳ τ
    /// ```
    fn infer_universe(context: &Context, term: &RcTerm) -> Result<(RcValue, Level), TypeError> {
        let (elab, ty) = infer(context, term)?;
        match *ty.inner {
            Value::Universe(level) => Ok((elab, level)),
            _ => Err(TypeError::ExpectedUniverse {
                span: term.span(),
                found: ty,
            }),
        }
    }

    match *term.inner {
        //  1.  Γ ⊢ ρ ⇒ Typeᵢ ⤳ τ
        //  2.  ρ ⇓ τ
        //  3.  Γ ⊢ e ⇐ τ ⤳ v
        // ───────────────────────────── (INFER/ANN)
        //      Γ ⊢ (e:ρ) ⇒ τ ⤳ v
        Term::Ann(_, ref expr, ref ty) => {
            infer_universe(context, ty)?; // 1.
            let simp_ty = normalize(context, &ty)?; // 2.
            let elab_expr = check(context, expr, &simp_ty)?; // 3.
            Ok((elab_expr, simp_ty))
        },

        // ───────────────────────────────── (INFER/TYPE)
        //  Γ ⊢ Typeᵢ ⇒ Typeᵢ₊₁ ⤳ Typeᵢ
        Term::Universe(_, level) => Ok((
            Value::Universe(level).into(),
            Value::Universe(level.succ()).into(),
        )),

        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_binder(name) {
                //  1.  λx:τ ∈ Γ
                // ─────────────────────── (INFER/VAR-LAM)
                //      Γ ⊢ x ⇒ τ ⤳ x
                //
                //  2.  Πx:τ ∈ Γ
                // ─────────────────────── (INFER/VAR-PI)
                //      Γ ⊢ x ⇒ τ ⤳ x
                Some(&Binder::Lam(Some(ref ty))) | Some(&Binder::Pi(ref ty)) => {
                    Ok((Value::Var(var.clone()).into(), ty.clone()))
                },
                //  1.  let x:τ = v ∈ Γ
                // ─────────────────────── (INFER/VAR-LET)
                //      Γ ⊢ x ⇒ τ ⤳ v
                Some(&Binder::Let(ref ty, ref value)) => Ok((ty.clone(), value.clone())),

                Some(&Binder::Lam(None)) => Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: binder.span(),
                    var_span: Some(term.span()),
                    name: name.clone(),
                }),
                None => Err(TypeError::UndefinedName {
                    var_span: term.span(),
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(ref index) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: term.span(),
                name: index.name.clone(),
                index: index.inner,
            }.into()),
        },

        //  1.  Γ ⊢ ρ ⇒ Typeᵢ ⤳ τ
        //  2.  ρ ⇓ τ₁
        //  3.  Γ,λx:τ₁ ⊢ e ⇒ τ₂ ⤳ v
        // ───────────────────────────────────────── (INFER/LAM)
        //      Γ ⊢ λx:ρ.e ⇒ Πx:τ₁.τ₂ ⤳ λx:τ.v
        Term::Lam(_, ref lam) => {
            let (param, body) = lam.clone().unbind();

            match param.inner {
                Some(ann) => {
                    let (elab_ann, _) = infer_universe(context, &ann)?; // 1.
                    let simp_ann = normalize(context, &ann)?; // 2.
                    let binder = Binder::Lam(Some(simp_ann.clone()));
                    let body_context = context.extend(param.name.clone(), binder);
                    let (elab_body, body_ty) = infer(&body_context, &body)?; // 3.

                    let elab_param = Named::new(param.name.clone(), Some(elab_ann));
                    let elab_lam = ValueLam::bind(elab_param, elab_body);
                    let pi_ty = ValuePi::bind(Named::new(param.name.clone(), simp_ann), body_ty);

                    Ok((Value::Lam(elab_lam).into(), Value::Pi(pi_ty).into()))
                },
                None => Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: param.name.clone(),
                }),
            }
        },

        //  1.  Γ ⊢ ρ₁ ⇒ Typeᵢ ⤳ τ₁
        //  2.  ρ₁ ⇓ τ₁'
        //  3.  Γ,Πx:τ₁' ⊢ ρ₂ ⇐ Typeⱼ ⤳ τ₂
        //  4.  k = max(i, j)
        // ────────────────────────────────────────── (INFER/PI)
        //      Γ ⊢ Πx:ρ₁.ρ₂ ⇒ Typeₖ ⤳ Πx:τ₁.τ₂
        Term::Pi(_, ref pi) => {
            let (param, body) = pi.clone().unbind();

            let (elab_ann, level_ann) = infer_universe(context, &param.inner)?; // 1.
            let simp_ann = normalize(context, &param.inner)?; // 2.
            let body_context = context.extend(param.name.clone(), Binder::Pi(simp_ann));
            let (elab_body, level_body) = infer_universe(&body_context, &body)?; // 3.

            let elab_param = Named::new(param.name.clone(), elab_ann);
            let elab_pi = ValuePi::bind(elab_param, elab_body);
            let level = cmp::max(level_ann, level_body); // 4.

            Ok((Value::Pi(elab_pi).into(), Value::Universe(level).into()))
        },

        //  1.  Γ ⊢ e₁ ⇒ Πx:τ₁.τ₂ ⤳ v₁
        //  2.  Γ ⊢ e₂ ⇐ τ₁ ⤳ v₂
        //  3.  τ₂ ⇓ τ₃
        // ────────────────────────────────────── (INFER/APP)
        //      Γ ⊢ e₁ e₂ ⇒ τ₃[x↦e₂] ⤳ v₁ v₂
        Term::App(_, ref fn_expr, ref arg_expr) => {
            let (elab_fn_expr, fn_ty) = infer(context, fn_expr)?; // 1.

            match *fn_ty.inner {
                Value::Pi(ref pi) => {
                    let (pi_param, mut pi_body) = pi.clone().unbind();

                    let elab_arg_expr = check(context, arg_expr, &pi_param.inner)?; // 2.
                    let simp_arg_expr = normalize(context, &arg_expr)?; // 3.
                    pi_body.subst(&pi_param.name, &simp_arg_expr);

                    Ok((Value::App(elab_fn_expr, elab_arg_expr).into(), pi_body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: fn_expr.span(),
                    arg_span: arg_expr.span(),
                    found: fn_ty.clone(),
                }),
            }
        },
    }
}
