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
//! | normalization  | `Γ ⊢ t ⇒ v`     | `Γ`, `t`      | `v`      | `semantics::normalize` |
//! | type checking  | `Γ ⊢ r ↑ V ⤳ t` | `Γ`, `r`, `V` | `t`      | `semantics::check`     |
//! | type inference | `Γ ⊢ r ↓ V ⤳ t` | `Γ`, `r`      | `V`, `t` | `semantics::infer`     |
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
use nameless::{self, BoundTerm, Embed, Name, Var};
use std::rc::Rc;

use syntax::core::{Binder, Context, Definition, Level, Module, Neutral, RawModule, RawTerm, Term,
                   Type, Value};

mod errors;
#[cfg(test)]
mod tests;

pub use self::errors::{InternalError, TypeError};

/// Typecheck and elaborate a module
pub fn check_module(module: &RawModule) -> Result<Module, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(module.definitions.len());

    for definition in &module.definitions {
        let name = definition.name.clone();
        let (term, ann) = match *definition.ann {
            // We don't have a type annotation available to us! Instead we will
            // attempt to infer it based on the body of the definition
            RawTerm::Hole(_) => infer(&context, &definition.term)?,
            // We have a type annotation! Elaborate it, then nomalize it, then
            // check that it matches the body of the definition
            _ => {
                let (ann, _) = infer(&context, &definition.ann)?;
                let ann = normalize(&context, &ann)?;
                let elab_term = check(&context, &definition.term, &ann)?;
                (elab_term, ann)
            },
        };

        // Add the definition to the context
        context = context.extend_let(Name::user(name.clone()), ann.clone(), term.clone());

        definitions.push(Definition { name, term, ann })
    }

    Ok(Module {
        name: module.name.clone(),
        definitions,
    })
}

/// Evaluate a term in a context
pub fn normalize(context: &Context, term: &Rc<Term>) -> Result<Rc<Value>, InternalError> {
    match **term {
        // E-ANN
        Term::Ann(_, ref expr, _) => normalize(context, expr),

        // E-TYPE
        Term::Universe(_, level) => Ok(Rc::new(Value::Universe(level))),

        /// E-CONST
        Term::Constant(_, ref c) => Ok(Rc::new(Value::Constant(c.clone()))),

        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_binder(name) {
                // E-VAR-ANN
                Some(&Binder::Lam { .. }) | Some(&Binder::Pi { .. }) => {
                    Ok(Rc::new(Value::from(Neutral::Var(var.clone()))))
                },

                // E-VAR-DEF)
                Some(&Binder::Let { ref value, .. }) => normalize(context, value),

                None => Err(InternalError::UndefinedName {
                    var_span: term.span(),
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(ref name, index) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: term.span(),
                name: name.clone(),
                index: index,
            }),
        },

        // E-PI
        Term::Pi(_, ref scope) => {
            let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &param_ann)?;
            let body_context = context.extend_pi(name.clone(), ann.clone());
            let body = normalize(&body_context, &body)?;

            Ok(Rc::new(Value::Pi(nameless::bind((name, Embed(ann)), body))))
        },

        // E-LAM
        Term::Lam(_, ref scope) => {
            let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &param_ann)?;
            let param = (name.clone(), Embed(ann.clone()));
            let body = normalize(&context.extend_lam(name, ann), &body)?;

            Ok(Rc::new(Value::Lam(nameless::bind(param, body))))
        },

        // E-APP
        Term::App(_, ref fn_expr, ref arg) => {
            let fn_value = normalize(context, fn_expr)?;

            match *fn_value {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

                    let body_context = context.extend_let(name, param_ann, arg.clone());
                    normalize(&body_context, &Rc::new(Term::from(&*body)))
                },
                Value::Neutral(ref fn_expr) => Ok(Rc::new(Value::from(Neutral::App(
                    fn_expr.clone(),
                    arg.clone(),
                )))),
                _ => Err(InternalError::ArgumentAppliedToNonFunction {
                    span: fn_expr.span(),
                }),
            }
        },
    }
}

/// Type checking of terms
pub fn check(
    context: &Context,
    term: &Rc<RawTerm>,
    expected: &Rc<Type>,
) -> Result<Rc<Term>, TypeError> {
    use syntax::core::{Constant, RawConstant};

    /// ```text
    /// Γ ⊢ r ↑ c ⤳ t
    /// ```
    fn check_const(c: &RawConstant, c_ty: &Constant) -> Option<Constant> {
        match (c, c_ty) {
            // FIXME: overflow?
            (&RawConstant::Int(value), &Constant::U8Type) => Some(Constant::U8(value as u8)),
            (&RawConstant::Int(value), &Constant::U16Type) => Some(Constant::U16(value as u16)),
            (&RawConstant::Int(value), &Constant::U32Type) => Some(Constant::U32(value as u32)),
            (&RawConstant::Int(value), &Constant::U64Type) => Some(Constant::U64(value)),
            (&RawConstant::Int(value), &Constant::I8Type) => Some(Constant::I8(value as i8)),
            (&RawConstant::Int(value), &Constant::I16Type) => Some(Constant::I16(value as i16)),
            (&RawConstant::Int(value), &Constant::I32Type) => Some(Constant::I32(value as i32)),
            (&RawConstant::Int(value), &Constant::I64Type) => Some(Constant::I64(value as i64)),
            (&RawConstant::Int(value), &Constant::F32Type) => Some(Constant::F32(value as f32)),
            (&RawConstant::Int(value), &Constant::F64Type) => Some(Constant::F64(value as f64)),
            (&RawConstant::Float(value), &Constant::F32Type) => Some(Constant::F32(value as f32)),
            (&RawConstant::Float(value), &Constant::F64Type) => Some(Constant::F64(value)),
            (_, _) => None,
        }
    }

    match (&**term, &**expected) {
        // C-LAM
        (&RawTerm::Lam(meta, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (pi_name, Embed(pi_ann)), pi_body) =
                nameless::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let RawTerm::Hole(_) = *lam_ann {
                let body_context = context.extend_pi(pi_name, pi_ann.clone());
                let elab_param = (lam_name, Embed(Rc::new(Term::from(&*pi_ann))));
                let elab_lam_body = check(&body_context, &lam_body, &pi_body)?;

                return Ok(Rc::new(Term::Lam(
                    meta,
                    nameless::bind(elab_param, elab_lam_body),
                )));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and reunbinding at I-LAM
        },
        (&RawTerm::Constant(meta, ref c), &Value::Constant(ref c_ty)) => {
            if let Some(c) = check_const(c, c_ty) {
                return Ok(Rc::new(Term::Constant(meta, c)));
            }
        },
        (&RawTerm::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: term.span(),
                expected: expected.clone(),
            });
        },
        (&RawTerm::Hole(meta), _) => {
            return Err(TypeError::UnableToElaborateHole {
                span: meta.span,
                expected: Some(expected.clone()),
            });
        },
        _ => {},
    }

    // C-CONV
    let (elab_term, inferred_ty) = infer(context, term)?;
    match Type::term_eq(&inferred_ty, expected) {
        true => Ok(elab_term),
        false => Err(TypeError::Mismatch {
            span: term.span(),
            found: inferred_ty,
            expected: expected.clone(),
        }),
    }
}

/// Type inference of terms
pub fn infer(context: &Context, term: &Rc<RawTerm>) -> Result<(Rc<Term>, Rc<Type>), TypeError> {
    use std::cmp;

    use syntax::core::{RawConstant, SourceMeta};

    /// Ensures that the given term is a universe, returning the level of that
    /// universe and its elaborated form.
    fn infer_universe(
        context: &Context,
        term: &Rc<RawTerm>,
    ) -> Result<(Rc<Term>, Level), TypeError> {
        let (elab, ty) = infer(context, term)?;
        match *ty {
            Value::Universe(level) => Ok((elab, level)),
            _ => Err(TypeError::ExpectedUniverse {
                span: term.span(),
                found: ty,
            }),
        }
    }

    fn infer_const(meta: SourceMeta, c: &RawConstant) -> Result<(Rc<Term>, Rc<Type>), TypeError> {
        use syntax::core::{Constant as C, RawConstant as RawC};

        let (term, ty) = match *c {
            RawC::String(ref value) => (C::String(value.clone()), Value::Constant(C::StringType)),
            RawC::Char(value) => (C::Char(value), Value::Constant(C::CharType)),
            RawC::Int(_) => return Err(TypeError::AmbiguousIntLiteral { span: meta.span }),
            RawC::Float(_) => return Err(TypeError::AmbiguousFloatLiteral { span: meta.span }),
            RawC::StringType => (C::StringType, Value::Universe(Level(0))),
            RawC::CharType => (C::CharType, Value::Universe(Level(0))),
            RawC::U8Type => (C::U8Type, Value::Universe(Level(0))),
            RawC::U16Type => (C::U16Type, Value::Universe(Level(0))),
            RawC::U32Type => (C::U32Type, Value::Universe(Level(0))),
            RawC::U64Type => (C::U64Type, Value::Universe(Level(0))),
            RawC::I8Type => (C::I8Type, Value::Universe(Level(0))),
            RawC::I16Type => (C::I16Type, Value::Universe(Level(0))),
            RawC::I32Type => (C::I32Type, Value::Universe(Level(0))),
            RawC::I64Type => (C::I64Type, Value::Universe(Level(0))),
            RawC::F32Type => (C::F32Type, Value::Universe(Level(0))),
            RawC::F64Type => (C::F64Type, Value::Universe(Level(0))),
        };

        Ok((Rc::new(Term::Constant(meta, term)), Rc::new(ty)))
    }

    match **term {
        //  I-ANN
        RawTerm::Ann(meta, ref expr, ref ty) => {
            let (elab_ty, _) = infer_universe(context, ty)?;
            let simp_ty = normalize(context, &elab_ty)?;
            let elab_expr = check(context, expr, &simp_ty)?;

            Ok((Rc::new(Term::Ann(meta, elab_expr, elab_ty)), simp_ty))
        },

        // I-TYPE
        RawTerm::Universe(meta, level) => Ok((
            Rc::new(Term::Universe(meta, level)),
            Rc::new(Value::Universe(level.succ())),
        )),

        RawTerm::Hole(meta) => Err(TypeError::UnableToElaborateHole {
            span: meta.span,
            expected: None,
        }),

        RawTerm::Constant(meta, ref c) => infer_const(meta, c),

        // I-VAR-ANN, I-VAR-DEF
        RawTerm::Var(meta, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_binder(name) {
                Some(&Binder::Lam { ann: ref ty, .. })
                | Some(&Binder::Pi { ann: ref ty, .. })
                | Some(&Binder::Let { ann: ref ty, .. }) => {
                    Ok((Rc::new(Term::Var(meta, var.clone())), ty.clone()))
                },

                None => Err(TypeError::UndefinedName {
                    var_span: term.span(),
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(ref name, index) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: term.span(),
                name: name.clone(),
                index: index,
            }.into()),
        },

        // I-PI
        RawTerm::Pi(meta, ref scope) => {
            let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

            let (elab_ann, level_ann) = infer_universe(context, &param_ann)?;
            let simp_ann = normalize(context, &elab_ann)?;
            let body_context = context.extend_pi(name.clone(), simp_ann);
            let (elab_body, level_body) = infer_universe(&body_context, &body)?;

            let elab_param = (name, Embed(elab_ann));
            let elab_pi = Term::Pi(meta, nameless::bind(elab_param, elab_body));
            let level = cmp::max(level_ann, level_body);

            Ok((Rc::new(elab_pi), Rc::new(Value::Universe(level))))
        },

        // I-LAM
        RawTerm::Lam(meta, ref scope) => {
            let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

            // Check for holes before entering to ensure we get a nice error
            if let RawTerm::Hole(_) = *param_ann {
                return Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: name.clone(),
                });
            }

            let (lam_ann, _) = infer_universe(context, &param_ann)?;
            let pi_ann = normalize(context, &lam_ann)?;
            let body_ctx = context.extend_lam(name.clone(), pi_ann.clone());
            let (lam_body, pi_body) = infer(&body_ctx, &body)?;

            let lam_param = (name.clone(), Embed(lam_ann));
            let pi_param = (name.clone(), Embed(pi_ann));

            Ok((
                Rc::new(Term::Lam(meta, nameless::bind(lam_param, lam_body))),
                Rc::new(Value::Pi(nameless::bind(pi_param, pi_body))),
            ))
        },

        // I-APP
        RawTerm::App(meta, ref fn_expr, ref arg_expr) => {
            let (elab_fn_expr, fn_ty) = infer(context, fn_expr)?;

            match *fn_ty {
                Value::Pi(ref scope) => {
                    let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());

                    let arg_expr = check(context, arg_expr, &param_ann)?;
                    let body = normalize(
                        &context.extend_let(name, param_ann, arg_expr.clone()),
                        &Rc::new(Term::from(&*body)),
                    )?;

                    Ok((Rc::new(Term::App(meta, elab_fn_expr, arg_expr)), body))
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
