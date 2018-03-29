//! The semantics of the language

use codespan::ByteSpan;
use nameless::{self, BoundTerm, Embed, Name, Var};
use std::rc::Rc;

use syntax::core::{Binder, Constant, Context, Definition, Level, Module, Neutral, RawConstant,
                   RawModule, RawTerm, Term, Type, Value};

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

        // E-CONST
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
    /// Coerce a raw constant
    fn check_const(
        span: ByteSpan,
        c: &RawConstant,
        c_ty: &Constant,
    ) -> Result<Option<Constant>, TypeError> {
        match (c, c_ty) {
            // FIXME: overflow?
            (&RawConstant::Int(val), &Constant::U8Type) => Ok(Some(Constant::U8(val as u8))),
            (&RawConstant::Int(val), &Constant::U16Type) => Ok(Some(Constant::U16(val as u16))),
            (&RawConstant::Int(val), &Constant::U32Type) => Ok(Some(Constant::U32(val as u32))),
            (&RawConstant::Int(val), &Constant::U64Type) => Ok(Some(Constant::U64(val))),
            (&RawConstant::Int(val), &Constant::I8Type) => Ok(Some(Constant::I8(val as i8))),
            (&RawConstant::Int(val), &Constant::I16Type) => Ok(Some(Constant::I16(val as i16))),
            (&RawConstant::Int(val), &Constant::I32Type) => Ok(Some(Constant::I32(val as i32))),
            (&RawConstant::Int(val), &Constant::I64Type) => Ok(Some(Constant::I64(val as i64))),
            (&RawConstant::Int(val), &Constant::F32Type) => Ok(Some(Constant::F32(val as f32))),
            (&RawConstant::Int(val), &Constant::F64Type) => Ok(Some(Constant::F64(val as f64))),
            (&RawConstant::Int(_), _) => Err(TypeError::NumericLiteralMismatch {
                literal_span: span,
                expected: c_ty.clone(),
            }),
            (&RawConstant::Float(val), &Constant::F32Type) => Ok(Some(Constant::F32(val as f32))),
            (&RawConstant::Float(val), &Constant::F64Type) => Ok(Some(Constant::F64(val))),
            (&RawConstant::Float(_), _) => Err(TypeError::FloatLiteralMismatch {
                literal_span: span,
                expected: c_ty.clone(),
            }),
            (_, _) => Ok(None),
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
            if let Some(c) = check_const(meta.span, c, c_ty)? {
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

        RawTerm::Constant(meta, ref c) => match *c {
            RawConstant::String(ref value) => Ok((
                Rc::new(Term::Constant(meta, Constant::String(value.clone()))),
                Rc::new(Value::Constant(Constant::StringType)),
            )),
            RawConstant::Char(value) => Ok((
                Rc::new(Term::Constant(meta, Constant::Char(value))),
                Rc::new(Value::Constant(Constant::CharType)),
            )),
            RawConstant::Int(_) => Err(TypeError::AmbiguousIntLiteral { span: meta.span }),
            RawConstant::Float(_) => Err(TypeError::AmbiguousFloatLiteral { span: meta.span }),
        },

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
