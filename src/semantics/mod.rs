//! The semantics of the language

use codespan::ByteSpan;
use nameless::{self, BoundTerm, Embed, Name, Var};
use std::rc::Rc;

use syntax::core::{Constant, Context, Definition, Level, Module, Neutral, RawConstant, RawModule,
                   RawTerm, Term, Type, Value};

mod errors;
#[cfg(test)]
mod tests;

pub use self::errors::{InternalError, TypeError};

/// Typecheck and elaborate a module
pub fn check_module(raw_module: &RawModule) -> Result<Module, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(raw_module.definitions.len());

    for raw_definition in &raw_module.definitions {
        let name = raw_definition.name.clone();
        let (term, ann) = match *raw_definition.ann {
            // We don't have a type annotation available to us! Instead we will
            // attempt to infer it based on the body of the definition
            RawTerm::Hole(_) => infer(&context, &raw_definition.term)?,
            // We have a type annotation! Elaborate it, then nomalize it, then
            // check that it matches the body of the definition
            _ => {
                let (ann, _) = infer(&context, &raw_definition.ann)?;
                let ann = normalize(&context, &ann)?;
                let term = check(&context, &raw_definition.term, &ann)?;
                (term, ann)
            },
        };

        // Add the definition to the context
        context = context.claim(Name::user(name.clone()), ann.clone());
        context = context.define(Name::user(name.clone()), term.clone());

        definitions.push(Definition { name, term, ann })
    }

    Ok(Module {
        name: raw_module.name.clone(),
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

        // E-VAR, E-VAR-DEF
        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_definition(name) {
                Some(term) => normalize(context, term),
                None => Ok(Rc::new(Value::from(Neutral::Var(var.clone())))),
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

        // E-SING
        Term::Singleton(_, ref expr) => Ok(Rc::new(Value::Singleton(normalize(context, expr)?))),

        // E-PI
        Term::Pi(_, ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &ann)?;
            let body = normalize(&context.claim(name.clone(), ann.clone()), &body)?;

            Ok(Rc::new(Value::Pi(nameless::bind((name, Embed(ann)), body))))
        },

        // E-LAM
        Term::Lam(_, ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &ann)?;
            let body = normalize(&context.claim(name.clone(), ann.clone()), &body)?;

            Ok(Rc::new(Value::Lam(nameless::bind(
                (name, Embed(ann)),
                body,
            ))))
        },

        // E-APP
        Term::App(_, ref expr, ref arg) => {
            let value_expr = normalize(context, expr)?;

            match *value_expr {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((name, Embed(_)), body) = nameless::unbind(scope.clone());

                    normalize(
                        &context.define(name, arg.clone()),
                        &Rc::new(Term::from(&*body)),
                    )
                },
                Value::Neutral(ref expr) => Ok(Rc::new(Value::from(Neutral::App(
                    expr.clone(),
                    arg.clone(),
                )))),
                _ => Err(InternalError::ArgumentAppliedToNonFunction { span: expr.span() }),
            }
        },
    }
}

/// Type checking of terms
pub fn check(
    context: &Context,
    raw_term: &Rc<RawTerm>,
    expected_ty: &Rc<Type>,
) -> Result<Rc<Term>, TypeError> {
    match (&**raw_term, &**expected_ty) {
        (&RawTerm::Constant(meta, ref raw_c), &Value::Constant(ref c_ty)) => {
            use syntax::core::RawConstant as RawC;

            let c = match (raw_c, c_ty) {
                (&RawC::String(ref val), &Constant::StringType) => Constant::String(val.clone()),
                (&RawC::Char(val), &Constant::CharType) => Constant::Char(val),

                // FIXME: overflow?
                (&RawC::Int(val), &Constant::U8Type) => Constant::U8(val as u8),
                (&RawC::Int(val), &Constant::U16Type) => Constant::U16(val as u16),
                (&RawC::Int(val), &Constant::U32Type) => Constant::U32(val as u32),
                (&RawC::Int(val), &Constant::U64Type) => Constant::U64(val),
                (&RawC::Int(val), &Constant::I8Type) => Constant::I8(val as i8),
                (&RawC::Int(val), &Constant::I16Type) => Constant::I16(val as i16),
                (&RawC::Int(val), &Constant::I32Type) => Constant::I32(val as i32),
                (&RawC::Int(val), &Constant::I64Type) => Constant::I64(val as i64),
                (&RawC::Int(val), &Constant::F32Type) => Constant::F32(val as f32),
                (&RawC::Int(val), &Constant::F64Type) => Constant::F64(val as f64),
                (&RawC::Float(val), &Constant::F32Type) => Constant::F32(val as f32),
                (&RawC::Float(val), &Constant::F64Type) => Constant::F64(val),

                (_, _) => {
                    return Err(TypeError::LiteralMismatch {
                        literal_span: meta.span,
                        found: raw_c.clone(),
                        expected: c_ty.clone(),
                    });
                },
            };

            return Ok(Rc::new(Term::Constant(meta, c)));
        },

        // C-LAM
        (&RawTerm::Lam(meta, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (pi_name, Embed(pi_ann)), pi_body) =
                nameless::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let RawTerm::Hole(_) = *lam_ann {
                let lam_ann = Rc::new(Term::from(&*pi_ann));
                let lam_body = check(&context.claim(pi_name, pi_ann), &lam_body, &pi_body)?;
                let lam_scope = nameless::bind((lam_name, Embed(lam_ann)), lam_body);

                return Ok(Rc::new(Term::Lam(meta, lam_scope)));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and reunbinding at I-LAM
        },
        (&RawTerm::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: raw_term.span(),
                expected: expected_ty.clone(),
            });
        },

        (&RawTerm::Hole(meta), _) => {
            return Err(TypeError::UnableToElaborateHole {
                span: meta.span,
                expected: Some(expected_ty.clone()),
            });
        },

        _ => {},
    }

    // C-CONV
    let (term, inferred_ty) = infer(context, raw_term)?;
    match Type::term_eq(&inferred_ty, expected_ty) {
        true => Ok(term),
        false => Err(TypeError::Mismatch {
            span: term.span(),
            found: inferred_ty,
            expected: expected_ty.clone(),
        }),
    }
}

/// Type inference of terms
pub fn infer(context: &Context, raw_term: &Rc<RawTerm>) -> Result<(Rc<Term>, Rc<Type>), TypeError> {
    use std::cmp;

    /// Ensures that the given term is a universe, returning the level of that
    /// universe and its elaborated form.
    fn infer_universe(
        context: &Context,
        raw_term: &Rc<RawTerm>,
    ) -> Result<(Rc<Term>, Level), TypeError> {
        let (term, ty) = infer(context, raw_term)?;
        match *ty {
            Value::Universe(level) => Ok((term, level)),
            _ => Err(TypeError::ExpectedUniverse {
                span: raw_term.span(),
                found: ty,
            }),
        }
    }

    match **raw_term {
        //  I-ANN
        RawTerm::Ann(meta, ref raw_expr, ref raw_ty) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = normalize(context, &ty)?;
            let expr = check(context, raw_expr, &value_ty)?;

            Ok((Rc::new(Term::Ann(meta, expr, ty)), value_ty))
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

        RawTerm::Constant(meta, ref raw_c) => match *raw_c {
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

        // I-VAR
        RawTerm::Var(meta, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_claim(name) {
                Some(ty) => Ok((Rc::new(Term::Var(meta, var.clone())), ty.clone())),
                None => Err(TypeError::UndefinedName {
                    var_span: meta.span,
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(ref name, index) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: raw_term.span(),
                name: name.clone(),
                index: index,
            }.into()),
        },

        // I-SING
        RawTerm::Singleton(meta, ref expr) => {
            let (expr, ty1) = infer(context, expr)?;
            let (_, ty2) = infer(context, &Rc::new(RawTerm::from(&*ty1)))?;

            Ok((Term::Singleton(meta, expr), ty2))
        },

        // I-PI
        RawTerm::Pi(meta, ref raw_scope) => {
            let ((name, Embed(raw_ann)), raw_body) = nameless::unbind(raw_scope.clone());

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(name.clone(), ann), &raw_body)?
            };

            let scope = nameless::bind((name, Embed(ann)), body);
            let level = cmp::max(ann_level, body_level);

            Ok((
                Rc::new(Term::Pi(meta, scope)),
                Rc::new(Value::Universe(level)),
            ))
        },

        // I-LAM
        RawTerm::Lam(meta, ref raw_scope) => {
            let ((name, Embed(raw_ann)), raw_body) = nameless::unbind(raw_scope.clone());

            // Check for holes before entering to ensure we get a nice error
            if let RawTerm::Hole(_) = *raw_ann {
                return Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: name.clone(),
                });
            }

            let (lam_ann, _) = infer_universe(context, &raw_ann)?;
            let pi_ann = normalize(context, &lam_ann)?;
            let (lam_body, pi_body) =
                infer(&context.claim(name.clone(), pi_ann.clone()), &raw_body)?;

            let lam_param = (name.clone(), Embed(lam_ann));
            let pi_param = (name.clone(), Embed(pi_ann));

            Ok((
                Rc::new(Term::Lam(meta, nameless::bind(lam_param, lam_body))),
                Rc::new(Value::Pi(nameless::bind(pi_param, pi_body))),
            ))
        },

        // I-APP
        RawTerm::App(meta, ref raw_expr, ref raw_arg) => {
            let (expr, expr_ty) = infer(context, raw_expr)?;

            match *expr_ty {
                Value::Pi(ref scope) => {
                    let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

                    let arg = check(context, raw_arg, &ann)?;
                    let body = normalize(
                        &context.define(name, arg.clone()),
                        &Rc::new(Term::from(&*body)),
                    )?;

                    Ok((Rc::new(Term::App(meta, expr, arg)), body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: raw_expr.span(),
                    arg_span: raw_arg.span(),
                    found: expr_ty.clone(),
                }),
            }
        },
    }
}

pub fn subtype(context: &Context, ty1: &Rc<Type>, ty2: &Rc<Type>) -> Result<(), TypeError> {
    match (&**ty1, &**ty2) {
        // S-SING
        (Value::Singleton(ref expr1), _) => {
            let (_, ty1) = infer(context, &Rc::new(RawTerm::from(&**expr1)))?;

            equiv(&ty1, ty2)
        },

        // S-SING-EQ-SYM
        (Value::Singleton(ref expr1), Value::Singleton(ref expr2)) => {
            let (_, ty1) = infer(context, &Rc::new(RawTerm::from(&**expr1)))?;
            let (_, ty2) = infer(context, &Rc::new(RawTerm::from(&**expr2)))?;

            equiv(&ty1, &ty2)
        }

        // S-REFL
        (_, _) => equiv(ty1, ty2),

        // TODO: pi types
    }
}

pub fn equiv(ty1: &Rc<Type>, ty2: &Rc<Type>) -> Result<(), TypeError> {
    match Type::term_eq(ty1, ty2) {
        true => Ok(()),
        false => Err(TypeError::Mismatch {
            span: term.span(),
            found: ty1.clone(),
            expected: ty2.clone(),
        }),
    }
}
