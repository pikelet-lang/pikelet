//! The semantics of the language
//!
//! Here we define the rules of normalization, type checking, and type inference.
//!
//! For more information, check out the theory appendix of the Pikelet book.

use codespan::ByteSpan;
use nameless::{self, BoundPattern, BoundTerm, Embed, Ignore, Name, Var};
use std::rc::Rc;

use syntax::context::Context;
use syntax::core::{
    Definition, Head, Label, Level, Literal, Module, Neutral, RawLiteral, RawModule, RawTerm, Term,
    Type, Value,
};
use syntax::translation::Resugar;

mod errors;
#[cfg(test)]
mod tests;

pub use self::errors::{InternalError, TypeError};

/// Typecheck and elaborate a module
pub fn check_module(raw_module: &RawModule) -> Result<Module, TypeError> {
    let mut context = Context::default();
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
        context = context.define_term(Name::user(name.clone()), ann.clone(), term.clone());

        definitions.push(Definition { name, term, ann })
    }

    Ok(Module { definitions })
}

/// Apply a substitution to a value
///
/// Since this may 'unstick' some neutral terms, the returned term will need to
/// be re-evaluated afterwards to ensure that it remains in its normal form.
pub fn subst(value: &Value, substs: &[(Name, Rc<Term>)]) -> Rc<Term> {
    match *value {
        Value::Universe(level) => Rc::new(Term::Universe(Ignore::default(), level)),
        Value::Literal(ref lit) => Rc::new(Term::Literal(Ignore::default(), lit.clone())),
        Value::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind((name, Embed(subst(&ann, substs))), subst(&body, substs)),
            ))
        },
        Value::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind((name, Embed(subst(&ann, substs))), subst(&body, substs)),
            ))
        },
        Value::RecordType(ref scope) => {
            let ((label, Embed(ann)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::RecordType(
                Ignore::default(),
                nameless::bind((label, Embed(subst(&ann, substs))), subst(&body, substs)),
            ))
        },
        Value::Record(ref scope) => {
            let ((label, Embed(expr)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::Record(
                Ignore::default(),
                nameless::bind((label, Embed(subst(&expr, substs))), subst(&body, substs)),
            ))
        },
        Value::RecordTypeEmpty => Rc::new(Term::RecordTypeEmpty(Ignore::default())),
        Value::RecordEmpty => Rc::new(Term::RecordEmpty(Ignore::default())),
        Value::Array(ref elems) => Rc::new(Term::Array(
            Ignore::default(),
            elems.iter().map(|elem| subst(elem, substs)).collect(),
        )),
        Value::Neutral(ref neutral) => {
            let (head, spine) = match **neutral {
                Neutral::App(Head::Var(Var::Free(ref name)), ref spine) => {
                    let head = match substs.iter().find(|s| *name == s.0) {
                        Some(&(_, ref term)) => term.clone(),
                        None => Rc::new(Term::Var(Ignore::default(), Var::Free(name.clone()))),
                    };

                    (head, spine)
                },
                Neutral::App(Head::Var(ref var), ref spine) => {
                    (Rc::new(Term::Var(Ignore::default(), var.clone())), spine)
                },
                Neutral::If(ref cond, ref if_true, ref if_false, ref spine) => {
                    let head = Rc::new(Term::If(
                        Ignore::default(),
                        subst(&Value::Neutral(cond.clone()), substs),
                        subst(if_true, substs),
                        subst(if_false, substs),
                    ));

                    (head, spine)
                },
                Neutral::Proj(ref expr, ref label, ref spine) => {
                    let head = Rc::new(Term::Proj(
                        Ignore::default(),
                        subst(&Value::Neutral(expr.clone()), substs),
                        Ignore::default(),
                        label.clone(),
                    ));

                    (head, spine)
                },
            };

            spine
                .iter()
                .fold(head, |acc, arg| Rc::new(Term::App(acc, subst(arg, substs))))
        },
    }
}

/// Reduce a term to its normal form
pub fn normalize(context: &Context, term: &Rc<Term>) -> Result<Rc<Value>, InternalError> {
    use syntax::context::Definition;

    match **term {
        // E-ANN
        Term::Ann(_, ref expr, _) => normalize(context, expr),

        // E-TYPE
        Term::Universe(_, level) => Ok(Rc::new(Value::Universe(level))),

        Term::Literal(_, ref lit) => Ok(Rc::new(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_definition(name) {
                Some(Definition::Term(term)) => normalize(context, &term),
                Some(Definition::Prim(_)) | None => Ok(Rc::new(Value::from(var.clone()))),
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
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(Rc::new(Value::Pi(nameless::bind((name, Embed(ann)), body))))
        },

        // E-LAM
        Term::Lam(_, ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(Rc::new(Value::Lam(nameless::bind(
                (name, Embed(ann)),
                body,
            ))))
        },

        // E-APP
        Term::App(ref expr, ref arg) => {
            let mut value_expr = normalize(context, expr)?;

            match Rc::make_mut(&mut value_expr) {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((name, Embed(_)), body) = nameless::unbind(scope.clone());
                    normalize(context, &subst(&*body, &vec![(name, arg.clone())]))
                },
                Value::Neutral(ref mut neutral) => {
                    let arg = normalize(context, arg)?;

                    // Update the spine in place, if possible
                    match *Rc::make_mut(neutral) {
                        Neutral::App(Head::Var(Var::Free(ref name)), ref mut spine) => {
                            spine.push(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully normalized
                            if let Some(Definition::Prim(prim)) = context.lookup_definition(name) {
                                if prim.arity == spine.len() && spine.iter().all(|arg| arg.is_nf())
                                {
                                    return Ok((prim.fun)(spine).unwrap());
                                }
                            }
                        },
                        Neutral::App(_, ref mut spine)
                        | Neutral::If(_, _, _, ref mut spine)
                        | Neutral::Proj(_, _, ref mut spine) => spine.push(arg),
                    }

                    Ok(Rc::new(Value::Neutral(neutral.clone())))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction { span: expr.span() }),
            }
        },

        // E-IF, E-IF-TRUE, E-IF-FALSE
        Term::If(_, ref cond, ref if_true, ref if_false) => {
            let value_cond = normalize(context, cond)?;

            match *value_cond {
                Value::Literal(Literal::Bool(true)) => normalize(context, if_true),
                Value::Literal(Literal::Bool(false)) => normalize(context, if_false),
                Value::Neutral(ref cond) => Ok(Rc::new(Value::from(Neutral::If(
                    cond.clone(),
                    normalize(context, if_true)?,
                    normalize(context, if_false)?,
                    vec![],
                )))),
                _ => Err(InternalError::ExpectedBoolExpr { span: cond.span() }),
            }
        },

        // E-RECORD-TYPE
        Term::RecordType(_, ref scope) => {
            let ((label, Embed(ann)), body) = nameless::unbind(scope.clone());
            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(Value::RecordType(nameless::bind((label, Embed(ann)), body)).into())
        },

        // E-EMPTY-RECORD-TYPE
        Term::RecordTypeEmpty(_) => Ok(Rc::new(Value::RecordTypeEmpty)),

        // E-RECORD
        Term::Record(_, ref scope) => {
            let ((label, Embed(term)), body) = nameless::unbind(scope.clone());
            let value = normalize(context, &term)?;
            let body = normalize(context, &body)?;

            Ok(Value::Record(nameless::bind((label, Embed(value)), body)).into())
        },

        // E-EMPTY-RECORD
        Term::RecordEmpty(_) => Ok(Rc::new(Value::RecordEmpty)),

        // E-PROJ
        Term::Proj(_, ref expr, label_span, ref label) => {
            match *normalize(context, expr)? {
                Value::Neutral(ref neutral) => Ok(Rc::new(Value::from(Neutral::Proj(
                    neutral.clone(),
                    label.clone(),
                    vec![],
                )))),
                ref expr => match expr.lookup_record(label) {
                    Some(value) => Ok(value.clone()),
                    None => Err(InternalError::ProjectedOnNonExistentField {
                        label_span: label_span.0, // FIXME: better location info
                        label: label.clone(),
                    }),
                },
            }
        },

        Term::Array(_, ref elems) => Ok(Rc::new(Value::Array(elems
            .iter()
            .map(|elem| normalize(context, elem))
            .collect::<Result<_, _>>()?))),
    }
}

/// Type checking of terms
pub fn check(
    context: &Context,
    raw_term: &Rc<RawTerm>,
    expected_ty: &Rc<Type>,
) -> Result<Rc<Term>, TypeError> {
    match (&**raw_term, &**expected_ty) {
        (&RawTerm::Literal(span, ref raw_literal), ty) => {
            fn is_name(ty: &Type, name: &str) -> bool {
                if let Value::Neutral(ref neutral) = *ty {
                    if let Neutral::App(Head::Var(Var::Free(ref n)), ref spine) = **neutral {
                        return Name::user(name) == *n && spine.is_empty();
                    }
                }
                false
            }

            let literal = match *raw_literal {
                RawLiteral::String(ref val) if is_name(ty, "String") => {
                    Literal::String(val.clone())
                },
                RawLiteral::Char(val) if is_name(ty, "Char") => Literal::Char(val),

                // FIXME: overflow?
                RawLiteral::Int(val) if is_name(ty, "U8") => Literal::U8(val as u8),
                RawLiteral::Int(val) if is_name(ty, "U16") => Literal::U16(val as u16),
                RawLiteral::Int(val) if is_name(ty, "U32") => Literal::U32(val as u32),
                RawLiteral::Int(val) if is_name(ty, "U64") => Literal::U64(val),
                RawLiteral::Int(val) if is_name(ty, "I8") => Literal::I8(val as i8),
                RawLiteral::Int(val) if is_name(ty, "I16") => Literal::I16(val as i16),
                RawLiteral::Int(val) if is_name(ty, "I32") => Literal::I32(val as i32),
                RawLiteral::Int(val) if is_name(ty, "I64") => Literal::I64(val as i64),
                RawLiteral::Int(val) if is_name(ty, "F32") => Literal::F32(val as f32),
                RawLiteral::Int(val) if is_name(ty, "F64") => Literal::F64(val as f64),
                RawLiteral::Float(val) if is_name(ty, "F32") => Literal::F32(val as f32),
                RawLiteral::Float(val) if is_name(ty, "F64") => Literal::F64(val),

                _ => {
                    return Err(TypeError::LiteralMismatch {
                        literal_span: span.0,
                        found: raw_literal.clone(),
                        expected: Box::new(expected_ty.resugar()),
                    });
                },
            };

            return Ok(Rc::new(Term::Literal(span, literal)));
        },

        // C-LAM
        (&RawTerm::Lam(span, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (pi_name, Embed(pi_ann)), pi_body) =
                nameless::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let RawTerm::Hole(_) = *lam_ann {
                let lam_ann = Rc::new(Term::from(&*pi_ann));
                let lam_body = check(&context.claim(pi_name, pi_ann), &lam_body, &pi_body)?;
                let lam_scope = nameless::bind((lam_name, Embed(lam_ann)), lam_body);

                return Ok(Rc::new(Term::Lam(span, lam_scope)));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and reunbinding at I-LAM
        },
        (&RawTerm::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: raw_term.span(),
                expected: Box::new(expected_ty.resugar()),
            });
        },

        // C-IF
        (&RawTerm::If(span, ref raw_cond, ref raw_if_true, ref raw_if_false), _) => {
            let bool_ty = Rc::new(Value::from(Var::Free(Name::user("Bool"))));
            let cond = check(context, raw_cond, &bool_ty)?;
            let if_true = check(context, raw_if_true, expected_ty)?;
            let if_false = check(context, raw_if_false, expected_ty)?;

            return Ok(Rc::new(Term::If(span, cond, if_true, if_false)));
        },

        // C-RECORD
        (&RawTerm::Record(span, ref scope), &Value::RecordType(ref ty_scope)) => {
            let ((label, Embed(raw_expr)), raw_body, (ty_label, Embed(ann)), ty_body) =
                nameless::unbind2(scope.clone(), ty_scope.clone());

            if Label::pattern_eq(&label, &ty_label) {
                let expr = check(context, &raw_expr, &ann)?;
                let ty_body = normalize(
                    context,
                    &subst(&ty_body, &vec![(label.0.clone(), expr.clone())]),
                )?;
                let body = check(context, &raw_body, &ty_body)?;

                return Ok(Rc::new(Term::Record(
                    span,
                    nameless::bind((label, Embed(expr)), body),
                )));
            } else {
                return Err(TypeError::LabelMismatch {
                    span: span.0,
                    found: label,
                    expected: ty_label,
                });
            }
        },

        (&RawTerm::Array(span, ref elems), ty) => match ty.free_app() {
            Some((name, [ref len, ref elem_ty])) if *name == Name::user("Array") => {
                if let Value::Literal(Literal::U64(len)) = **len {
                    if len != elems.len() as u64 {
                        return Err(TypeError::ArrayLengthMismatch {
                            span: span.0,
                            found_len: elems.len() as u64,
                            expected_len: len,
                        });
                    }
                }

                return Ok(Rc::new(Term::Array(
                    span,
                    elems
                        .iter()
                        .map(|elem| check(context, elem, elem_ty))
                        .collect::<Result<_, _>>()?,
                )));
            },
            Some(_) | None => unimplemented!(),
        },

        (&RawTerm::Hole(span), _) => {
            return Err(TypeError::UnableToElaborateHole {
                span: span.0,
                expected: Some(Box::new(expected_ty.resugar())),
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
            found: Box::new(inferred_ty.resugar()),
            expected: Box::new(expected_ty.resugar()),
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
                found: Box::new(ty.resugar()),
            }),
        }
    }

    match **raw_term {
        //  I-ANN
        RawTerm::Ann(span, ref raw_expr, ref raw_ty) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = normalize(context, &ty)?;
            let expr = check(context, raw_expr, &value_ty)?;

            Ok((Rc::new(Term::Ann(span, expr, ty)), value_ty))
        },

        // I-TYPE
        RawTerm::Universe(span, level) => Ok((
            Rc::new(Term::Universe(span, level)),
            Rc::new(Value::Universe(level.succ())),
        )),

        RawTerm::Hole(span) => Err(TypeError::UnableToElaborateHole {
            span: span.0,
            expected: None,
        }),

        RawTerm::Literal(span, ref raw_literal) => match *raw_literal {
            RawLiteral::String(ref value) => Ok((
                Rc::new(Term::Literal(span, Literal::String(value.clone()))),
                Rc::new(Value::from(Var::Free(Name::user("String")))),
            )),
            RawLiteral::Char(value) => Ok((
                Rc::new(Term::Literal(span, Literal::Char(value))),
                Rc::new(Value::from(Var::Free(Name::user("Char")))),
            )),
            RawLiteral::Int(_) => Err(TypeError::AmbiguousIntLiteral { span: span.0 }),
            RawLiteral::Float(_) => Err(TypeError::AmbiguousFloatLiteral { span: span.0 }),
        },

        // I-VAR
        RawTerm::Var(span, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_claim(name) {
                Some(ty) => Ok((Rc::new(Term::Var(span, var.clone())), ty.clone())),
                None => Err(TypeError::UndefinedName {
                    var_span: span.0,
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

        // I-PI
        RawTerm::Pi(span, ref raw_scope) => {
            let ((name, Embed(raw_ann)), raw_body) = nameless::unbind(raw_scope.clone());

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(name.clone(), ann), &raw_body)?
            };

            Ok((
                Rc::new(Term::Pi(span, nameless::bind((name, Embed(ann)), body))),
                Rc::new(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        // I-LAM
        RawTerm::Lam(span, ref raw_scope) => {
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
                Rc::new(Term::Lam(span, nameless::bind(lam_param, lam_body))),
                Rc::new(Value::Pi(nameless::bind(pi_param, pi_body))),
            ))
        },

        // I-IF
        RawTerm::If(span, ref raw_cond, ref raw_if_true, ref raw_if_false) => {
            let bool_ty = Rc::new(Value::from(Var::Free(Name::user("Bool"))));
            let cond = check(context, raw_cond, &bool_ty)?;
            let (if_true, ty) = infer(context, raw_if_true)?;
            let if_false = check(context, raw_if_false, &ty)?;

            Ok((Rc::new(Term::If(span, cond, if_true, if_false)), ty))
        },

        // I-APP
        RawTerm::App(ref raw_expr, ref raw_arg) => {
            let (expr, expr_ty) = infer(context, raw_expr)?;

            match *expr_ty {
                Value::Pi(ref scope) => {
                    let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());

                    let arg = check(context, raw_arg, &ann)?;
                    let body = normalize(context, &subst(&*body, &vec![(name, arg.clone())]))?;

                    Ok((Rc::new(Term::App(expr, arg)), body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: raw_expr.span(),
                    arg_span: raw_arg.span(),
                    found: Box::new(expr_ty.resugar()),
                }),
            }
        },

        // I-RECORD-TYPE
        RawTerm::RecordType(span, ref raw_scope) => {
            let ((label, Embed(raw_ann)), raw_body) = nameless::unbind(raw_scope.clone());

            // Check that rest of record is well-formed?
            // Might be able to skip that for now, because there's no way to
            // express ill-formed records in the concrete syntax...

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(label.0.clone(), ann), &raw_body)?
            };

            let scope = nameless::bind((label, Embed(ann)), body);

            Ok((
                Rc::new(Term::RecordType(span, scope)),
                Rc::new(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        RawTerm::Record(span, _) => Err(TypeError::AmbiguousRecord { span: span.0 }),

        // I-EMPTY-RECORD-TYPE
        RawTerm::RecordTypeEmpty(span) => Ok((
            Rc::new(Term::RecordTypeEmpty(span)),
            Rc::new(Value::Universe(Level(0))),
        )),

        // I-EMPTY-RECORD
        RawTerm::RecordEmpty(span) => Ok((
            Rc::new(Term::RecordEmpty(span)),
            Rc::new(Value::RecordTypeEmpty),
        )),

        // I-PROJ
        RawTerm::Proj(span, ref expr, label_span, ref label) => {
            let (expr, ty) = infer(context, expr)?;

            match ty.lookup_record_ty(label) {
                Some(field_ty) => {
                    let substs = field_substs(&expr, &label, &ty);
                    Ok((
                        Rc::new(Term::Proj(span, expr, label_span, label.clone())),
                        normalize(context, &subst(&field_ty, &substs))?,
                    ))
                },
                None => Err(TypeError::NoFieldInType {
                    label_span: label_span.0,
                    expected_label: label.clone(),
                    found: Box::new(ty.resugar()),
                }),
            }
        },

        RawTerm::Array(span, _) => Err(TypeError::AmbiguousArrayLiteral { span: span.0 }),
    }
}

fn field_substs(expr: &Rc<Term>, label: &Label, ty: &Rc<Type>) -> Vec<(Name, Rc<Term>)> {
    let mut substs = vec![];
    let mut current_scope = ty.record_ty();

    while let Some(scope) = current_scope {
        let ((curr_label, Embed(_)), body) = nameless::unbind(scope);

        if Label::pattern_eq(&curr_label, &label) {
            break;
        }

        let proj = Rc::new(Term::Proj(
            Ignore::default(),
            expr.clone(),
            Ignore::default(),
            curr_label.clone(),
        ));

        substs.push((curr_label.0, proj));
        current_scope = body.record_ty();
    }

    substs
}
