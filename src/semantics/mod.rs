//! The semantics of the language
//!
//! Here we define the rules of normalization, type checking, and type inference.
//!
//! For more information, check out the theory appendix of the Pikelet book.

use codespan::ByteSpan;
use moniker::{Binder, BoundTerm, Embed, FreeVar, Nest, Scope, Var};

use syntax::context::Context;
use syntax::core::{
    Definition, Head, Literal, Module, Neutral, Pattern, RcNeutral, RcPattern, RcTerm, RcType,
    RcValue, Term, Type, Value,
};
use syntax::raw;
use syntax::translation::Resugar;
use syntax::Level;

mod errors;
#[cfg(test)]
mod tests;

pub use self::errors::{InternalError, TypeError};

/// Type check and elaborate a module
pub fn check_module(raw_module: &raw::Module) -> Result<Module, TypeError> {
    let mut context = Context::default();
    let definitions = raw_module
        .definitions
        .clone()
        .unnest()
        .into_iter()
        .map(|(Binder(free_var), Embed(raw_definition))| {
            let (term, ann) = match *raw_definition.ann.inner {
                // We don't have a type annotation available to us! Instead we will
                // attempt to infer it based on the body of the definition
                raw::Term::Hole(_) => infer_term(&context, &raw_definition.term)?,
                // We have a type annotation! Elaborate it, then normalize it, then
                // check that it matches the body of the definition
                _ => {
                    let (ann, _) = infer_term(&context, &raw_definition.ann)?;
                    let ann = normalize(&context, &ann)?;
                    let term = check_term(&context, &raw_definition.term, &ann)?;
                    (term, ann)
                },
            };

            // Add the definition to the context
            context = context.define_term(free_var.clone(), ann.clone(), term.clone());

            Ok((Binder(free_var), Embed(Definition { term, ann })))
        })
        .collect::<Result<_, TypeError>>()?;

    Ok(Module {
        definitions: Nest::new(definitions),
    })
}

/// Reduce a term to its normal form
pub fn normalize(context: &Context, term: &RcTerm) -> Result<RcValue, InternalError> {
    use syntax::context::Definition;

    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => normalize(context, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var) => match *var {
            Var::Free(ref name) => match context.lookup_definition(name) {
                Some(Definition::Term(term)) => normalize(context, &term),
                Some(Definition::Prim(_)) | None => Ok(RcValue::from(Value::from(var.clone()))),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_, _, _) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: None,
                var: var.clone(),
            }),
        },

        // E-PI
        Term::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Pi(Scope::new(
                (name, Embed(normalize(context, &ann)?)),
                normalize(context, &body)?,
            ))))
        },

        // E-LAM
        Term::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Lam(Scope::new(
                (name, Embed(normalize(context, &ann)?)),
                normalize(context, &body)?,
            ))))
        },

        // E-APP
        Term::App(ref head, ref arg) => {
            match *normalize(context, head)?.inner {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((Binder(free_var), Embed(_)), body) = scope.clone().unbind();
                    normalize(context, &body.substs(&[(free_var, arg.clone())]))
                },
                Value::Neutral(ref neutral, ref spine) => {
                    let arg = normalize(context, arg)?;
                    let mut spine = spine.clone();

                    match *neutral.inner {
                        Neutral::Head(Head::Var(Var::Free(ref free_var))) => {
                            spine.push_back(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully normalized
                            if let Some(Definition::Prim(prim)) =
                                context.lookup_definition(free_var)
                            {
                                if prim.arity == spine.len() && spine.iter().all(|arg| arg.is_nf())
                                {
                                    return Ok((prim.fun)(spine).unwrap());
                                }
                            }
                        },
                        Neutral::Head(_)
                        | Neutral::If(_, _, _)
                        | Neutral::Proj(_, _)
                        | Neutral::Case(_, _) => spine.push_back(arg),
                    }

                    Ok(RcValue::from(Value::Neutral(neutral.clone(), spine)))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction),
            }
        },

        // E-IF, E-IF-TRUE, E-IF-FALSE
        Term::If(ref cond, ref if_true, ref if_false) => {
            let value_cond = normalize(context, cond)?;

            match *value_cond {
                Value::Literal(Literal::Bool(true)) => normalize(context, if_true),
                Value::Literal(Literal::Bool(false)) => normalize(context, if_false),
                Value::Neutral(ref cond, ref spine) => Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::If(
                        cond.clone(),
                        normalize(context, if_true)?,
                        normalize(context, if_false)?,
                    )),
                    spine.clone(),
                ))),
                _ => Err(InternalError::ExpectedBoolExpr),
            }
        },

        // E-RECORD-TYPE
        Term::RecordType(ref scope) => {
            let ((label, binder, Embed(ann)), body) = scope.clone().unbind();
            let ann = normalize(context, &ann)?;
            let body = normalize(context, &body)?;

            Ok(Value::RecordType(Scope::new((label, binder, Embed(ann)), body)).into())
        },

        // E-EMPTY-RECORD-TYPE
        Term::RecordTypeEmpty => Ok(RcValue::from(Value::RecordTypeEmpty)),

        // E-RECORD
        Term::Record(ref scope) => {
            let ((label, binder, Embed(term)), body) = scope.clone().unbind();
            let value = normalize(context, &term)?;
            let body = normalize(context, &body)?;

            Ok(Value::Record(Scope::new((label, binder, Embed(value)), body)).into())
        },

        // E-EMPTY-RECORD
        Term::RecordEmpty => Ok(RcValue::from(Value::RecordEmpty)),

        // E-PROJ
        Term::Proj(ref expr, ref label) => match *normalize(context, expr)? {
            Value::Neutral(ref neutral, ref spine) => Ok(RcValue::from(Value::Neutral(
                RcNeutral::from(Neutral::Proj(neutral.clone(), label.clone())),
                spine.clone(),
            ))),
            ref expr => match expr.lookup_record(label) {
                Some(value) => Ok(value.clone()),
                None => Err(InternalError::ProjectedOnNonExistentField {
                    label: label.clone(),
                }),
            },
        },

        // E-CASE
        Term::Case(ref head, ref clauses) => {
            let head = normalize(context, head)?;

            if let Value::Neutral(ref neutral, ref spine) = *head {
                Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::Case(
                        neutral.clone(),
                        clauses
                            .iter()
                            .map(|clause| {
                                let (pattern, body) = clause.clone().unbind();
                                Ok(Scope::new(pattern, normalize(context, &body)?))
                            })
                            .collect::<Result<_, _>>()?,
                    )),
                    spine.clone(),
                )))
            } else {
                for clause in clauses {
                    let (pattern, body) = clause.clone().unbind();
                    if let Some(mappings) = match_value(&pattern, &head) {
                        let mappings = mappings
                            .into_iter()
                            .map(|(free_var, value)| (free_var, RcTerm::from(&*value.inner)))
                            .collect::<Vec<_>>();
                        return normalize(context, &body.substs(&mappings));
                    }
                }
                Err(InternalError::NoPatternsApplicable)
            }
        },

        // E-ARRAY
        Term::Array(ref elems) => Ok(RcValue::from(Value::Array(
            elems
                .iter()
                .map(|elem| normalize(context, elem))
                .collect::<Result<_, _>>()?,
        ))),
    }
}

/// If the pattern matches the value, this function returns the substitutions
/// needed to apply the pattern to some body expression
pub fn match_value(
    pattern: &RcPattern,
    value: &RcValue,
) -> Option<Vec<(FreeVar<String>, RcValue)>> {
    match (&*pattern.inner, &*value.inner) {
        (&Pattern::Literal(ref pattern_lit), &Value::Literal(ref value_lit))
            if pattern_lit == value_lit =>
        {
            Some(vec![])
        },
        (&Pattern::Binder(Binder(ref free_var)), _) => {
            Some(vec![(free_var.clone(), value.clone())])
        },
        (_, _) => None,
    }
}

/// Ensures that the given term is a universe, returning the level of that
/// universe and its elaborated form.
fn infer_universe(context: &Context, raw_term: &raw::RcTerm) -> Result<(RcTerm, Level), TypeError> {
    let (term, ty) = infer_term(context, raw_term)?;
    match *ty {
        Value::Universe(level) => Ok((term, level)),
        _ => Err(TypeError::ExpectedUniverse {
            span: raw_term.span(),
            found: Box::new(ty.resugar()),
        }),
    }
}

/// Checks that a literal is compatible with the given type, returning the
/// elaborated literal if successful
fn check_literal(raw_literal: &raw::Literal, expected_ty: &RcType) -> Result<Literal, TypeError> {
    fn is_name(ty: &RcType, name: &str) -> bool {
        match ty.free_app() {
            Some((free_var, spine)) => *free_var == FreeVar::user(name) && spine.is_empty(),
            _ => false,
        }
    }

    match *raw_literal {
        raw::Literal::String(_, ref val) if is_name(expected_ty, "String") => {
            Ok(Literal::String(val.clone()))
        },
        raw::Literal::Char(_, val) if is_name(expected_ty, "Char") => Ok(Literal::Char(val)),

        // FIXME: overflow?
        raw::Literal::Int(_, val) if is_name(expected_ty, "U8") => Ok(Literal::U8(val as u8)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "U16") => Ok(Literal::U16(val as u16)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "U32") => Ok(Literal::U32(val as u32)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "U64") => Ok(Literal::U64(val)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "I8") => Ok(Literal::I8(val as i8)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "I16") => Ok(Literal::I16(val as i16)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "I32") => Ok(Literal::I32(val as i32)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "I64") => Ok(Literal::I64(val as i64)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "F32") => Ok(Literal::F32(val as f32)),
        raw::Literal::Int(_, val) if is_name(expected_ty, "F64") => Ok(Literal::F64(val as f64)),
        raw::Literal::Float(_, val) if is_name(expected_ty, "F32") => Ok(Literal::F32(val as f32)),
        raw::Literal::Float(_, val) if is_name(expected_ty, "F64") => Ok(Literal::F64(val)),

        _ => Err(TypeError::LiteralMismatch {
            literal_span: raw_literal.span(),
            found: raw_literal.clone(),
            expected: Box::new(expected_ty.resugar()),
        }),
    }
}

/// Synthesize the type of a literal, returning the elaborated literal and the
/// inferred type if successful
fn infer_literal(raw_literal: &raw::Literal) -> Result<(Literal, RcType), TypeError> {
    match *raw_literal {
        raw::Literal::String(_, ref value) => Ok((
            Literal::String(value.clone()),
            RcValue::from(Value::from(Var::user("String"))),
        )),
        raw::Literal::Char(_, value) => Ok((
            Literal::Char(value),
            RcValue::from(Value::from(Var::user("Char"))),
        )),
        raw::Literal::Int(span, _) => Err(TypeError::AmbiguousIntLiteral { span }),
        raw::Literal::Float(span, _) => Err(TypeError::AmbiguousFloatLiteral { span }),
    }
}

/// Checks that a pattern is compatible with the given type, returning the
/// elaborated pattern and a vector of the claims it introduced if successful
pub fn check_pattern(
    context: &Context,
    raw_pattern: &raw::RcPattern,
    expected_ty: &RcType,
) -> Result<(RcPattern, Vec<(FreeVar<String>, RcType)>), TypeError> {
    match (&*raw_pattern.inner, &*expected_ty.inner) {
        (&raw::Pattern::Binder(_, Binder(ref free_var)), _) => {
            return Ok((
                RcPattern::from(Pattern::Binder(Binder(free_var.clone()))),
                vec![(free_var.clone(), expected_ty.clone())],
            ));
        },
        (&raw::Pattern::Literal(ref raw_literal), _) => {
            let literal = check_literal(raw_literal, expected_ty)?;
            return Ok((RcPattern::from(Pattern::Literal(literal)), vec![]));
        },
        _ => {},
    }

    let (pattern, inferred_ty, claims) = infer_pattern(context, raw_pattern)?;
    if Type::term_eq(&inferred_ty, expected_ty) {
        Ok((pattern, claims))
    } else {
        Err(TypeError::Mismatch {
            span: raw_pattern.span(),
            found: Box::new(inferred_ty.resugar()),
            expected: Box::new(expected_ty.resugar()),
        })
    }
}

/// Synthesize the type of a pattern, returning the elaborated pattern, the
/// inferred type, and a vector of the claims it introduced if successful
pub fn infer_pattern(
    context: &Context,
    raw_pattern: &raw::RcPattern,
) -> Result<(RcPattern, RcType, Vec<(FreeVar<String>, RcType)>), TypeError> {
    match *raw_pattern.inner {
        raw::Pattern::Ann(ref raw_pattern, Embed(ref raw_ty)) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = normalize(context, &ty)?;
            let (pattern, claims) = check_pattern(context, raw_pattern, &value_ty)?;

            Ok((
                RcPattern::from(Pattern::Ann(pattern, Embed(ty))),
                value_ty,
                claims,
            ))
        },
        raw::Pattern::Literal(ref literal) => {
            let (literal, ty) = infer_literal(literal)?;
            Ok((RcPattern::from(Pattern::Literal(literal)), ty, vec![]))
        },
        raw::Pattern::Binder(span, ref binder) => Err(TypeError::BinderNeedsAnnotation {
            span,
            binder: binder.clone(),
        }),
    }
}

/// Checks that a term is compatible with the given type, returning the
/// elaborated term if successful
pub fn check_term(
    context: &Context,
    raw_term: &raw::RcTerm,
    expected_ty: &RcType,
) -> Result<RcTerm, TypeError> {
    match (&*raw_term.inner, &*expected_ty.inner) {
        (&raw::Term::Literal(ref raw_literal), _) => {
            let literal = check_literal(raw_literal, expected_ty)?;
            return Ok(RcTerm::from(Term::Literal(literal)));
        },

        // C-LAM
        (&raw::Term::Lam(_, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (Binder(pi_name), Embed(pi_ann)), pi_body) =
                Scope::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let raw::Term::Hole(_) = *lam_ann.inner {
                let lam_ann = RcTerm::from(Term::from(&*pi_ann));
                let lam_body = check_term(&context.claim(pi_name, pi_ann), &lam_body, &pi_body)?;
                let lam_scope = Scope::new((lam_name, Embed(lam_ann)), lam_body);

                return Ok(RcTerm::from(Term::Lam(lam_scope)));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and unbinding again at I-LAM
        },
        (&raw::Term::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: raw_term.span(),
                expected: Box::new(expected_ty.resugar()),
            });
        },

        // C-IF
        (&raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false), _) => {
            let bool_ty = RcValue::from(Value::from(Var::user("Bool")));
            let cond = check_term(context, raw_cond, &bool_ty)?;
            let if_true = check_term(context, raw_if_true, expected_ty)?;
            let if_false = check_term(context, raw_if_false, expected_ty)?;

            return Ok(RcTerm::from(Term::If(cond, if_true, if_false)));
        },

        // C-RECORD
        (&raw::Term::Record(span, ref scope), &Value::RecordType(ref ty_scope)) => {
            let (
                (label, binder, Embed(raw_expr)),
                raw_body,
                (ty_label, ty_binder, Embed(ann)),
                ty_body,
            ) = Scope::unbind2(scope.clone(), ty_scope.clone());

            if label == ty_label {
                let expr = check_term(context, &raw_expr, &ann)?;
                let ty_body = normalize(context, &ty_body.substs(&[(ty_binder.0, expr.clone())]))?;
                let body = check_term(context, &raw_body, &ty_body)?;

                return Ok(RcTerm::from(Term::Record(Scope::new(
                    (label, binder, Embed(expr)),
                    body,
                ))));
            } else {
                return Err(TypeError::LabelMismatch {
                    span,
                    found: label,
                    expected: ty_label,
                });
            }
        },

        (&raw::Term::Case(_, ref raw_head, ref raw_clauses), _) => {
            let (head, head_ty) = infer_term(context, raw_head)?;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, claims) = check_pattern(context, &raw_pattern, &head_ty)?;

                    // FIXME: clean this up?
                    let mut context = context.clone();
                    for (free_var, ty) in claims {
                        context = context.claim(free_var, ty);
                    }

                    let body = check_term(&context, &raw_body, expected_ty)?;
                    Ok(Scope::new(pattern, body))
                })
                .collect::<Result<_, TypeError>>()?;

            return Ok(RcTerm::from(Term::Case(head, clauses)));
        },

        (&raw::Term::Array(span, ref elems), ty) => match ty.free_app() {
            Some((name, spine)) if *name == FreeVar::user("Array") && spine.len() == 2 => {
                let len = &spine[0];
                let elem_ty = &spine[1];
                if let Value::Literal(Literal::U64(len)) = **len {
                    if len != elems.len() as u64 {
                        return Err(TypeError::ArrayLengthMismatch {
                            span,
                            found_len: elems.len() as u64,
                            expected_len: len,
                        });
                    }
                }

                return Ok(RcTerm::from(Term::Array(
                    elems
                        .iter()
                        .map(|elem| check_term(context, elem, elem_ty))
                        .collect::<Result<_, _>>()?,
                )));
            },
            Some(_) | None => unimplemented!(),
        },

        (&raw::Term::Hole(span), _) => {
            let expected = Some(Box::new(expected_ty.resugar()));
            return Err(TypeError::UnableToElaborateHole { span, expected });
        },

        _ => {},
    }

    // C-CONV
    let (term, inferred_ty) = infer_term(context, raw_term)?;
    if Type::term_eq(&inferred_ty, expected_ty) {
        Ok(term)
    } else {
        Err(TypeError::Mismatch {
            span: raw_term.span(),
            found: Box::new(inferred_ty.resugar()),
            expected: Box::new(expected_ty.resugar()),
        })
    }
}

/// Synthesize the type of a term, returning the elaborated term and the
/// inferred type if successful
pub fn infer_term(
    context: &Context,
    raw_term: &raw::RcTerm,
) -> Result<(RcTerm, RcType), TypeError> {
    use std::cmp;

    match *raw_term.inner {
        //  I-ANN
        raw::Term::Ann(ref raw_expr, ref raw_ty) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = normalize(context, &ty)?;
            let expr = check_term(context, raw_expr, &value_ty)?;

            Ok((RcTerm::from(Term::Ann(expr, ty)), value_ty))
        },

        // I-TYPE
        raw::Term::Universe(_, level) => Ok((
            RcTerm::from(Term::Universe(level)),
            RcValue::from(Value::Universe(level.succ())),
        )),

        raw::Term::Hole(span) => {
            let expected = None;
            Err(TypeError::UnableToElaborateHole { span, expected })
        },

        raw::Term::Literal(ref raw_literal) => {
            let (literal, ty) = infer_literal(raw_literal)?;
            Ok((RcTerm::from(Term::Literal(literal)), ty))
        },

        // I-VAR
        raw::Term::Var(var_span, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_claim(name) {
                Some(ty) => Ok((RcTerm::from(Term::Var(var.clone())), ty.clone())),
                None => Err(TypeError::UndefinedName {
                    var_span,
                    name: name.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_, _, _) => Err(InternalError::UnsubstitutedDebruijnIndex {
                span: Some(raw_term.span()),
                var: var.clone(),
            }.into()),
        },

        // I-PI
        raw::Term::Pi(_, ref raw_scope) => {
            let ((Binder(free_var), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(free_var.clone(), ann), &raw_body)?
            };

            Ok((
                RcTerm::from(Term::Pi(Scope::new((Binder(free_var), Embed(ann)), body))),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        // I-LAM
        raw::Term::Lam(_, ref raw_scope) => {
            let ((Binder(name), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check for holes before entering to ensure we get a nice error
            if let raw::Term::Hole(_) = *raw_ann {
                return Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: name.clone(),
                });
            }

            let (lam_ann, _) = infer_universe(context, &raw_ann)?;
            let pi_ann = normalize(context, &lam_ann)?;
            let (lam_body, pi_body) =
                infer_term(&context.claim(name.clone(), pi_ann.clone()), &raw_body)?;

            let lam_param = (Binder(name.clone()), Embed(lam_ann));
            let pi_param = (Binder(name.clone()), Embed(pi_ann));

            Ok((
                RcTerm::from(Term::Lam(Scope::new(lam_param, lam_body))),
                RcValue::from(Value::Pi(Scope::new(pi_param, pi_body))),
            ))
        },

        // I-IF
        raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false) => {
            let bool_ty = RcValue::from(Value::from(Var::user("Bool")));
            let cond = check_term(context, raw_cond, &bool_ty)?;
            let (if_true, ty) = infer_term(context, raw_if_true)?;
            let if_false = check_term(context, raw_if_false, &ty)?;

            Ok((RcTerm::from(Term::If(cond, if_true, if_false)), ty))
        },

        // I-APP
        raw::Term::App(ref raw_head, ref raw_arg) => {
            let (head, head_ty) = infer_term(context, raw_head)?;

            match *head_ty {
                Value::Pi(ref scope) => {
                    let ((Binder(free_var), Embed(ann)), body) = scope.clone().unbind();

                    let arg = check_term(context, raw_arg, &ann)?;
                    let body = normalize(context, &body.substs(&[(free_var, arg.clone())]))?;

                    Ok((RcTerm::from(Term::App(head, arg)), body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: raw_head.span(),
                    arg_span: raw_arg.span(),
                    found: Box::new(head_ty.resugar()),
                }),
            }
        },

        // I-RECORD-TYPE
        raw::Term::RecordType(_, ref raw_scope) => {
            let ((label, binder, Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check that rest of record is well-formed?
            // Might be able to skip that for now, because there's no way to
            // express ill-formed records in the concrete syntax...

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = normalize(context, &ann)?;
                infer_universe(&context.claim(binder.0.clone(), ann), &raw_body)?
            };

            let scope = Scope::new((label, binder, Embed(ann)), body);

            Ok((
                RcTerm::from(Term::RecordType(scope)),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        raw::Term::Record(span, _) => Err(TypeError::AmbiguousRecord { span }),

        // I-EMPTY-RECORD-TYPE
        raw::Term::RecordTypeEmpty(_) => Ok((
            RcTerm::from(Term::RecordTypeEmpty),
            RcValue::from(Value::Universe(Level(0))),
        )),

        // I-EMPTY-RECORD
        raw::Term::RecordEmpty(_) => Ok((
            RcTerm::from(Term::RecordEmpty),
            RcValue::from(Value::RecordTypeEmpty),
        )),

        // I-PROJ
        raw::Term::Proj(_, ref expr, label_span, ref label) => {
            let (expr, ty) = infer_term(context, expr)?;

            match ty.lookup_record_ty(label) {
                Some(field_ty) => {
                    let mappings = field_substs(&expr, &label, &ty);
                    Ok((
                        RcTerm::from(Term::Proj(expr, label.clone())),
                        normalize(context, &field_ty.substs(&mappings))?,
                    ))
                },
                None => Err(TypeError::NoFieldInType {
                    label_span,
                    expected_label: label.clone(),
                    found: Box::new(ty.resugar()),
                }),
            }
        },

        raw::Term::Case(span, ref raw_head, ref raw_clauses) => {
            let (head, head_ty) = infer_term(context, raw_head)?;
            let mut ty = None;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, claims) = check_pattern(context, &raw_pattern, &head_ty)?;

                    // FIXME: clean this up?
                    let mut context = context.clone();
                    for (free_var, ty) in claims {
                        context = context.claim(free_var, ty);
                    }

                    let (body, body_ty) = infer_term(&context, &raw_body)?;

                    match ty {
                        None => ty = Some(body_ty),
                        Some(ref ty) if RcValue::term_eq(&body_ty, ty) => {},
                        Some(ref ty) => {
                            return Err(TypeError::Mismatch {
                                span: raw_body.span(),
                                found: Box::new(body_ty.resugar()),
                                expected: Box::new(ty.resugar()),
                            });
                        },
                    }

                    Ok(Scope::new(pattern, body))
                })
                .collect::<Result<_, TypeError>>()?;

            match ty {
                Some(ty) => Ok((RcTerm::from(Term::Case(head, clauses)), ty)),
                None => Err(TypeError::AmbiguousEmptyCase { span }),
            }
        },

        raw::Term::Array(span, _) => Err(TypeError::AmbiguousArrayLiteral { span }),
    }
}

fn field_substs(expr: &RcTerm, label: &str, ty: &RcType) -> Vec<(FreeVar<String>, RcTerm)> {
    let mut substs = vec![];
    let mut current_scope = ty.record_ty();

    while let Some(scope) = current_scope {
        let ((current_label, current_binder, Embed(_)), body) = scope.unbind();

        if current_label == label {
            break;
        }

        let proj = RcTerm::from(Term::Proj(expr.clone(), current_label));
        substs.push((current_binder.0, proj));

        current_scope = body.record_ty();
    }

    substs
}
