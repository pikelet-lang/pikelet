//! The semantics of the language
//!
//! Here we define the rules of normalization, type checking, and type inference.
//!
//! For more information, check out the theory appendix of the Pikelet book.

extern crate codespan;
extern crate codespan_reporting;
#[macro_use]
extern crate failure;
extern crate im;
extern crate moniker;
extern crate pikelet_syntax;

use codespan::ByteSpan;
use moniker::{Binder, BoundPattern, BoundTerm, Embed, FreeVar, Nest, Scope, Var};

use pikelet_syntax::core::{Literal, Pattern, RcPattern, RcTerm, RcType, RcValue, Term, Value};
use pikelet_syntax::raw;
use pikelet_syntax::Level;

mod context;
mod errors;
mod normalize;

pub use self::context::{Context, Globals, Import};
pub use self::errors::{InternalError, TypeError};
pub use self::normalize::{match_value, nf_term};

/// Returns true if `ty1` is a subtype of `ty2`
fn is_subtype(context: &Context, ty1: &RcType, ty2: &RcType) -> bool {
    match (&*ty1.inner, &*ty2.inner) {
        // ST-TYPE
        (&Value::Universe(level1), &Value::Universe(level2)) => level1 <= level2,

        // ST-PI
        (&Value::Pi(ref scope1), &Value::Pi(ref scope2)) => {
            let ((_, Embed(ann1)), body1, (Binder(free_var2), Embed(ann2)), body2) =
                Scope::unbind2(scope1.clone(), scope2.clone());

            is_subtype(context, &ann2, &ann1) && {
                let mut context = context.clone();
                context.insert_declaration(free_var2, ann2);
                is_subtype(&context, &body1, &body2)
            }
        },

        // ST-RECORD-TYPE, ST-EMPTY-RECORD-TYPE
        (&Value::RecordType(ref scope1), &Value::RecordType(ref scope2)) => {
            if scope1.unsafe_pattern.unsafe_patterns.len()
                != scope2.unsafe_pattern.unsafe_patterns.len()
            {
                return false;
            }

            let (fields1, (), fields2, ()) = Scope::unbind2(scope1.clone(), scope2.clone());

            let mut context = context.clone();
            for (field1, field2) in
                Iterator::zip(fields1.unnest().into_iter(), fields2.unnest().into_iter())
            {
                let (label1, Binder(free_var1), Embed(ty1)) = field1;
                let (label2, _, Embed(ty2)) = field2;

                if label1 == label2 && is_subtype(&context, &ty1, &ty2) {
                    context.insert_declaration(free_var1, ty1);
                } else {
                    return false;
                }
            }

            true
        },

        // ST-ALPHA-EQ
        (_, _) => RcType::term_eq(ty1, ty2),
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
            found: Box::new(context.resugar(&ty)),
        }),
    }
}

/// Checks that a literal is compatible with the given type, returning the
/// elaborated literal if successful
fn check_literal(
    context: &Context,
    raw_literal: &raw::Literal,
    expected_ty: &RcType,
) -> Result<Literal, TypeError> {
    use pikelet_syntax::core::Literal::*;
    use pikelet_syntax::FloatFormat::Dec as FloatDec;

    let ty = expected_ty;
    match *raw_literal {
        raw::Literal::String(_, ref val) if context.string() == ty => Ok(String(val.clone())),
        raw::Literal::Char(_, val) if context.char() == ty => Ok(Char(val)),

        // FIXME: overflow?
        raw::Literal::Int(_, v, format) if context.u8() == ty => Ok(U8(v as u8, format)),
        raw::Literal::Int(_, v, format) if context.u16() == ty => Ok(U16(v as u16, format)),
        raw::Literal::Int(_, v, format) if context.u32() == ty => Ok(U32(v as u32, format)),
        raw::Literal::Int(_, v, format) if context.u64() == ty => Ok(U64(v, format)),
        raw::Literal::Int(_, v, format) if context.s8() == ty => Ok(S8(v as i8, format)),
        raw::Literal::Int(_, v, format) if context.s16() == ty => Ok(S16(v as i16, format)),
        raw::Literal::Int(_, v, format) if context.s32() == ty => Ok(S32(v as i32, format)),
        raw::Literal::Int(_, v, format) if context.s64() == ty => Ok(S64(v as i64, format)),
        raw::Literal::Int(_, v, _) if context.f32() == ty => Ok(F32(v as f32, FloatDec)),
        raw::Literal::Int(_, v, _) if context.f64() == ty => Ok(F64(v as f64, FloatDec)),
        raw::Literal::Float(_, v, format) if context.f32() == ty => Ok(F32(v as f32, format)),
        raw::Literal::Float(_, v, format) if context.f64() == ty => Ok(F64(v, format)),

        _ => Err(TypeError::LiteralMismatch {
            literal_span: raw_literal.span(),
            found: raw_literal.clone(),
            expected: Box::new(context.resugar(expected_ty)),
        }),
    }
}

/// Synthesize the type of a literal, returning the elaborated literal and the
/// inferred type if successful
fn infer_literal(
    context: &Context,
    raw_literal: &raw::Literal,
) -> Result<(Literal, RcType), TypeError> {
    use pikelet_syntax::core::Literal::{Char, String};

    match *raw_literal {
        raw::Literal::String(_, ref val) => Ok((String(val.clone()), context.string().clone())),
        raw::Literal::Char(_, val) => Ok((Char(val), context.char().clone())),
        raw::Literal::Int(span, _, _) => Err(TypeError::AmbiguousIntLiteral { span }),
        raw::Literal::Float(span, _, _) => Err(TypeError::AmbiguousFloatLiteral { span }),
    }
}

/// Checks that a pattern is compatible with the given type, returning the
/// elaborated pattern and a vector of the declarations it introduced if successful
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
            let literal = check_literal(context, raw_literal, expected_ty)?;
            return Ok((RcPattern::from(Pattern::Literal(literal)), vec![]));
        },
        _ => {},
    }

    let (pattern, inferred_ty, declarations) = infer_pattern(context, raw_pattern)?;
    if is_subtype(context, &inferred_ty, expected_ty) {
        Ok((pattern, declarations))
    } else {
        Err(TypeError::Mismatch {
            span: raw_pattern.span(),
            found: Box::new(context.resugar(&inferred_ty)),
            expected: Box::new(context.resugar(expected_ty)),
        })
    }
}

/// Synthesize the type of a pattern, returning the elaborated pattern, the
/// inferred type, and a vector of the declarations it introduced if successful
pub fn infer_pattern(
    context: &Context,
    raw_pattern: &raw::RcPattern,
) -> Result<(RcPattern, RcType, Vec<(FreeVar<String>, RcType)>), TypeError> {
    match *raw_pattern.inner {
        raw::Pattern::Ann(ref raw_pattern, Embed(ref raw_ty)) => {
            let (ty, _) = infer_universe(context, raw_ty)?;
            let value_ty = nf_term(context, &ty)?;
            let (pattern, declarations) = check_pattern(context, raw_pattern, &value_ty)?;

            Ok((
                RcPattern::from(Pattern::Ann(pattern, Embed(ty))),
                value_ty,
                declarations,
            ))
        },
        raw::Pattern::Binder(span, ref binder) => Err(TypeError::BinderNeedsAnnotation {
            span,
            binder: binder.clone(),
        }),
        raw::Pattern::Var(span, Embed(ref var), shift) => match *var {
            Var::Free(ref free_var) => match context.get_declaration(free_var) {
                Some(ty) => {
                    let mut ty = ty.clone();
                    ty.shift_universes(shift);
                    let pattern = RcPattern::from(Pattern::Var(Embed(var.clone()), shift));

                    Ok((pattern, ty, vec![]))
                },
                None => Err(TypeError::UndefinedName {
                    span,
                    free_var: free_var.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                span: Some(raw_pattern.span()),
                var: var.clone(),
            }
            .into()),
        },
        raw::Pattern::Literal(ref literal) => {
            let (literal, ty) = infer_literal(context, literal)?;
            Ok((RcPattern::from(Pattern::Literal(literal)), ty, vec![]))
        },
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
            let literal = check_literal(context, raw_literal, expected_ty)?;
            return Ok(RcTerm::from(Term::Literal(literal)));
        },

        // C-LAM
        (&raw::Term::Lam(_, ref lam_scope), &Value::Pi(ref pi_scope)) => {
            let ((lam_name, Embed(lam_ann)), lam_body, (Binder(pi_name), Embed(pi_ann)), pi_body) =
                Scope::unbind2(lam_scope.clone(), pi_scope.clone());

            // Elaborate the hole, if it exists
            if let raw::Term::Hole(_) = *lam_ann.inner {
                let lam_ann = RcTerm::from(Term::from(&*pi_ann));
                let lam_body = {
                    let mut body_context = context.clone();
                    body_context.insert_declaration(pi_name, pi_ann);
                    check_term(&body_context, &lam_body, &pi_body)?
                };
                let lam_scope = Scope::new((lam_name, Embed(lam_ann)), lam_body);

                return Ok(RcTerm::from(Term::Lam(lam_scope)));
            }

            // TODO: We might want to optimise for this case, rather than
            // falling through to `infer` and unbinding again at I-LAM
        },
        (&raw::Term::Lam(_, _), _) => {
            return Err(TypeError::UnexpectedFunction {
                span: raw_term.span(),
                expected: Box::new(context.resugar(expected_ty)),
            });
        },

        // C-RECORD
        (&raw::Term::Record(span, ref raw_scope), &Value::RecordType(ref raw_ty_scope)) => {
            let (raw_fields, (), raw_ty_fields, ()) = {
                // Until Scope::unbind2 returns a Result.
                let found_size = raw_scope.unsafe_pattern.binders().len();
                let expected_size = raw_ty_scope.unsafe_pattern.binders().len();
                if found_size == expected_size {
                    Scope::unbind2(raw_scope.clone(), raw_ty_scope.clone())
                } else {
                    return Err(TypeError::RecordSizeMismatch {
                        span,
                        found_size: found_size as u64,
                        expected_size: expected_size as u64,
                    });
                }
            };

            let raw_fields = raw_fields.unnest();
            let raw_ty_fields = raw_ty_fields.unnest();

            // FIXME: Check that record is well-formed?
            let fields = {
                let mut mappings = Vec::with_capacity(raw_fields.len());
                let fields = <_>::zip(raw_fields.into_iter(), raw_ty_fields.into_iter())
                    .map(|(field, ty_field)| {
                        let (label, Binder(free_var), Embed(raw_expr)) = field;
                        let (ty_label, Binder(ty_free_var), Embed(ann)) = ty_field;

                        if label == ty_label {
                            let ann = nf_term(context, &ann.substs(&mappings))?;
                            let expr = check_term(context, &raw_expr, &ann)?;
                            mappings.push((ty_free_var, expr.clone()));
                            Ok((label, Binder(free_var), Embed(expr)))
                        } else {
                            Err(TypeError::LabelMismatch {
                                span,
                                found: label,
                                expected: ty_label,
                            })
                        }
                    })
                    .collect::<Result<_, _>>()?;

                Nest::new(fields)
            };

            return Ok(RcTerm::from(Term::Record(Scope::new(fields, ()))));
        },

        (&raw::Term::Case(_, ref raw_head, ref raw_clauses), _) => {
            let (head, head_ty) = infer_term(context, raw_head)?;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, declarations) = check_pattern(context, &raw_pattern, &head_ty)?;

                    let body = {
                        let mut body_context = context.clone();
                        for (free_var, ty) in declarations {
                            body_context.insert_declaration(free_var, ty);
                        }
                        check_term(&body_context, &raw_body, expected_ty)?
                    };

                    Ok(Scope::new(pattern, body))
                })
                .collect::<Result<_, TypeError>>()?;

            return Ok(RcTerm::from(Term::Case(head, clauses)));
        },

        (&raw::Term::Array(span, ref elems), _) => {
            return match context.array(expected_ty) {
                Some((len, elem_ty)) if len == elems.len() as u64 => {
                    let elems = elems
                        .iter()
                        .map(|elem| check_term(context, elem, elem_ty))
                        .collect::<Result<_, _>>()?;

                    Ok(RcTerm::from(Term::Array(elems)))
                },
                Some((len, _)) => Err(TypeError::ArrayLengthMismatch {
                    span,
                    found_len: elems.len() as u64,
                    expected_len: len,
                }),
                None => Err(TypeError::Internal(InternalError::Unimplemented {
                    span: Some(span),
                    message: "unexpected arguments to `Array`".to_owned(),
                })),
            }
        },

        (&raw::Term::Hole(span), _) => {
            let expected = Some(Box::new(context.resugar(expected_ty)));
            return Err(TypeError::UnableToElaborateHole { span, expected });
        },

        _ => {},
    }

    // C-CONV
    let (term, inferred_ty) = infer_term(context, raw_term)?;
    if is_subtype(context, &inferred_ty, expected_ty) {
        Ok(term)
    } else {
        Err(TypeError::Mismatch {
            span: raw_term.span(),
            found: Box::new(context.resugar(&inferred_ty)),
            expected: Box::new(context.resugar(expected_ty)),
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
            let value_ty = nf_term(context, &ty)?;
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
            let (literal, ty) = infer_literal(context, raw_literal)?;
            Ok((RcTerm::from(Term::Literal(literal)), ty))
        },

        // I-VAR
        raw::Term::Var(span, ref var, shift) => match *var {
            Var::Free(ref free_var) => match context.get_declaration(free_var) {
                Some(ty) => {
                    let mut ty = ty.clone();
                    ty.shift_universes(shift);

                    Ok((RcTerm::from(Term::Var(var.clone(), shift)), ty))
                },
                None => Err(TypeError::UndefinedName {
                    span,
                    free_var: free_var.clone(),
                }),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                span: Some(raw_term.span()),
                var: var.clone(),
            }
            .into()),
        },

        raw::Term::Import(_, name_span, ref name) => match context.get_import(name) {
            Some((_, ty)) => Ok((RcTerm::from(Term::Import(name.clone())), ty.clone())),
            None => Err(TypeError::UndefinedImport {
                span: name_span,
                name: name.clone(),
            }),
        },

        // I-PI
        raw::Term::Pi(_, ref raw_scope) => {
            let ((Binder(free_var), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            let (ann, ann_level) = infer_universe(context, &raw_ann)?;
            let (body, body_level) = {
                let ann = nf_term(context, &ann)?;
                let mut body_context = context.clone();
                body_context.insert_declaration(free_var.clone(), ann);
                infer_universe(&body_context, &raw_body)?
            };

            Ok((
                RcTerm::from(Term::Pi(Scope::new((Binder(free_var), Embed(ann)), body))),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        // I-LAM
        raw::Term::Lam(_, ref raw_scope) => {
            let ((Binder(free_var), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check for holes before entering to ensure we get a nice error
            if let raw::Term::Hole(_) = *raw_ann {
                return Err(TypeError::FunctionParamNeedsAnnotation {
                    param_span: ByteSpan::default(), // TODO: param.span(),
                    var_span: None,
                    name: free_var.clone(),
                });
            }

            let (lam_ann, _) = infer_universe(context, &raw_ann)?;
            let pi_ann = nf_term(context, &lam_ann)?;
            let (lam_body, pi_body) = {
                let mut body_context = context.clone();
                body_context.insert_declaration(free_var.clone(), pi_ann.clone());
                infer_term(&body_context, &raw_body)?
            };

            let lam_param = (Binder(free_var.clone()), Embed(lam_ann));
            let pi_param = (Binder(free_var.clone()), Embed(pi_ann));

            Ok((
                RcTerm::from(Term::Lam(Scope::new(lam_param, lam_body))),
                RcValue::from(Value::Pi(Scope::new(pi_param, pi_body))),
            ))
        },

        // I-LET
        raw::Term::Let(_, ref raw_scope) => {
            let (raw_fields, raw_body) = raw_scope.clone().unbind();

            let (term, ty) = {
                let mut context = context.clone();
                let bindings = raw_fields
                    .unnest()
                    .into_iter()
                    .map(|(Binder(free_var), Embed((raw_ann, raw_term)))| {
                        let (term, ann, ann_value) = if let raw::Term::Hole(_) = *raw_ann {
                            let (term, ann_value) = infer_term(&context, &raw_term)?;
                            (term, RcTerm::from(&*ann_value.inner), ann_value)
                        } else {
                            let (ann, _) = infer_universe(&context, &raw_ann)?;
                            let ann_value = nf_term(&context, &ann)?;
                            (check_term(&context, &raw_term, &ann_value)?, ann, ann_value)
                        };

                        context.insert_definition(free_var.clone(), term.clone());
                        context.insert_declaration(free_var.clone(), ann_value);

                        Ok((Binder(free_var), Embed((ann, term))))
                    })
                    .collect::<Result<_, TypeError>>()?;

                let (body, ty) = infer_term(&context, &raw_body)?;
                let term = RcTerm::from(Term::Let(Scope::new(Nest::new(bindings), body)));

                (term, ty)
            };

            Ok((term, ty))
        },

        // I-APP
        raw::Term::App(ref raw_head, ref raw_arg) => {
            let (head, head_ty) = infer_term(context, raw_head)?;

            match *head_ty {
                Value::Pi(ref scope) => {
                    let ((Binder(free_var), Embed(ann)), body) = scope.clone().unbind();

                    let arg = check_term(context, raw_arg, &ann)?;
                    let body = nf_term(context, &body.substs(&[(free_var, arg.clone())]))?;

                    Ok((RcTerm::from(Term::App(head, arg)), body))
                },
                _ => Err(TypeError::ArgAppliedToNonFunction {
                    fn_span: raw_head.span(),
                    arg_span: raw_arg.span(),
                    found: Box::new(context.resugar(&head_ty)),
                }),
            }
        },

        // I-RECORD-TYPE, I-EMPTY-RECORD-TYPE
        raw::Term::RecordType(_, ref raw_scope) => {
            let (raw_fields, ()) = raw_scope.clone().unbind();
            let mut max_level = Level(0);

            // FIXME: Check that record is well-formed?
            let fields = {
                let mut context = context.clone();
                raw_fields
                    .unnest()
                    .into_iter()
                    .map(|(label, Binder(free_var), Embed(raw_ann))| {
                        let (ann, ann_level) = infer_universe(&context, &raw_ann)?;
                        let nf_ann = nf_term(&context, &ann)?;

                        max_level = cmp::max(max_level, ann_level);
                        context.insert_declaration(free_var.clone(), nf_ann);

                        Ok((label, Binder(free_var), Embed(ann)))
                    })
                    .collect::<Result<_, TypeError>>()?
            };

            Ok((
                RcTerm::from(Term::RecordType(Scope::new(Nest::new(fields), ()))),
                RcValue::from(Value::Universe(max_level)),
            ))
        },

        // I-RECORD, I-EMPTY-RECORD
        raw::Term::Record(_, ref raw_scope) => {
            let (raw_fields, ()) = raw_scope.clone().unbind();
            let raw_fields = raw_fields.unnest();

            let mut fields = Vec::with_capacity(raw_fields.len());
            let mut ty_fields = Vec::with_capacity(raw_fields.len());

            // FIXME: error on duplicate field names
            {
                let mut ty_mappings = Vec::with_capacity(raw_fields.len());
                for (label, Binder(free_var), Embed(raw_term)) in raw_fields {
                    let (term, term_ty) = infer_term(context, &raw_term)?;
                    let term_ty = nf_term(context, &term_ty.substs(&ty_mappings))?;

                    fields.push((label.clone(), Binder(free_var.clone()), Embed(term.clone())));
                    ty_fields.push((label, Binder(free_var.clone()), Embed(term_ty)));
                    ty_mappings.push((free_var, term));
                }
            }

            Ok((
                RcTerm::from(Term::Record(Scope::new(Nest::new(fields), ()))),
                RcValue::from(Value::RecordType(Scope::new(Nest::new(ty_fields), ()))),
            ))
        },

        // I-PROJ
        raw::Term::Proj(_, ref expr, label_span, ref label, shift) => {
            let (expr, ty) = infer_term(context, expr)?;

            if let Value::RecordType(ref scope) = *ty.inner {
                let (fields, ()) = scope.clone().unbind();
                let mut mappings = vec![];

                for (current_label, Binder(free_var), Embed(current_ann)) in fields.unnest() {
                    if current_label == *label {
                        let mut ty = nf_term(context, &current_ann.substs(&mappings))?;
                        ty.shift_universes(shift);

                        return Ok((RcTerm::from(Term::Proj(expr, current_label, shift)), ty));
                    } else {
                        mappings.push((
                            free_var,
                            // NOTE: Not sure if we should be shifting here...
                            RcTerm::from(Term::Proj(expr.clone(), current_label, shift)),
                        ));
                    }
                }
            }

            Err(TypeError::NoFieldInType {
                label_span,
                expected_label: label.clone(),
                found: Box::new(context.resugar(&ty)),
            })
        },

        // I-CASE
        raw::Term::Case(span, ref raw_head, ref raw_clauses) => {
            let (head, head_ty) = infer_term(context, raw_head)?;
            let mut ty = None;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, declarations) = check_pattern(context, &raw_pattern, &head_ty)?;

                    let (body, body_ty) = {
                        let mut body_context = context.clone();
                        for (free_var, ty) in declarations {
                            body_context.insert_declaration(free_var, ty);
                        }
                        infer_term(&body_context, &raw_body)?
                    };

                    match ty {
                        None => ty = Some(body_ty),
                        // FIXME: use common subtype?
                        Some(ref ty) if RcValue::term_eq(&body_ty, ty) => {},
                        Some(ref ty) => {
                            return Err(TypeError::Mismatch {
                                span: raw_body.span(),
                                found: Box::new(context.resugar(&body_ty)),
                                expected: Box::new(context.resugar(ty)),
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
