//! The semantics of the language
//!
//! Here we define the rules of normalization, type checking, and type inference.
//!
//! For more information, check out the theory appendix of the Pikelet book.

use codespan::ByteSpan;
use moniker::{Binder, BoundTerm, Embed, FreeVar, Scope, Var};

use syntax::core::{
    Item, Literal, Module, Pattern, RcPattern, RcTerm, RcType, RcValue, Term, Type, Value,
};
use syntax::raw;
use syntax::translation::Resugar;
use syntax::Level;

mod env;
mod errors;
mod normalize;
mod prim;
#[cfg(test)]
mod tests;

pub use self::env::TcEnv;
pub use self::errors::{InternalError, TypeError};
pub use self::normalize::{match_value, nf_term};
pub use self::prim::{PrimEnv, PrimFn};

/// Type check and elaborate a module
pub fn check_module(tc_env: &TcEnv, raw_module: &raw::Module) -> Result<Module, TypeError> {
    use im::HashMap;

    #[derive(Clone)]
    pub enum ForwardDecl {
        Pending(ByteSpan, RcTerm),
        Defined(ByteSpan),
    }

    // Declarations that may be waiting to be defined
    let mut forward_declarations = HashMap::new();
    let mut tc_env = tc_env.clone();
    // The elaborated items, pre-allocated to improve performance
    let mut items = Vec::with_capacity(raw_module.items.len());

    // Iterate through the items in the module, checking each in turn
    for raw_item in &raw_module.items {
        match *raw_item {
            raw::Item::Declaration(declaration_span, ref free_var, ref raw_ty) => {
                // Ensure that this declaration has not already been seen
                match forward_declarations.get(free_var).cloned() {
                    // There's already a definition associated with this name -
                    // we can't add a new declaration for it!
                    Some(ForwardDecl::Defined(definition_span)) => {
                        return Err(TypeError::DeclarationFollowedDefinition {
                            definition_span,
                            declaration_span,
                            free_var: free_var.clone(),
                        });
                    },
                    // There's a declaration  for this name already pending - we
                    // can't add a new one!
                    Some(ForwardDecl::Pending(original_span, _)) => {
                        return Err(TypeError::DuplicateDeclarations {
                            original_span,
                            duplicate_span: declaration_span,
                            free_var: free_var.clone(),
                        });
                    },
                    // No previous declaration for this name was seen, so we can
                    // go-ahead and type check, elaborate, and then add it to
                    // the context
                    None => {
                        // Ensure that the declaration's type annotation is actually a type
                        let (ty, _) = infer_universe(&tc_env, raw_ty)?;
                        // Remember the declaration for when we get to a subsequent definition
                        let declaration = ForwardDecl::Pending(declaration_span, ty.clone());
                        forward_declarations.insert(free_var.clone(), declaration);
                        // Add the declaration to the elaborated items
                        items.push(Item::Declaration(free_var.clone(), ty));
                    },
                }
            },

            raw::Item::Definition(span, ref free_var, ref raw_term) => {
                let (term, ty) = match forward_declarations.get(free_var).cloned() {
                    // This declaration was already given a definition, so this
                    // is an error!
                    //
                    // NOTE: Some languages (eg. Haskell, Agda, Idris, and
                    // Erlang) turn duplicate definitions into case matches.
                    // Languages like Elm don't. What should we do here?
                    Some(ForwardDecl::Defined(original_span)) => {
                        return Err(TypeError::DuplicateDefinitions {
                            original_span,
                            duplicate_span: span,
                            free_var: free_var.clone(),
                        });
                    },
                    // We found a prior declaration, so lets use it as a basis
                    // for checking the definition
                    Some(ForwardDecl::Pending(_, ty)) => {
                        let ty = nf_term(&tc_env, &ty)?;
                        (check_term(&tc_env, &raw_term, &ty)?, ty)
                    },
                    // No prior declaration was found, so try to infer the type
                    // from the given definition alone
                    None => infer_term(&tc_env, &raw_term)?,
                };

                // We must not remove this from the list of pending
                // declarations, lest we encounter another declaration or
                // definition of the same name later on!
                forward_declarations.insert(free_var.clone(), ForwardDecl::Defined(span));
                // Add the declaration and definition to the environment,
                // allowing them to be used in later type checking
                tc_env.declarations.insert(free_var.clone(), ty);
                tc_env.definitions.insert(free_var.clone(), term.clone());
                // Add the definition to the elaborated items
                items.push(Item::Definition(free_var.clone(), term));
            },
        }
    }

    Ok(Module { items })
}

/// Ensures that the given term is a universe, returning the level of that
/// universe and its elaborated form.
fn infer_universe(tc_env: &TcEnv, raw_term: &raw::RcTerm) -> Result<(RcTerm, Level), TypeError> {
    let (term, ty) = infer_term(tc_env, raw_term)?;
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
    match expected_ty.global_app() {
        Some((name, spine)) if spine.is_empty() => {
            match (raw_literal, name) {
                (&raw::Literal::String(_, ref val), "String") => {
                    return Ok(Literal::String(val.clone()));
                },
                (&raw::Literal::Char(_, val), "Char") => return Ok(Literal::Char(val)),

                // FIXME: overflow?
                (&raw::Literal::Int(_, val), "U8") => return Ok(Literal::U8(val as u8)),
                (&raw::Literal::Int(_, val), "U16") => return Ok(Literal::U16(val as u16)),
                (&raw::Literal::Int(_, val), "U32") => return Ok(Literal::U32(val as u32)),
                (&raw::Literal::Int(_, val), "U64") => return Ok(Literal::U64(val)),
                (&raw::Literal::Int(_, val), "I8") => return Ok(Literal::I8(val as i8)),
                (&raw::Literal::Int(_, val), "I16") => return Ok(Literal::I16(val as i16)),
                (&raw::Literal::Int(_, val), "I32") => return Ok(Literal::I32(val as i32)),
                (&raw::Literal::Int(_, val), "I64") => return Ok(Literal::I64(val as i64)),
                (&raw::Literal::Int(_, val), "F32") => return Ok(Literal::F32(val as f32)),
                (&raw::Literal::Int(_, val), "F64") => return Ok(Literal::F64(val as f64)),
                (&raw::Literal::Float(_, val), "F32") => return Ok(Literal::F32(val as f32)),
                (&raw::Literal::Float(_, val), "F64") => return Ok(Literal::F64(val)),

                _ => {},
            }
        },
        Some(_) | None => {},
    }

    Err(TypeError::LiteralMismatch {
        literal_span: raw_literal.span(),
        found: raw_literal.clone(),
        expected: Box::new(expected_ty.resugar()),
    })
}

/// Synthesize the type of a literal, returning the elaborated literal and the
/// inferred type if successful
fn infer_literal(raw_literal: &raw::Literal) -> Result<(Literal, RcType), TypeError> {
    match *raw_literal {
        raw::Literal::String(_, ref value) => Ok((
            Literal::String(value.clone()),
            RcValue::from(Value::global("String")),
        )),
        raw::Literal::Char(_, value) => {
            Ok((Literal::Char(value), RcValue::from(Value::global("Char"))))
        },
        raw::Literal::Int(span, _) => Err(TypeError::AmbiguousIntLiteral { span }),
        raw::Literal::Float(span, _) => Err(TypeError::AmbiguousFloatLiteral { span }),
    }
}

/// Checks that a pattern is compatible with the given type, returning the
/// elaborated pattern and a vector of the declarations it introduced if successful
pub fn check_pattern(
    tc_env: &TcEnv,
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

    let (pattern, inferred_ty, declarations) = infer_pattern(tc_env, raw_pattern)?;
    if Type::term_eq(&inferred_ty, expected_ty) {
        Ok((pattern, declarations))
    } else {
        Err(TypeError::Mismatch {
            span: raw_pattern.span(),
            found: Box::new(inferred_ty.resugar()),
            expected: Box::new(expected_ty.resugar()),
        })
    }
}

/// Synthesize the type of a pattern, returning the elaborated pattern, the
/// inferred type, and a vector of the declarations it introduced if successful
pub fn infer_pattern(
    tc_env: &TcEnv,
    raw_pattern: &raw::RcPattern,
) -> Result<(RcPattern, RcType, Vec<(FreeVar<String>, RcType)>), TypeError> {
    match *raw_pattern.inner {
        raw::Pattern::Ann(ref raw_pattern, Embed(ref raw_ty)) => {
            let (ty, _) = infer_universe(tc_env, raw_ty)?;
            let value_ty = nf_term(tc_env, &ty)?;
            let (pattern, declarations) = check_pattern(tc_env, raw_pattern, &value_ty)?;

            Ok((
                RcPattern::from(Pattern::Ann(pattern, Embed(ty))),
                value_ty,
                declarations,
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
    tc_env: &TcEnv,
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
                let lam_body = {
                    let mut body_tc_env = tc_env.clone();
                    body_tc_env.declarations.insert(pi_name, pi_ann);
                    check_term(&body_tc_env, &lam_body, &pi_body)?
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
                expected: Box::new(expected_ty.resugar()),
            });
        },

        // C-IF
        (&raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false), _) => {
            let bool_ty = RcValue::from(Value::global("Bool"));
            let cond = check_term(tc_env, raw_cond, &bool_ty)?;
            let if_true = check_term(tc_env, raw_if_true, expected_ty)?;
            let if_false = check_term(tc_env, raw_if_false, expected_ty)?;

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
                let expr = check_term(tc_env, &raw_expr, &ann)?;
                let ty_body = nf_term(tc_env, &ty_body.substs(&[(ty_binder.0, expr.clone())]))?;
                let body = check_term(tc_env, &raw_body, &ty_body)?;

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
            let (head, head_ty) = infer_term(tc_env, raw_head)?;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, declarations) = check_pattern(tc_env, &raw_pattern, &head_ty)?;

                    let mut body_tc_env = tc_env.clone();
                    body_tc_env.declarations.extend(declarations);
                    let body = check_term(&body_tc_env, &raw_body, expected_ty)?;

                    Ok(Scope::new(pattern, body))
                }).collect::<Result<_, TypeError>>()?;

            return Ok(RcTerm::from(Term::Case(head, clauses)));
        },

        (&raw::Term::Array(span, ref elems), ty) => match ty.global_app() {
            Some(("Array", spine)) if spine.len() == 2 => {
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
                        .map(|elem| check_term(tc_env, elem, elem_ty))
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
    let (term, inferred_ty) = infer_term(tc_env, raw_term)?;
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
pub fn infer_term(tc_env: &TcEnv, raw_term: &raw::RcTerm) -> Result<(RcTerm, RcType), TypeError> {
    use std::cmp;

    match *raw_term.inner {
        //  I-ANN
        raw::Term::Ann(ref raw_expr, ref raw_ty) => {
            let (ty, _) = infer_universe(tc_env, raw_ty)?;
            let value_ty = nf_term(tc_env, &ty)?;
            let expr = check_term(tc_env, raw_expr, &value_ty)?;

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
        raw::Term::Var(span, ref var) => match *var {
            Var::Free(ref free_var) => match tc_env.declarations.get(free_var) {
                Some(ty) => Ok((RcTerm::from(Term::Var(var.clone())), ty.clone())),
                None => Err(TypeError::NotYetDefined {
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
            }.into()),
        },

        raw::Term::Extern(_, name_span, ref name, _) if tc_env.primitives.get(name).is_none() => {
            Err(TypeError::UndefinedExternName {
                span: name_span,
                name: name.clone(),
            })
        },

        raw::Term::Extern(_, _, ref name, ref raw_ty) => {
            let (ty, _) = infer_universe(tc_env, raw_ty)?;
            let value_ty = nf_term(tc_env, &ty)?;
            Ok((RcTerm::from(Term::Extern(name.clone(), ty)), value_ty))
        },

        raw::Term::Global(span, ref name) => match tc_env.globals.get(name.as_str()) {
            Some((_, ref ty)) => Ok((RcTerm::from(Term::global(name.clone())), ty.clone())),
            None => Err(TypeError::UndefinedName {
                span,
                name: name.clone(),
            }),
        },

        // I-PI
        raw::Term::Pi(_, ref raw_scope) => {
            let ((Binder(free_var), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            let (ann, ann_level) = infer_universe(tc_env, &raw_ann)?;
            let (body, body_level) = {
                let ann = nf_term(tc_env, &ann)?;
                let mut body_tc_env = tc_env.clone();
                body_tc_env.declarations.insert(free_var.clone(), ann);
                infer_universe(&body_tc_env, &raw_body)?
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

            let (lam_ann, _) = infer_universe(tc_env, &raw_ann)?;
            let pi_ann = nf_term(tc_env, &lam_ann)?;
            let (lam_body, pi_body) = {
                let mut body_tc_env = tc_env.clone();
                body_tc_env
                    .declarations
                    .insert(free_var.clone(), pi_ann.clone());
                infer_term(&body_tc_env, &raw_body)?
            };

            let lam_param = (Binder(free_var.clone()), Embed(lam_ann));
            let pi_param = (Binder(free_var.clone()), Embed(pi_ann));

            Ok((
                RcTerm::from(Term::Lam(Scope::new(lam_param, lam_body))),
                RcValue::from(Value::Pi(Scope::new(pi_param, pi_body))),
            ))
        },

        // I-IF
        raw::Term::If(_, ref raw_cond, ref raw_if_true, ref raw_if_false) => {
            let bool_ty = RcValue::from(Value::global("Bool"));
            let cond = check_term(tc_env, raw_cond, &bool_ty)?;
            let (if_true, ty) = infer_term(tc_env, raw_if_true)?;
            let if_false = check_term(tc_env, raw_if_false, &ty)?;

            Ok((RcTerm::from(Term::If(cond, if_true, if_false)), ty))
        },

        // I-APP
        raw::Term::App(ref raw_head, ref raw_arg) => {
            let (head, head_ty) = infer_term(tc_env, raw_head)?;

            match *head_ty {
                Value::Pi(ref scope) => {
                    let ((Binder(free_var), Embed(ann)), body) = scope.clone().unbind();

                    let arg = check_term(tc_env, raw_arg, &ann)?;
                    let body = nf_term(tc_env, &body.substs(&[(free_var, arg.clone())]))?;

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
            let ((label, Binder(free_var), Embed(raw_ann)), raw_body) = raw_scope.clone().unbind();

            // Check that rest of record is well-formed?
            // Might be able to skip that for now, because there's no way to
            // express ill-formed records in the concrete syntax...

            let (ann, ann_level) = infer_universe(tc_env, &raw_ann)?;
            let (body, body_level) = {
                let ann = nf_term(tc_env, &ann)?;
                let mut body_tc_env = tc_env.clone();
                body_tc_env.declarations.insert(free_var.clone(), ann);
                infer_universe(&body_tc_env, &raw_body)?
            };

            let scope = Scope::new((label, Binder(free_var), Embed(ann)), body);

            Ok((
                RcTerm::from(Term::RecordType(scope)),
                RcValue::from(Value::Universe(cmp::max(ann_level, body_level))),
            ))
        },

        raw::Term::Record(span, _) => Err(TypeError::AmbiguousRecord { span }),

        // I-EMPTY-RECORD-TYPE
        raw::Term::RecordTypeEmpty(_) => Ok((
            RcTerm::from(Term::RecordTypeEmpty),
            RcValue::from(Value::universe(0)),
        )),

        // I-EMPTY-RECORD
        raw::Term::RecordEmpty(_) => Ok((
            RcTerm::from(Term::RecordEmpty),
            RcValue::from(Value::RecordTypeEmpty),
        )),

        // I-PROJ
        raw::Term::Proj(_, ref expr, label_span, ref label) => {
            let (expr, ty) = infer_term(tc_env, expr)?;

            let mut mappings = vec![];
            let mut current_scope = ty.record_ty();
            let mut field_ty = None;

            while let Some(scope) = current_scope {
                let ((current_label, current_binder, Embed(current_field_ty)), body) =
                    scope.unbind();

                if current_label == *label {
                    field_ty = Some(current_field_ty.substs(&mappings));
                    break;
                }

                let proj = RcTerm::from(Term::Proj(expr.clone(), current_label));
                mappings.push((current_binder.0, proj));
                current_scope = body.record_ty();
            }

            match field_ty {
                Some(field_ty) => Ok((
                    RcTerm::from(Term::Proj(expr, label.clone())),
                    nf_term(tc_env, &field_ty)?,
                )),
                None => Err(TypeError::NoFieldInType {
                    label_span,
                    expected_label: label.clone(),
                    found: Box::new(ty.resugar()),
                }),
            }
        },

        // I-CASE
        raw::Term::Case(span, ref raw_head, ref raw_clauses) => {
            let (head, head_ty) = infer_term(tc_env, raw_head)?;
            let mut ty = None;

            // TODO: ensure that patterns are exhaustive
            let clauses = raw_clauses
                .iter()
                .map(|raw_clause| {
                    let (raw_pattern, raw_body) = raw_clause.clone().unbind();
                    let (pattern, declarations) = check_pattern(tc_env, &raw_pattern, &head_ty)?;

                    let (body, body_ty) = {
                        let mut body_tc_env = tc_env.clone();
                        body_tc_env.declarations.extend(declarations);
                        infer_term(&body_tc_env, &raw_body)?
                    };

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
                }).collect::<Result<_, TypeError>>()?;

            match ty {
                Some(ty) => Ok((RcTerm::from(Term::Case(head, clauses)), ty)),
                None => Err(TypeError::AmbiguousEmptyCase { span }),
            }
        },

        raw::Term::Array(span, _) => Err(TypeError::AmbiguousArrayLiteral { span }),
    }
}
