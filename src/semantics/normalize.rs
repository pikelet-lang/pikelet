use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};

use syntax::core::{Head, Neutral, Pattern, RcNeutral, RcPattern, RcTerm, RcValue, Term, Value};

use semantics::{Import, InternalError, TcEnv};

/// Reduce a term to its normal form
pub fn nf_term(env: &TcEnv, term: &RcTerm) -> Result<RcValue, InternalError> {
    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => nf_term(env, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var, shift) => match *var {
            Var::Free(ref name) => match env.get_definition(name) {
                Some(term) => {
                    let mut value = nf_term(env, term)?;
                    value.shift_universes(shift);
                    Ok(value)
                },
                None => Ok(RcValue::from(Value::var(var.clone(), shift))),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                span: None,
                var: var.clone(),
            }),
        },

        Term::Import(ref name) => match env.get_import_definition(name) {
            Some(Import::Term(ref term)) => nf_term(env, term),
            Some(Import::Prim(ref interpretation)) => match interpretation(&[]) {
                Some(value) => Ok(value),
                None => Ok(RcValue::from(Value::from(Neutral::Head(Head::Import(
                    name.clone(),
                ))))),
            },
            None => Ok(RcValue::from(Value::from(Neutral::Head(Head::Import(
                name.clone(),
            ))))),
        },

        // E-PI
        Term::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Pi(Scope::new(
                (name, Embed(nf_term(env, &ann)?)),
                nf_term(env, &body)?,
            ))))
        },

        // E-LAM
        Term::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Lam(Scope::new(
                (name, Embed(nf_term(env, &ann)?)),
                nf_term(env, &body)?,
            ))))
        },

        // E-APP
        Term::App(ref head, ref arg) => {
            match *nf_term(env, head)?.inner {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((Binder(free_var), Embed(_)), body) = scope.clone().unbind();
                    nf_term(env, &body.substs(&[(free_var, arg.clone())]))
                },
                Value::Neutral(ref neutral, ref spine) => {
                    let arg = nf_term(env, arg)?;
                    let mut spine = spine.clone();

                    match *neutral.inner {
                        Neutral::Head(Head::Import(ref name)) => {
                            spine.push(arg);

                            match env.get_import_definition(name) {
                                Some(Import::Term(ref term)) => {
                                    // nf_term(env, term)
                                    unimplemented!("import applications")
                                },
                                Some(Import::Prim(ref interpretation)) => {
                                    match interpretation(&spine) {
                                        Some(value) => return Ok(value),
                                        None => {},
                                    }
                                },
                                None => {},
                            }
                        },
                        Neutral::Head(Head::Var(..)) | Neutral::Proj(..) | Neutral::Case(..) => {
                            spine.push(arg)
                        },
                    }

                    Ok(RcValue::from(Value::Neutral(neutral.clone(), spine)))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction),
            }
        },

        // E-LET
        Term::Let(ref scope) => {
            let (bindings, body) = scope.clone().unbind();
            let mut mappings = Vec::with_capacity(bindings.unsafe_patterns.len());

            for (Binder(free_var), Embed((_, term))) in bindings.unnest() {
                let value = nf_term(env, &term.substs(&mappings))?;
                mappings.push((free_var, RcTerm::from(&*value.inner)));
            }

            nf_term(env, &body.substs(&mappings))
        },

        // E-RECORD-TYPE, E-EMPTY-RECORD-TYPE
        Term::RecordType(ref scope) => {
            let (fields, ()) = scope.clone().unbind();
            let fields = Nest::new(
                fields
                    .unnest()
                    .into_iter()
                    .map(|(label, binder, Embed(ann))| {
                        Ok((label, binder, Embed(nf_term(env, &ann)?)))
                    }).collect::<Result<_, _>>()?,
            );

            Ok(RcValue::from(Value::RecordType(Scope::new(fields, ()))))
        },

        // E-RECORD, E-EMPTY-RECORD
        Term::Record(ref scope) => {
            let (fields, ()) = scope.clone().unbind();
            let fields = Nest::new(
                fields
                    .unnest()
                    .into_iter()
                    .map(|(label, binder, Embed(term))| {
                        Ok((label, binder, Embed(nf_term(env, &term)?)))
                    }).collect::<Result<_, _>>()?,
            );

            Ok(RcValue::from(Value::Record(Scope::new(fields, ()))))
        },

        // E-PROJ
        Term::Proj(ref expr, ref label, shift) => {
            match *nf_term(env, expr)? {
                Value::Neutral(ref neutral, ref spine) => {
                    return Ok(RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Proj(neutral.clone(), label.clone(), shift)),
                        spine.clone(),
                    )));
                },
                Value::Record(ref scope) => {
                    let (fields, ()) = scope.clone().unbind();

                    // FIXME: mappings?
                    for (current_label, _, Embed(current_expr)) in fields.unnest() {
                        if current_label == *label {
                            return Ok(current_expr.clone());
                        }
                    }
                },
                _ => {},
            }

            Err(InternalError::ProjectedOnNonExistentField {
                label: label.clone(),
            })
        },

        // E-CASE
        Term::Case(ref head, ref clauses) => {
            let head = nf_term(env, head)?;

            if let Value::Neutral(ref neutral, ref spine) = *head {
                Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::Case(
                        neutral.clone(),
                        clauses
                            .iter()
                            .map(|clause| {
                                let (pattern, body) = clause.clone().unbind();
                                Ok(Scope::new(pattern, nf_term(env, &body)?))
                            }).collect::<Result<_, _>>()?,
                    )),
                    spine.clone(),
                )))
            } else {
                for clause in clauses {
                    let (pattern, body) = clause.clone().unbind();
                    if let Some(mappings) = match_value(env, &pattern, &head)? {
                        let mappings = mappings
                            .into_iter()
                            .map(|(free_var, value)| (free_var, RcTerm::from(&*value.inner)))
                            .collect::<Vec<_>>();
                        return nf_term(env, &body.substs(&mappings));
                    }
                }
                Err(InternalError::NoPatternsApplicable)
            }
        },

        // E-ARRAY
        Term::Array(ref elems) => Ok(RcValue::from(Value::Array(
            elems
                .iter()
                .map(|elem| nf_term(env, elem))
                .collect::<Result<_, _>>()?,
        ))),
    }
}

/// If the pattern matches the value, this function returns the substitutions
/// needed to apply the pattern to some body expression
pub fn match_value(
    env: &TcEnv,
    pattern: &RcPattern,
    value: &RcValue,
) -> Result<Option<Vec<(FreeVar<String>, RcValue)>>, InternalError> {
    match (&*pattern.inner, &*value.inner) {
        (&Pattern::Binder(Binder(ref free_var)), _) => {
            Ok(Some(vec![(free_var.clone(), value.clone())]))
        },
        (&Pattern::Var(Embed(Var::Free(ref free_var)), _), _) => {
            match env.get_definition(free_var).map(|term| nf_term(env, term)) {
                Some(Ok(ref term)) if term == value => Ok(Some(vec![])),
                Some(Ok(_)) | None => Ok(None),
                Some(Err(err)) => Err(err),
            }
        },
        (&Pattern::Literal(ref pattern_lit), &Value::Literal(ref value_lit))
            if pattern_lit == value_lit =>
        {
            Ok(Some(vec![]))
        },
        (_, _) => Ok(None),
    }
}
