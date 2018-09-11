use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};

use syntax::core::{
    Head, Literal, Neutral, Pattern, RcNeutral, RcPattern, RcTerm, RcValue, Term, Value,
};

use semantics::errors::InternalError;
use semantics::DefinitionEnv;

/// Reduce a term to its normal form
pub fn nf_term<Env>(env: &Env, term: &RcTerm) -> Result<RcValue, InternalError>
where
    Env: DefinitionEnv,
{
    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => nf_term(env, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var) => {
            match *var {
                Var::Free(ref name) => match env.get_definition(name) {
                    Some(term) => nf_term(env, term),
                    None => Ok(RcValue::from(Value::from(var.clone()))),
                },

                // We should always be substituting bound variables with fresh
                // variables when entering scopes using `unbind`, so if we've
                // encountered one here this is definitely a bug!
                Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                    span: None,
                    var: var.clone(),
                }),
            }
        },

        Term::Extern(ref name, ref ty) => Ok(RcValue::from(Value::from(Neutral::Head(
            Head::Extern(name.clone(), nf_term(env, ty)?),
        )))),

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
                        Neutral::Head(Head::Extern(ref name, _)) => {
                            spine.push(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully nfd
                            if let Some(prim) = env.get_extern_definition(name) {
                                if prim.arity == spine.len() && spine.iter().all(|arg| arg.is_nf())
                                {
                                    match (prim.interpretation)(spine) {
                                        Ok(value) => return Ok(value),
                                        Err(()) => unimplemented!("proper error"),
                                    }
                                }
                            }
                        },
                        Neutral::Head(Head::Var(_))
                        | Neutral::If(_, _, _)
                        | Neutral::Proj(_, _)
                        | Neutral::Case(_, _) => spine.push(arg),
                    }

                    Ok(RcValue::from(Value::Neutral(neutral.clone(), spine)))
                },
                _ => Err(InternalError::ArgumentAppliedToNonFunction),
            }
        },

        // E-LET
        Term::Let(ref scope) => {
            let ((Binder(free_var), Embed(bind)), body) = scope.clone().unbind();
            nf_term(env, &body.substs(&[(free_var, bind.clone())]))
        },

        // E-IF, E-IF-TRUE, E-IF-FALSE
        Term::If(ref cond, ref if_true, ref if_false) => {
            let value_cond = nf_term(env, cond)?;

            match *value_cond {
                Value::Literal(Literal::Bool(true)) => nf_term(env, if_true),
                Value::Literal(Literal::Bool(false)) => nf_term(env, if_false),
                Value::Neutral(ref cond, ref spine) => Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::If(
                        cond.clone(),
                        nf_term(env, if_true)?,
                        nf_term(env, if_false)?,
                    )),
                    spine.clone(),
                ))),
                _ => Err(InternalError::ExpectedBoolExpr),
            }
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
        Term::Proj(ref expr, ref label) => {
            match *nf_term(env, expr)? {
                Value::Neutral(ref neutral, ref spine) => {
                    return Ok(RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Proj(neutral.clone(), label.clone())),
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
                    if let Some(mappings) = match_value(&pattern, &head) {
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
