use moniker::{Binder, Embed, FreeVar, Scope, Var};

use syntax::core::{
    Head, Literal, Neutral, Pattern, RcNeutral, RcPattern, RcTerm, RcValue, Term, Value,
};

use semantics::errors::InternalError;
use semantics::TcEnv;

/// Reduce a term to its normal form
pub fn nf_term(tc_env: &TcEnv, term: &RcTerm) -> Result<RcValue, InternalError> {
    match *term.inner {
        // E-ANN
        Term::Ann(ref expr, _) => nf_term(tc_env, expr),

        // E-TYPE
        Term::Universe(level) => Ok(RcValue::from(Value::Universe(level))),

        Term::Literal(ref lit) => Ok(RcValue::from(Value::Literal(lit.clone()))),

        // E-VAR, E-VAR-DEF
        Term::Var(ref var) => match *var {
            Var::Free(ref name) => match tc_env.definitions.get(name) {
                Some(term) => nf_term(tc_env, term),
                None => Ok(RcValue::from(Value::from(var.clone()))),
            },

            // We should always be substituting bound variables with fresh
            // variables when entering scopes using `unbind`, so if we've
            // encountered one here this is definitely a bug!
            Var::Bound(_) => Err(InternalError::UnexpectedBoundVar {
                span: None,
                var: var.clone(),
            }),
        },

        Term::Extern(ref name, ref ty) => Ok(RcValue::from(Value::from(Neutral::Head(
            Head::Extern(name.clone(), nf_term(tc_env, ty)?),
        )))),

        Term::Global(ref name) => match tc_env.globals.get(name.as_str()) {
            Some(&(Some(ref value), _)) => Ok(value.clone()),
            Some(&(None, _)) | None => Ok(RcValue::from(Value::global(name.clone()))),
        },

        // E-PI
        Term::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Pi(Scope::new(
                (name, Embed(nf_term(tc_env, &ann)?)),
                nf_term(tc_env, &body)?,
            ))))
        },

        // E-LAM
        Term::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = scope.clone().unbind();

            Ok(RcValue::from(Value::Lam(Scope::new(
                (name, Embed(nf_term(tc_env, &ann)?)),
                nf_term(tc_env, &body)?,
            ))))
        },

        // E-APP
        Term::App(ref head, ref arg) => {
            match *nf_term(tc_env, head)?.inner {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((Binder(free_var), Embed(_)), body) = scope.clone().unbind();
                    nf_term(tc_env, &body.substs(&[(free_var, arg.clone())]))
                },
                Value::Neutral(ref neutral, ref spine) => {
                    let arg = nf_term(tc_env, arg)?;
                    let mut spine = spine.clone();

                    match *neutral.inner {
                        Neutral::Head(Head::Extern(ref name, _)) => {
                            spine.push_back(arg);

                            // Apply the arguments to primitive definitions if the number of
                            // arguments matches the arity of the primitive, all aof the arguments
                            // are fully nfd
                            if let Some(prim) = tc_env.primitives.get(name) {
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
                        | Neutral::Head(Head::Global(_))
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
            let value_cond = nf_term(tc_env, cond)?;

            match *value_cond {
                Value::Literal(Literal::Bool(true)) => nf_term(tc_env, if_true),
                Value::Literal(Literal::Bool(false)) => nf_term(tc_env, if_false),
                Value::Neutral(ref cond, ref spine) => Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::If(
                        cond.clone(),
                        nf_term(tc_env, if_true)?,
                        nf_term(tc_env, if_false)?,
                    )),
                    spine.clone(),
                ))),
                _ => Err(InternalError::ExpectedBoolExpr),
            }
        },

        // E-RECORD-TYPE
        Term::RecordType(ref scope) => {
            let ((label, binder, Embed(ann)), body) = scope.clone().unbind();
            let ann = nf_term(tc_env, &ann)?;
            let body = nf_term(tc_env, &body)?;

            Ok(Value::RecordType(Scope::new((label, binder, Embed(ann)), body)).into())
        },

        // E-EMPTY-RECORD-TYPE
        Term::RecordTypeEmpty => Ok(RcValue::from(Value::RecordTypeEmpty)),

        // E-RECORD
        Term::Record(ref scope) => {
            let ((label, binder, Embed(term)), body) = scope.clone().unbind();
            let value = nf_term(tc_env, &term)?;
            let body = nf_term(tc_env, &body)?;

            Ok(Value::Record(Scope::new((label, binder, Embed(value)), body)).into())
        },

        // E-EMPTY-RECORD
        Term::RecordEmpty => Ok(RcValue::from(Value::RecordEmpty)),

        // E-PROJ
        Term::Proj(ref expr, ref label) => match *nf_term(tc_env, expr)? {
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
            let head = nf_term(tc_env, head)?;

            if let Value::Neutral(ref neutral, ref spine) = *head {
                Ok(RcValue::from(Value::Neutral(
                    RcNeutral::from(Neutral::Case(
                        neutral.clone(),
                        clauses
                            .iter()
                            .map(|clause| {
                                let (pattern, body) = clause.clone().unbind();
                                Ok(Scope::new(pattern, nf_term(tc_env, &body)?))
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
                        return nf_term(tc_env, &body.substs(&mappings));
                    }
                }
                Err(InternalError::NoPatternsApplicable)
            }
        },

        // E-ARRAY
        Term::Array(ref elems) => Ok(RcValue::from(Value::Array(
            elems
                .iter()
                .map(|elem| nf_term(tc_env, elem))
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
