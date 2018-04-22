use nameless::{self, Embed, Ignore, Name, Var};
use std::rc::Rc;

use semantics::InternalError;
use syntax::core::{Constant, Context, Neutral, Term, Value};

/// Convert a term to weak head normal form in a context
pub fn whnf(context: &Context, term: &Rc<Term>) -> Result<Rc<Value>, InternalError> {
    match **term {
        // WHNF-ANN
        Term::Ann(_, ref expr, _) => whnf(context, expr),

        // WHNF-TYPE
        Term::Universe(_, level) => Ok(Rc::new(Value::Universe(level))),

        // WHNF-CONST
        Term::Constant(_, ref c) => Ok(Rc::new(Value::Constant(c.clone()))),

        // WHNF-VAR, WHNF-VAR-DEF
        Term::Var(_, ref var) => match *var {
            Var::Free(ref name) => match context.lookup_definition(name) {
                Some(term) => whnf(context, &term),
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

        // WHNF-PI
        Term::Pi(_, ref scope) => Ok(Rc::new(Value::Pi(scope.clone()))),

        // WHNF-LAM
        Term::Lam(_, ref scope) => Ok(Rc::new(Value::Lam(scope.clone()))),

        // WHNF-APP
        Term::App(ref expr, ref arg) => {
            let value_expr = whnf(context, expr)?;

            match *value_expr {
                Value::Lam(ref scope) => {
                    // FIXME: do a local unbind here
                    let ((name, Embed(_)), body) = nameless::unbind(scope.clone());

                    whnf(
                        context,
                        &Rc::new(Term::Subst(nameless::bind(
                            (name, Embed(arg.clone())),
                            body,
                        ))),
                    )
                },
                Value::Neutral(ref expr) => Ok(Rc::new(Value::from(Neutral::App(
                    expr.clone(),
                    arg.clone(),
                )))),
                _ => Err(InternalError::ArgumentAppliedToNonFunction { span: expr.span() }),
            }
        },

        // WHNF-SUBST
        Term::Subst(ref scope) => {
            let ((name, Embed(subst_term)), body) = nameless::unbind(scope.clone());
            let body = whnf(&context.define(name.clone(), subst_term.clone()), &body)?;
            whnf(context, &subst_whnf(body, &name, &subst_term))
        },

        // WHNF-IF, WHNF-IF-TRUE, WHNF-IF-FALSE
        Term::If(_, ref cond, ref if_true, ref if_false) => match *whnf(context, cond)? {
            Value::Constant(Constant::Bool(true)) => whnf(context, if_true),
            Value::Constant(Constant::Bool(false)) => whnf(context, if_false),
            Value::Neutral(ref cond) => Ok(Rc::new(Value::from(Neutral::If(
                cond.clone(),
                if_true.clone(),
                if_false.clone(),
            )))),
            _ => Err(InternalError::ExpectedBoolExpr { span: cond.span() }),
        },

        // WHNF-RECORD-TYPE
        Term::RecordType(_, ref label, ref ann, ref rest) => {
            let rest = whnf(context, rest)?;
            Ok(Rc::new(Value::RecordType(label.clone(), ann.clone(), rest)))
        },

        // WHNF-RECORD
        Term::Record(_, ref label, ref expr, ref rest) => {
            let rest = whnf(context, rest)?;
            Ok(Rc::new(Value::Record(label.clone(), expr.clone(), rest)))
        },

        // WHNF-EMPTY-RECORD-TYPE
        Term::EmptyRecordType(_) => Ok(Rc::new(Value::EmptyRecordType)),

        // WHNF-EMPTY-RECORD
        Term::EmptyRecord(_) => Ok(Rc::new(Value::EmptyRecord)),

        // WHNF-PROJ
        Term::Proj(_, ref expr, label_span, ref label) => {
            match whnf(context, expr)?.lookup_record(label) {
                Some(expr) => whnf(context, &expr),
                None => Err(InternalError::ProjectedOnNonExistentField {
                    label_span: label_span.0, // FIXME: better location info
                    label: label.clone(),
                }),
            }
        },
    }
}

/// Push a substitution into the arguments and bodies of a value
///
/// This might unstick some neutral terms, so it will need to be re-evaluated
/// afterwards!
fn subst_whnf(value: Rc<Value>, subst_name: &Name, subst_term: &Rc<Term>) -> Rc<Term> {
    let subst = |term: Rc<Term>| {
        // Blegh - this feels quite costly! :O
        Rc::new(Term::Subst(nameless::bind(
            (subst_name.clone(), Embed(subst_term.clone())),
            term,
        )))
    };

    match *value {
        Value::Universe(level) => Rc::new(Term::Universe(Ignore::default(), level)),
        Value::Constant(ref c) => Rc::new(Term::Constant(Ignore::default(), c.clone())),
        Value::Pi(ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::Pi(
                Ignore::default(),
                nameless::bind((name, Embed(subst(ann))), subst(body)),
            ))
        },
        Value::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            Rc::new(Term::Lam(
                Ignore::default(),
                nameless::bind((name, Embed(subst(ann))), subst(body)),
            ))
        },
        Value::RecordType(ref label, ref ann, ref body) => Rc::new(Term::RecordType(
            Ignore::default(),
            label.clone(),
            subst(ann.clone()),
            subst_whnf(body.clone(), subst_name, subst_term),
        )),
        Value::Record(ref label, ref expr, ref body) => Rc::new(Term::Record(
            Ignore::default(),
            label.clone(),
            subst(expr.clone()),
            subst_whnf(body.clone(), subst_name, subst_term),
        )),
        Value::EmptyRecordType => Rc::new(Term::EmptyRecordType(Ignore::default())),
        Value::EmptyRecord => Rc::new(Term::EmptyRecord(Ignore::default())),
        Value::Neutral(ref neutral) => match **neutral {
            Neutral::Var(ref var) => Rc::new(Term::Var(Ignore::default(), var.clone())),
            Neutral::App(ref expr, ref arg) => Rc::new(Term::App(
                subst(Rc::new(Term::from(&**expr))),
                subst(arg.clone()),
            )),
            Neutral::If(ref pred, ref if_true, ref if_false) => Rc::new(Term::If(
                Ignore::default(),
                subst(Rc::new(Term::from(&**pred))),
                subst(if_true.clone()),
                subst(if_false.clone()),
            )),
            Neutral::Proj(ref expr, ref label) => Rc::new(Term::Proj(
                Ignore::default(),
                subst(Rc::new(Term::from(&**expr))),
                Ignore::default(),
                label.clone(),
            )),
        },
    }
}
