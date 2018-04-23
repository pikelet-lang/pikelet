use nameless::{self, Embed, Ignore, Var};
use std::rc::Rc;

use semantics::{whnf, TypeError};
use syntax::core::{Context, Term, Type, Value};

/// Type directed conversion checking of terms
pub fn compare(
    context: &Context,
    lhs: &Rc<Term>,
    rhs: &Rc<Term>,
    ty: &Rc<Type>,
) -> Result<(), TypeError> {
    let lhs = whnf(context, lhs)?;
    let rhs = whnf(context, rhs)?;
    let ty = whnf(context, &Rc::new(Term::from(&**ty)))?;

    compare_whnf(context, &lhs, &rhs, &ty)
}

/// Type directed conversion checking of weak head normal forms
pub fn compare_whnf(
    context: &Context,
    lhs: &Rc<Value>,
    rhs: &Rc<Value>,
    ty: &Rc<Type>,
) -> Result<(), TypeError> {
    match (&**lhs, &**rhs, &**ty) {
        (&Value::Universe(l), &Value::Universe(r), &Value::Universe(t))
            if l == r && t == l.succ() =>
        {
            Ok(())
        },
        (&Value::Constant(ref cl), &Value::Constant(ref cr), t) if cl == cr => unimplemented!(),
        (_, _, Value::Pi(ref scope)) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            let ann = whnf(context, &ann)?;
            let body = whnf(context, &body)?;

            compare(
                &context.claim(name.clone(), ann),
                &Rc::new(Term::App(
                    Rc::new(Term::from(&**lhs)),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(name.clone()))),
                )),
                &Rc::new(Term::App(
                    Rc::new(Term::from(&**rhs)),
                    Rc::new(Term::Var(Ignore::default(), Var::Free(name.clone()))),
                )),
                &body,
            )
        },
        _ => unimplemented!(),
    }
}
