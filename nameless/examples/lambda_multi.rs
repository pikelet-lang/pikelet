//! An example of using the `nameless` library to implement the untyped lambda
//! calculus with multibinders

#[macro_use]
extern crate nameless;

use std::rc::Rc;
use nameless::{AlphaEq, Debruijn, FreeName, GenId, Named, Scope, Var};

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash, AlphaEq)]
pub enum Name {
    User(String),
    Gen(GenId),
}

impl Name {
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }
}

impl FreeName for Name {
    fn freshen(&mut self) {
        *self = match *self {
            Name::User(_) => Name::Gen(GenId::fresh()),
            Name::Gen(_) => return,
        };
    }
}

#[derive(Debug, Clone)]
pub enum Env {
    Empty,
    Extend(Rc<Env>, Name, Rc<Expr>),
}

fn extend(env: Rc<Env>, name: Name, expr: Rc<Expr>) -> Rc<Env> {
    Rc::new(Env::Extend(env, name, expr))
}

fn lookup<'a>(mut env: &'a Rc<Env>, name: &Name) -> Option<&'a Rc<Expr>> {
    while let Env::Extend(ref next_env, ref curr_name, ref expr) = **env {
        if Name::alpha_eq(curr_name, name) {
            return Some(expr);
        } else {
            env = next_env;
        }
    }
    None
}

// FIXME: remove need for this!
#[derive(Debug, Copy, Clone, AlphaEq, Bound)]
pub struct Unit;

#[derive(Debug, Clone, AlphaEq, Bound)]
pub enum Expr {
    Var(Var<Name, Debruijn>),
    Lam(Scope<Vec<Named<Name, Unit>>, Rc<Expr>>),
    App(Rc<Expr>, Vec<Rc<Expr>>),
}

#[derive(Debug, Clone)]
pub enum EvalError {
    ArgumentCountMismatch { expected: usize, given: usize },
}

pub fn eval(env: &Rc<Env>, expr: &Rc<Expr>) -> Result<Rc<Expr>, EvalError> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => Ok(lookup(env, name).unwrap_or(expr).clone()),
        Expr::Var(Var::Bound(ref name)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => Ok(expr.clone()),
        Expr::App(ref fun, ref args) => match *eval(env, fun)? {
            Expr::Lam(ref scope) => {
                let (params, body) = scope.clone().unbind();

                if params.len() != args.len() {
                    Err(EvalError::ArgumentCountMismatch {
                        expected: params.len(),
                        given: args.len(),
                    })
                } else {
                    let mut acc_env = env.clone();
                    for (param, arg) in <_>::zip(params.into_iter(), args.iter()) {
                        acc_env = extend(acc_env, param.name, eval(env, arg)?);
                    }
                    eval(&acc_env, &body)
                }
            },
            _ => Ok(expr.clone()),
        },
    }
}

fn main() {
    // expr = (fn(x, y) -> x)(a, b)
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::bind(
            vec![
                Named::new(Name::user("x"), Unit),
                Named::new(Name::user("y"), Unit),
            ],
            Rc::new(Expr::Var(Var::Free(Name::user("x")))),
        ))),
        vec![
            Rc::new(Expr::Var(Var::Free(Name::user("a")))),
            Rc::new(Expr::Var(Var::Free(Name::user("b")))),
        ],
    ));

    // FIXME - currently prints `Ok(Var(User("a")))` due to a bad impl of `Binder` for `[T]`
    println!("{:?}", eval(&Rc::new(Env::Empty), &expr)); // Ok(Var(User("b")))
}
