//! An example of using the `nameless` library to implement the untyped lambda
//! calculus

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
#[derive(Debug, Copy, Clone, AlphaEq, LocallyNameless)]
pub struct Unit;

#[derive(Debug, Clone, AlphaEq, LocallyNameless)]
pub enum Expr {
    Var(Var<Name, Debruijn>),
    Lam(Scope<Named<Name, Unit>, Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn eval(env: &Rc<Env>, expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => lookup(env, name).unwrap_or(expr).clone(),
        Expr::Var(Var::Bound(ref name)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(env, fun) {
            Expr::Lam(ref scope) => {
                let (name, body) = scope.clone().unbind();
                eval(&extend(env.clone(), name.name, eval(env, arg)), &body)
            },
            _ => expr.clone(),
        },
    }
}

fn main() {
    // expr = (\x -> x) y
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::bind(
            Named::new(Name::user("x"), Unit),
            Rc::new(Expr::Var(Var::Free(Name::user("x")))),
        ))),
        Rc::new(Expr::Var(Var::Free(Name::user("y")))),
    ));

    println!("{:?}", eval(&Rc::new(Env::Empty), &expr)); // Var(User("y"))
}
