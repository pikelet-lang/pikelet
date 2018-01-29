use std::fmt;
use std::rc::Rc;

use parse::Term as ParseTerm;
use pretty::{self, ToDoc};
use var::{Debruijn, Name, Named, Var};

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(RcTerm, RcTerm),
    /// Type of types
    Type,
    /// A variable
    Var(Var),
    /// Lambda abstractions
    ///
    /// ```text
    /// \x => t
    /// \x : t => t
    /// ```
    Lam(Named<Option<RcTerm>>, RcTerm),
    /// Dependent function type
    ///
    /// ```text
    /// (x : t) -> t
    /// ```
    Pi(Named<RcTerm>, RcTerm),
    /// Term application
    ///
    /// ```text
    /// f x
    /// ```
    App(RcTerm, RcTerm),
}

impl From<Var> for Term {
    fn from(src: Var) -> Term {
        Term::Var(src)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Context::default())
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// Normal forms
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The type of types
    Type,
    /// A partially evaluated lambda
    Lam(Named<Option<RcValue>>, RcValue),
    /// A pi type
    Pi(Named<RcValue>, RcValue),
    /// Neutral values
    Neutral(RcNeutral),
}

impl From<RcNeutral> for Value {
    fn from(src: RcNeutral) -> Value {
        Value::Neutral(src)
    }
}

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::from(RcNeutral::from(src))
    }
}

impl From<Var> for Value {
    fn from(src: Var) -> Value {
        Value::from(Neutral::from(src))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Context::default())
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

/// Neutral forms
///
/// https://cs.stackexchange.com/questions/69434/intuitive-explanation-of-neutral-normal-form-in-lambda-calculus
#[derive(Debug, Clone, PartialEq)]
pub enum Neutral {
    /// Variabls
    Var(Var),
    /// Application of normal forms to neutral forms
    App(RcNeutral, RcValue),
}

impl From<Var> for Neutral {
    fn from(src: Var) -> Neutral {
        Neutral::Var(src)
    }
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Context::default())
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

// Wrapper types

macro_rules! make_wrapper {
    ($name:ident, $inner:ty) => {
        #[derive(Clone, PartialEq)]
        pub struct $name {
            pub inner: Rc<$inner>,
        }

        impl From<$inner> for $name {
            fn from(src: $inner) -> $name {
                $name {
                    inner: Rc::new(src),
                }
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Debug::fmt(&self.inner, f)
            }
        }
    };
}

make_wrapper!(RcTerm, Term);
make_wrapper!(RcValue, Value);
make_wrapper!(RcNeutral, Neutral);

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

// Abstraction and instantiation

impl RcTerm {
    pub fn abstract0(&mut self, name: &Name) {
        self.abstract_at(Debruijn::ZERO, name);
    }

    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Term::Ann(ref mut expr, ref mut ty) => {
                expr.abstract_at(level, name);
                ty.abstract_at(level, name);
            }
            Term::Lam(Named(_, None), ref mut body) => body.abstract_at(level.succ(), name),
            Term::Lam(Named(_, Some(ref mut ty)), ref mut body) => {
                ty.abstract_at(level, name);
                body.abstract_at(level.succ(), name);
            }
            Term::Pi(Named(_, ref mut ty), ref mut body) => {
                ty.abstract_at(level, name);
                body.abstract_at(level.succ(), name);
            }
            Term::Var(ref mut var) => var.abstract_at(level, name),
            Term::Type => {}
            Term::App(ref mut f, ref mut x) => {
                f.abstract_at(level, name);
                x.abstract_at(level, name);
            }
        }
    }
}

impl RcValue {
    pub fn eval_app(fn_expr: RcValue, arg: RcValue) -> Result<RcValue, EvalError> {
        match *fn_expr.inner {
            Value::Lam(_, ref body) => RcValue::instantiate0(body, &arg),
            Value::Neutral(ref stuck) => Ok(Value::from(Neutral::App(stuck.clone(), arg)).into()),
            _ => Err(EvalError::ArgAppliedToNonFunction {
                expr: fn_expr.clone(),
                arg: arg,
            }),
        }
    }

    pub fn instantiate0(val: &RcValue, x: &RcValue) -> Result<RcValue, EvalError> {
        RcValue::instantiate_at(val, Debruijn::ZERO, &x)
    }

    pub fn instantiate_at(
        val: &RcValue,
        level: Debruijn,
        x: &RcValue,
    ) -> Result<RcValue, EvalError> {
        match *val.inner {
            Value::Type => Ok(val.clone()),
            Value::Lam(Named(ref name, ref param_ty), ref body) => {
                let param_ty = match param_ty.as_ref() {
                    None => None,
                    Some(ref param_ty) => Some(RcValue::instantiate_at(param_ty, level, x)?),
                };
                let body = RcValue::instantiate_at(body, level.succ(), x)?;

                Ok(Value::Lam(Named(name.clone(), param_ty), body).into())
            }
            Value::Pi(Named(ref name, ref param_ty), ref body) => {
                let param_ty = RcValue::instantiate_at(param_ty, level, x)?;
                let body = RcValue::instantiate_at(body, level.succ(), x)?;

                Ok(Value::Pi(Named(name.clone(), param_ty), body).into())
            }
            Value::Neutral(ref stuck) => RcNeutral::instantiate_at(stuck, level, x),
        }
    }
}

impl RcNeutral {
    pub fn instantiate0(val: &RcNeutral, x: &RcValue) -> Result<RcValue, EvalError> {
        RcNeutral::instantiate_at(val, Debruijn::ZERO, &x)
    }

    pub fn instantiate_at(
        val: &RcNeutral,
        level: Debruijn,
        x: &RcValue,
    ) -> Result<RcValue, EvalError> {
        match *val.inner {
            Neutral::Var(ref var) => match var.instantiate_at(level) {
                true => Ok(x.clone()),
                false => Ok(Value::from(val.clone()).into()),
            },
            Neutral::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = RcNeutral::instantiate_at(fn_expr, level, x)?;
                let arg = RcValue::instantiate_at(arg_expr, level, x)?;

                RcValue::eval_app(fn_expr, arg)
            }
        }
    }
}

// Conversions from the parse tree

impl RcTerm {
    /// Convert a parsed term into a checkable term
    pub fn from_parse(term: &ParseTerm) -> RcTerm {
        match *term {
            ParseTerm::Var(ref x) => Term::Var(Var::Free(Name::User(x.clone()))).into(),
            ParseTerm::Type => Term::Type.into(),
            ParseTerm::Ann(ref e, ref t) => {
                Term::Ann(RcTerm::from_parse(e).into(), RcTerm::from_parse(t).into()).into()
            }
            ParseTerm::Lam(ref args, ref body) => {
                let mut term = RcTerm::from_parse(body);

                for &(ref name, ref ann) in args.iter().rev() {
                    let name = Name::User(name.clone());
                    term.abstract0(&name);
                    term = match *ann {
                        None => Term::Lam(Named(name, None), term).into(),
                        Some(ref ann) => {
                            let ann = RcTerm::from_parse(ann).into();
                            Term::Lam(Named(name, Some(ann)), term).into()
                        }
                    };
                }

                term
            }
            ParseTerm::Pi(ref name, ref ann, ref body) => {
                let name = Name::User(name.clone());
                let mut body = RcTerm::from_parse(body);
                body.abstract0(&name);

                Term::Pi(Named(name, RcTerm::from_parse(ann).into()), body).into()
            }
            ParseTerm::Arrow(ref ann, ref body) => {
                let name = Name::Abstract;
                let mut body = RcTerm::from_parse(body);
                body.abstract0(&name);

                Term::Pi(Named(name, RcTerm::from_parse(ann).into()), body).into()
            }
            ParseTerm::App(ref f, ref arg) => {
                Term::App(RcTerm::from_parse(f), RcTerm::from_parse(arg)).into()
            }
        }
    }
}

// Evaluation

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// Attempted to apply an argument to a term that is not a function
    ArgAppliedToNonFunction { arg: RcValue, expr: RcValue },
}

impl RcTerm {
    pub fn eval(&self) -> Result<RcValue, EvalError> {
        // e ⇓ v
        match *self.inner {
            //  1.  e ⇓ v
            // ────────────────── (EVAL/ANN)
            //      e : ρ ⇓ v
            Term::Ann(ref expr, _) => {
                expr.eval() // 1.
            }

            // ───────────── (EVAL/TYPE)
            //  Type ⇓ Type
            Term::Type => Ok(Value::Type.into()),

            // ─────── (EVAL/Var)
            //  x ⇓ x
            Term::Var(ref var) => Ok(Value::from(var.clone()).into()),

            //  1. e ⇓ v
            // ───────────────── (EVAL/LAM)
            //     λx.e ⇓ λx→v
            Term::Lam(Named(ref name, None), ref body_expr) => {
                let body_expr = body_expr.eval()?; // 1.

                Ok(Value::Lam(Named(name.clone(), None), body_expr).into())
            }

            //  1.  ρ ⇓ τ
            //  2.  e ⇓ v
            // ──────────────────────── (EVAL/LAM-ANN)
            //      λx:ρ→e ⇓ λx:τ→v
            Term::Lam(Named(ref name, Some(ref param_ty)), ref body_expr) => {
                let param_ty = param_ty.eval()?; // 1.
                let body_expr = body_expr.eval()?; // 2.

                Ok(Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into())
            }

            //  1.  ρ₁ ⇓ τ₁
            //  2.  ρ₂ ⇓ τ₂
            // ─────────────────────────── (EVAL/PI-ANN)
            //      (x:ρ₁)→ρ₂ ⇓ (x:τ₁)→τ₂
            Term::Pi(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = param_ty.eval()?; // 1.
                let body_expr = body_expr.eval()?; // 2.

                Ok(Value::Pi(Named(name.clone(), param_ty), body_expr).into())
            }

            //  1.  e₁ ⇓ λx→v₁
            //  2.  v₁[x↦e₂] ⇓ v₂
            // ───────────────────── (EVAL/APP)
            //      e₁ e₂ ⇓ v₂
            Term::App(ref fn_expr, ref arg) => {
                let fn_expr = fn_expr.eval()?; // 1.
                let arg = arg.eval()?; // 2.

                RcValue::eval_app(fn_expr, arg)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> RcTerm {
        RcTerm::from_parse(&src.parse().unwrap())
    }

    mod alpha_eq {
        use super::*;

        #[test]
        fn var() {
            assert_eq!(parse(r"x"), parse(r"x"));
        }

        #[test]
        #[should_panic]
        fn var_diff() {
            assert_eq!(parse(r"x"), parse(r"y"));
        }

        #[test]
        fn ty() {
            assert_eq!(parse(r"Type"), parse(r"Type"));
        }

        #[test]
        fn lam() {
            assert_eq!(parse(r"\x : Type => x"), parse(r"\a : Type => a"));
        }

        #[test]
        fn pi() {
            assert_eq!(parse(r"(x : Type) -> x"), parse(r"(a : Type) -> a"));
        }

        #[test]
        fn lam_app() {
            assert_eq!(
                parse(r"\x : Type -> Type => x Type"),
                parse(r"\a : Type -> Type => a Type")
            );
        }

        #[test]
        fn pi_app() {
            assert_eq!(
                parse(r"(x : Type -> Type) -> x Type"),
                parse(r"(a : Type -> Type) -> a Type")
            );
        }

        #[test]
        fn lam_lam_app() {
            assert_eq!(
                parse(r"\x : Type -> Type => \y : Type => x y"),
                parse(r"\a : Type -> Type => \b : Type => a b"),
            );
        }

        #[test]
        fn pi_pi_app() {
            assert_eq!(
                parse(r"(x : Type -> Type) -> (y : Type) -> x y"),
                parse(r"(a : Type -> Type) -> (b : Type) -> a b"),
            );
        }
    }

    mod eval {
        use super::*;

        #[test]
        fn var() {
            let x = Name::user("x");

            assert_eq!(
                parse(r"x").eval().unwrap(),
                Value::from(Var::Free(x)).into(),
            );
        }

        #[test]
        fn ty() {
            let ty: RcValue = Value::Type.into();

            assert_eq!(parse(r"Type").eval().unwrap(), ty);
        }

        #[test]
        fn lam() {
            let x = Name::user("x");
            let ty: RcValue = Value::Type.into();

            assert_eq!(
                parse(r"\x : Type => x").eval().unwrap(),
                Value::Lam(
                    Named(x.clone(), Some(ty)),
                    Value::from(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            );
        }

        #[test]
        fn pi() {
            let x = Name::user("x");
            let ty: RcValue = Value::Type.into();

            assert_eq!(
                parse(r"(x : Type) -> x").eval().unwrap(),
                Value::Pi(
                    Named(x.clone(), ty),
                    Value::from(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            );
        }

        #[test]
        fn lam_app() {
            let x = Name::user("x");
            let y = Name::user("y");
            let ty: RcValue = Value::Type.into();
            let ty_arr: RcValue = Value::Pi(Named(Name::Abstract, ty.clone()), ty.clone()).into();

            assert_eq!(
                parse(r"\x : Type -> Type => \y : Type => x y")
                    .eval()
                    .unwrap(),
                Value::Lam(
                    Named(x.clone(), Some(ty_arr)),
                    Value::Lam(
                        Named(y.clone(), Some(ty)),
                        Value::from(Neutral::App(
                            Neutral::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                            Value::from(Var::Bound(Named(y, Debruijn(0)))).into(),
                        )).into(),
                    ).into(),
                ).into(),
            );
        }

        #[test]
        fn pi_app() {
            let x = Name::user("x");
            let y = Name::user("y");
            let ty: RcValue = Value::Type.into();
            let ty_arr: RcValue = Value::Pi(Named(Name::Abstract, ty.clone()), ty.clone()).into();

            assert_eq!(
                parse(r"(x : Type -> Type) -> \y : Type => x y")
                    .eval()
                    .unwrap(),
                Value::Pi(
                    Named(x.clone(), ty_arr),
                    Value::Lam(
                        Named(y.clone(), Some(ty)),
                        Value::from(Neutral::App(
                            Neutral::from(Var::Bound(Named(x, Debruijn(1)))).into(),
                            Value::from(Var::Bound(Named(y, Debruijn(0)))).into(),
                        )).into(),
                    ).into(),
                ).into(),
            );
        }
    }
}
