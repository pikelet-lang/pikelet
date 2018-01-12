use std::fmt;
use std::rc::Rc;

use pretty::{self, ToDoc};
use var::{Debruijn, Name, Named, Var};

/// Checkable terms
///
/// These terms do not contain full type information within them, so in order to
/// check them we need to supply a type to the checking algorithm
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CTerm {
    /// Inferrable terms
    Inf(RcITerm),
    /// Lambdas without an explicit type annotation
    ///
    /// ```text
    /// \x => t
    /// ```
    Lam(Named<()>, RcCTerm),
}

impl From<ITerm> for CTerm {
    fn from(src: ITerm) -> CTerm {
        CTerm::Inf(src.into())
    }
}

impl From<Var> for CTerm {
    fn from(src: Var) -> CTerm {
        CTerm::from(ITerm::from(src))
    }
}

impl fmt::Display for CTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Context::default())
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RcCTerm {
    pub inner: Rc<CTerm>,
}

impl From<CTerm> for RcCTerm {
    fn from(src: CTerm) -> RcCTerm {
        RcCTerm {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcCTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

/// Inferrable terms
///
/// These terms can be fully inferred without needing to resort to type
/// inference
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ITerm {
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(RcCTerm, RcCTerm),
    /// Type of types
    Type,
    /// A variable
    Var(Var),
    /// Fully annotated lambda abstractions
    ///
    /// Note that the body of the lambda must have a type that can be inferred
    /// from context
    ///
    /// ```text
    /// \x : t => t
    /// ```
    Lam(Named<RcCTerm>, RcITerm),
    /// Fully annotated pi types
    ///
    /// ```text
    /// [x : t] -> t
    /// ```
    Pi(Named<RcCTerm>, RcCTerm),
    /// Term application
    ///
    /// ```text
    /// f x
    /// ```
    App(RcITerm, RcCTerm),
}

impl From<Var> for ITerm {
    fn from(src: Var) -> ITerm {
        ITerm::Var(src)
    }
}

impl fmt::Display for ITerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Context::default())
            .group()
            .render_fmt(f.width().unwrap_or(80), f)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RcITerm {
    pub inner: Rc<ITerm>,
}

impl From<ITerm> for RcITerm {
    fn from(src: ITerm) -> RcITerm {
        RcITerm {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcITerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

/// Normal forms
#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Clone, PartialEq, Eq)]
pub struct RcValue {
    pub inner: Rc<Value>,
}

impl From<Value> for RcValue {
    fn from(src: Value) -> RcValue {
        RcValue {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

/// Neutral forms
///
/// https://cs.stackexchange.com/questions/69434/intuitive-explanation-of-neutral-normal-form-in-lambda-calculus
#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Clone, PartialEq, Eq)]
pub struct RcNeutral {
    pub inner: Rc<Neutral>,
}

impl From<Neutral> for RcNeutral {
    fn from(src: Neutral) -> RcNeutral {
        RcNeutral {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcNeutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

// Abstraction and instantiation

impl RcCTerm {
    pub fn abstract0(&mut self, name: &Name) {
        self.abstract_at(Debruijn::ZERO, name);
    }

    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            CTerm::Inf(ref mut i) => i.abstract_at(level, name),
            CTerm::Lam(_, ref mut body) => body.abstract_at(level.succ(), name),
        }
    }
}

impl RcITerm {
    pub fn abstract0(&mut self, name: &Name) {
        self.abstract_at(Debruijn::ZERO, name);
    }

    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            ITerm::Ann(ref mut expr, ref mut ty) => {
                expr.abstract_at(level, name);
                ty.abstract_at(level, name);
            }
            ITerm::Lam(Named(_, ref mut ty), ref mut body) => {
                ty.abstract_at(level, name);
                body.abstract_at(level.succ(), name);
            }
            ITerm::Pi(Named(_, ref mut ty), ref mut body) => {
                ty.abstract_at(level, name);
                body.abstract_at(level.succ(), name);
            }
            ITerm::Var(ref mut var) => var.abstract_at(level, name),
            ITerm::Type => {}
            ITerm::App(ref mut f, ref mut x) => {
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

// Evaluation

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EvalError {
    /// Attempted to apply an argument to a term that is not a function
    ArgAppliedToNonFunction { arg: RcValue, expr: RcValue },
}

impl RcCTerm {
    pub fn eval(&self) -> Result<RcValue, EvalError> {
        // e ⇓ v
        match *self.inner {
            CTerm::Inf(ref inf) => inf.eval(),

            //  1. e ⇓ v
            // ───────────────── (EVAL/LAM)
            //     λx.e ⇓ λx→v
            CTerm::Lam(Named(ref name, ()), ref body_expr) => {
                let body_expr = body_expr.eval()?; // 1.

                Ok(Value::Lam(Named(name.clone(), None), body_expr).into())
            }
        }
    }
}

impl RcITerm {
    pub fn eval(&self) -> Result<RcValue, EvalError> {
        // e ⇓ v
        match *self.inner {
            //  1.  e ⇓ v
            // ────────────────── (EVAL/ANN)
            //      e : ρ ⇓ v
            ITerm::Ann(ref expr, _) => {
                expr.eval() // 1.
            }

            // ───────────── (EVAL/TYPE)
            //  Type ⇓ Type
            ITerm::Type => Ok(Value::Type.into()),

            // ─────── (EVAL/Var)
            //  x ⇓ x
            ITerm::Var(ref var) => Ok(Value::from(var.clone()).into()),

            //  1.  ρ ⇓ τ
            //  2.  e ⇓ v
            // ──────────────────────── (EVAL/LAM-ANN)
            //      λx:ρ→e ⇓ λx:τ→v
            ITerm::Lam(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = param_ty.eval()?; // 1.
                let body_expr = body_expr.eval()?; // 2.

                Ok(Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into())
            }

            //  1.  ρ₁ ⇓ τ₁
            //  2.  ρ₂ ⇓ τ₂
            // ─────────────────────────── (EVAL/PI-ANN)
            //      (x:ρ₁)→ρ₂ ⇓ (x:τ₁)→τ₂
            ITerm::Pi(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = param_ty.eval()?; // 1.
                let body_expr = body_expr.eval()?; // 2.

                Ok(Value::Pi(Named(name.clone(), param_ty), body_expr).into())
            }

            //  1.  e₁ ⇓ λx→v₁
            //  2.  v₁[x↦e₂] ⇓ v₂
            // ───────────────────── (EVAL/APP)
            //      e₁ e₂ ⇓ v₂
            ITerm::App(ref fn_expr, ref arg) => {
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

    fn parse(src: &str) -> RcITerm {
        use parse::Term;

        Term::to_core(&src.parse().unwrap()).unwrap()
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
            assert_eq!(parse(r"[x : Type] -> x"), parse(r"[a : Type] -> a"));
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
                parse(r"[x : Type -> Type] -> x Type"),
                parse(r"[a : Type -> Type] -> a Type")
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
                parse(r"[x : Type -> Type] -> [y : Type] -> x y"),
                parse(r"[a : Type -> Type] -> [b : Type] -> a b"),
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
                parse(r"[x : Type] -> x").eval().unwrap(),
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
                parse(r"[x : Type -> Type] -> \y : Type => x y")
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
