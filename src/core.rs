use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name(pub String);

/// A type annotated with a name for debugging purposes
///
/// The name is ignored for equality comparisons
#[derive(Debug, Clone, Eq)]
pub struct Named<T>(pub Name, pub T);

impl<T: PartialEq> PartialEq for Named<T> {
    fn eq(&self, other: &Named<T>) -> bool {
        &self.1 == &other.1
    }
}

/// The [debruijn index] of the binder that introduced the variable
///
/// For example:
///
/// ```text
/// λx.∀y.λz. x z (y z)
/// λ  ∀  λ   2 0 (1 0)
/// ```
///
/// [debruijn index]: https://en.wikipedia.org/wiki/De_Bruijn_index
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Debruijn(pub u32);

impl Debruijn {
    /// The debruijn index of the current binder
    pub fn zero() -> Debruijn {
        Debruijn(0)
    }

    /// Move the current debruijn index into an inner binder
    pub fn succ(self) -> Debruijn {
        Debruijn(self.0 + 1)
    }
}

/// Checkable terms
///
/// These terms do not contain full type information within them, so in order to
/// check them we need to supply a type to the checking algorithm
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CTerm {
    /// Inferrable terms
    Inf(Rc<ITerm>),
    /// Lambdas without an explicit type annotation
    ///
    /// ```
    /// \x, t
    /// ```
    Lam(Named<()>, Rc<CTerm>),
}

impl From<ITerm> for CTerm {
    fn from(src: ITerm) -> CTerm {
        CTerm::Inf(Rc::new(src))
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
    /// ```
    /// e : t
    /// ```
    Ann(Rc<CTerm>, Rc<CTerm>),
    /// Type of types
    Type,
    /// Fully annotated lambda abstractions
    ///
    /// Note that the body of the lambda must have a type that can be inferred
    /// from context
    ///
    /// ```
    /// \x : t, t
    /// ```
    Lam(Named<Rc<CTerm>>, Rc<ITerm>),
    /// Fully annotated pi types
    ///
    /// ```
    /// [x : t], t
    /// ```
    Pi(Named<Rc<CTerm>>, Rc<CTerm>),
    /// A variable that is bound by an abstraction
    Bound(Named<Debruijn>),
    /// A free variable
    Free(Name),
    /// Term application
    ///
    /// ```
    /// f x
    /// ```
    App(Rc<ITerm>, Rc<CTerm>),
}

/// Fully evaluated or stuck values
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Type,
    Bound(Named<Debruijn>),
    Lam(Named<Option<Rc<Value>>>, Rc<Value>),
    Pi(Named<Rc<Value>>, Rc<Value>),
    Stuck(Rc<SValue>),
}

/// 'Stuck' values that cannot be reduced further
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SValue {
    /// Attempted to evaluate a free variable
    Free(Name),
    /// Tried to apply a value to a stuck term
    App(Rc<SValue>, Rc<Value>),
}

pub type Type = Value;

// Abstraction and instantiation

impl CTerm {
    pub fn abstract0(&mut self, name: &Name) {
        self.abstract_at(Debruijn::zero(), name);
    }

    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        match *self {
            CTerm::Inf(ref mut i) => Rc::make_mut(i).abstract_at(level, name),
            CTerm::Lam(_, ref mut body) => Rc::make_mut(body).abstract_at(level.succ(), name),
        }
    }
}

impl ITerm {
    pub fn abstract0(&mut self, name: &Name) {
        self.abstract_at(Debruijn::zero(), name);
    }

    pub fn abstract_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *self {
            ITerm::Ann(ref mut expr, ref mut ty) => {
                Rc::make_mut(expr).abstract_at(level, name);
                Rc::make_mut(ty).abstract_at(level, name);
                return;
            }
            ITerm::Lam(Named(_, ref mut ty), ref mut body) => {
                Rc::make_mut(ty).abstract_at(level, name);
                Rc::make_mut(body).abstract_at(level.succ(), name);
                return;
            }
            ITerm::Pi(Named(_, ref mut ty), ref mut body) => {
                Rc::make_mut(ty).abstract_at(level, name);
                Rc::make_mut(body).abstract_at(level.succ(), name);
                return;
            }
            ITerm::Free(ref n) if n == name => ITerm::Bound(Named(n.clone(), level)),
            ITerm::Type | ITerm::Bound(_) | ITerm::Free(_) => return,
            ITerm::App(ref mut f, ref mut x) => {
                Rc::make_mut(f).abstract_at(level, name);
                Rc::make_mut(x).abstract_at(level, name);
                return;
            }
        }
    }
}

impl Value {
    pub fn instantiate0(&mut self, x: &Rc<Value>) {
        self.instantiate_at(Debruijn::zero(), &x);
    }

    pub fn instantiate_at(&mut self, level: Debruijn, x: &Rc<Value>) {
        *self = match *self {
            Value::Bound(Named(_, b)) if b == level => (**x).clone(),
            Value::Type | Value::Bound(_) => return,
            Value::Lam(Named(_, ref mut ty), ref mut body) => {
                if let Some(ref mut ty) = *ty {
                    Rc::make_mut(ty).instantiate_at(level, x);
                }
                Rc::make_mut(body).instantiate_at(level.succ(), x);
                return;
            }
            Value::Pi(Named(_, ref mut ty), ref mut body) => {
                Rc::make_mut(ty).instantiate_at(level, x);
                Rc::make_mut(body).instantiate_at(level.succ(), x);
                return;
            }
            Value::Stuck(ref mut stuck) => {
                Rc::make_mut(stuck).instantiate_at(level, x);
                return;
            }
        };
    }
}

impl SValue {
    pub fn instantiate0(&mut self, x: &Rc<Value>) {
        self.instantiate_at(Debruijn::zero(), &x);
    }

    pub fn instantiate_at(&mut self, level: Debruijn, x: &Rc<Value>) {
        match *self {
            SValue::Free(_) => {}
            SValue::App(ref mut fn_expr, ref mut arg_expr) => {
                Rc::make_mut(fn_expr).instantiate_at(level, x);
                Rc::make_mut(arg_expr).instantiate_at(level, x);
            }
        }
    }
}

// Evaluation

#[derive(Debug, Clone)]
pub enum EvalError {
    /// Attempted to apply an argument to a term that is not a function
    ArgumentAppliedToNonFunction { arg: Rc<Value>, expr: Rc<Value> },
}

impl CTerm {
    pub fn eval(&self) -> Result<Rc<Value>, EvalError> {
        match *self {
            CTerm::Inf(ref i) => i.eval(),
            CTerm::Lam(Named(ref name, ()), ref body) => {
                Ok(Rc::new(Value::Lam(Named(name.clone(), None), body.eval()?)))
            }
        }
    }
}

impl ITerm {
    pub fn eval(&self) -> Result<Rc<Value>, EvalError> {
        match *self {
            ITerm::Ann(ref expr, _) => expr.eval(),
            ITerm::Type => Ok(Rc::new(Value::Type)),
            ITerm::Lam(Named(ref name, ref ty), ref body) => Ok(Rc::new(Value::Lam(
                Named(name.clone(), Some(ty.eval()?)),
                body.eval()?,
            ))),
            ITerm::Pi(Named(ref name, ref ty), ref body) => Ok(Rc::new(Value::Pi(
                Named(name.clone(), ty.eval()?),
                body.eval()?,
            ))),
            ITerm::Bound(ref b) => Ok(Rc::new(Value::Bound(b.clone()))),
            ITerm::Free(ref n) => Ok(Rc::new(Value::Stuck(Rc::new(SValue::Free(n.clone()))))),
            ITerm::App(ref fn_expr, ref arg) => {
                let fn_expr = fn_expr.eval()?;
                let arg = arg.eval()?;
                match *fn_expr {
                    Value::Lam(_, ref body_expr) => {
                        let mut body_expr = body_expr.clone();
                        Rc::make_mut(&mut body_expr).instantiate0(&arg);
                        Ok(body_expr)
                    }
                    Value::Stuck(ref s) => {
                        Ok(Rc::new(Value::Stuck(Rc::new(SValue::App(s.clone(), arg)))))
                    }
                    _ => Err(EvalError::ArgumentAppliedToNonFunction {
                        arg,
                        expr: fn_expr.clone(),
                    }),
                }
            }
        }
    }
}

// Contexts and type checking

#[derive(Debug, Clone)]
pub enum TypeError {
    Eval(EvalError),
    IllegalApplication,
    ExpectedFunction,
    Mismatch { found: Rc<Type>, expected: Rc<Type> },
    UnboundVariable(Name),
}

impl From<EvalError> for TypeError {
    fn from(src: EvalError) -> TypeError {
        TypeError::Eval(src)
    }
}

pub enum Context<'a> {
    Nil,
    Cons(&'a Context<'a>, Rc<Value>),
}

impl Default for Context<'static> {
    fn default() -> Context<'static> {
        Context::Nil
    }
}

impl<'a> Context<'a> {
    pub fn extend(&'a self, value: Rc<Value>) -> Context<'a> {
        Context::Cons(self, value)
    }

    pub fn lookup(&'a self, x: Debruijn) -> Option<&'a Rc<Value>> {
        match (self, x) {
            (&Context::Nil, _) => None,
            (&Context::Cons(_, ref value), Debruijn(0)) => Some(value),
            (&Context::Cons(parent, _), Debruijn(x)) => parent.lookup(Debruijn(x - 1)),
        }
    }

    pub fn check(&self, expr: &CTerm, expected_ty: &Type) -> Result<(), TypeError> {
        match *expr {
            CTerm::Inf(ref inf_expr) => match self.infer(inf_expr)? {
                // Ensure that the inferred type matches the expected type
                ref inf_ty if &**inf_ty == expected_ty => Ok(()),
                inf_ty => Err(TypeError::Mismatch {
                    found: inf_ty,
                    expected: Rc::new(expected_ty.clone()),
                }),
            },
            CTerm::Lam(_, ref body_expr) => match *expected_ty {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    self.extend(param_ty.clone()).check(body_expr, ret_ty)
                }
                _ => Err(TypeError::ExpectedFunction),
            },
        }
    }

    pub fn infer(&self, expr: &ITerm) -> Result<Rc<Type>, TypeError> {
        match *expr {
            ITerm::Ann(ref expr, ref ty) => {
                // Check that the type is actually at the type level
                self.check(ty, &Value::Type)?;
                // Simplify the type
                let simp_ty = ty.eval()?;
                // Ensure that the type of the expression is compatible with the
                // simplified annotation
                self.check(expr, &simp_ty)?;
                Ok(simp_ty)
            }
            ITerm::Type => Ok(Rc::new(Value::Type)),
            ITerm::Lam(Named(ref param_name, ref param_ty), ref body_expr) => {
                // Check that the type is actually at the type level
                self.check(param_ty, &Value::Type)?;
                // Simplify the param type
                let simp_param_ty = param_ty.eval()?;
                // Infer the body of the lambda
                let body_ty = self.extend(simp_param_ty.clone()).infer(body_expr)?;

                Ok(Rc::new(Value::Pi(
                    Named(param_name.clone(), simp_param_ty),
                    body_ty,
                )))
            }
            ITerm::Pi(Named(_, ref param_ty), ref body_ty) => {
                // Check that the type is actually at the type level
                self.check(param_ty, &Value::Type)?;
                // Simplify the type
                let simp_param_ty = param_ty.eval()?;
                // Ensure that the body of the pi type is also a type, when the
                // parameter is added to the context
                self.extend(simp_param_ty).check(body_ty, &Value::Type)?;
                // If this is true, the type of the pi type is also a type
                Ok(Rc::new(Value::Type))
            }
            ITerm::Bound(Named(_, b)) => {
                Ok(self.lookup(b).expect("ICE: index out of bounds").clone())
            }
            ITerm::Free(ref n) => Err(TypeError::UnboundVariable(n.clone())),
            ITerm::App(ref fn_expr, ref arg_expr) => {
                match *self.infer(fn_expr)? {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        // Check that the type of the argument matches the
                        // expected type of the parameter
                        self.check(arg_expr, param_ty)?;
                        // Simplify the argument
                        let simp_arg_expr = arg_expr.eval()?;
                        // Apply the argument to the body of the pi type
                        let mut body_ty = ret_ty.clone();
                        Rc::make_mut(&mut body_ty).instantiate0(&simp_arg_expr);
                        Ok(body_ty)
                    }
                    _ => Err(TypeError::IllegalApplication),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> ITerm {
        use parse::Term;
        Term::to_core(&src.parse().unwrap()).unwrap()
    }

    mod alpha_eq {
        use super::*;

        #[test]
        fn var() {
            assert_eq!(parse(r"x").eval().unwrap(), parse(r"x").eval().unwrap(),);
        }

        #[test]
        #[should_panic]
        fn var_diff() {
            assert_eq!(parse(r"x").eval().unwrap(), parse(r"y").eval().unwrap(),);
        }

        #[test]
        fn ty() {
            assert_eq!(parse(r"*").eval().unwrap(), parse(r"*").eval().unwrap(),);
        }

        #[test]
        fn lam() {
            assert_eq!(
                parse(r"\x : *, x").eval().unwrap(),
                parse(r"\a : *, a").eval().unwrap(),
            );
        }

        #[test]
        fn pi() {
            assert_eq!(
                parse(r"[x : *], x").eval().unwrap(),
                parse(r"[a : *], a").eval().unwrap(),
            );
        }

        #[test]
        fn lam_app() {
            assert_eq!(
                parse(r"\x : (* -> *), x *").eval().unwrap(),
                parse(r"\a : (* -> *), a *").eval().unwrap(),
            );
        }

        #[test]
        fn pi_app() {
            assert_eq!(
                parse(r"[x : (* -> *)], x *").eval().unwrap(),
                parse(r"[a : (* -> *)], a *").eval().unwrap(),
            );
        }

        #[test]
        fn lam_lam_app() {
            assert_eq!(
                parse(r"\x : (* -> *), \y : *, x y").eval().unwrap(),
                parse(r"\a : (* -> *), \b : *, a b").eval().unwrap(),
            );
        }

        #[test]
        fn pi_pi_app() {
            assert_eq!(
                parse(r"[x : (* -> *)], [y : *], x y").eval().unwrap(),
                parse(r"[a : (* -> *)], [b : *], a b").eval().unwrap(),
            );
        }
    }

    mod infer {
        use super::*;

        #[test]
        fn ty() {
            let ctx = Context::default();

            assert_eq!(
                ctx.infer(&parse(r"*")).unwrap(),
                parse(r"*").eval().unwrap(),
            );
        }

        #[test]
        fn lam() {
            let ctx = Context::default();

            assert_eq!(
                ctx.infer(&parse(r"\a : *, a")).unwrap(),
                parse(r"[a : *], a").eval().unwrap(),
            );
        }

        #[test]
        fn pi() {
            let ctx = Context::default();

            assert_eq!(
                ctx.infer(&parse(r"[a : *], a")).unwrap(),
                parse(r"*").eval().unwrap(),
            );
        }

        #[test]
        fn id() {
            let ctx = Context::default();

            assert_eq!(
                ctx.infer(&parse(r"\a : *, \x : a, x")).unwrap(),
                parse(r"[a: *], a -> a").eval().unwrap(),
            );
        }
    }
}
