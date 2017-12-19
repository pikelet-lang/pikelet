use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name(pub String);

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
#[derive(Debug, Clone)]
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

/// Inferrable terms
///
/// These terms can be fully inferred without needing to resort to type
/// inference
#[derive(Debug, Clone)]
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
    Bound(Debruijn),
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
    Bound(Debruijn),
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
    pub fn abstract0(&self, name: &Name) -> CTerm {
        self.abstract_at(Debruijn::zero(), name)
    }

    pub fn abstract_at(&self, level: Debruijn, name: &Name) -> CTerm {
        match *self {
            CTerm::Inf(ref i) => CTerm::Inf(Rc::new(i.abstract_at(level, name))),
            CTerm::Lam(ref param, ref body) => {
                CTerm::Lam(param.clone(), Rc::new(body.abstract_at(level.succ(), name)))
            }
        }
    }
}

impl ITerm {
    pub fn abstract0(&self, name: &Name) -> ITerm {
        self.abstract_at(Debruijn::zero(), name)
    }

    pub fn abstract_at(&self, level: Debruijn, name: &Name) -> ITerm {
        match *self {
            ITerm::Ann(ref expr, ref ty) => ITerm::Ann(
                Rc::new(expr.abstract_at(level, name)),
                Rc::new(ty.abstract_at(level, name)),
            ),
            ITerm::Type => ITerm::Type,
            ITerm::Lam(Named(ref name, ref ty), ref body) => ITerm::Lam(
                Named(name.clone(), Rc::new(ty.abstract_at(level, name))),
                Rc::new(body.abstract_at(level.succ(), name)),
            ),
            ITerm::Pi(Named(ref name, ref ty), ref body) => ITerm::Pi(
                Named(name.clone(), Rc::new(ty.abstract_at(level, name))),
                Rc::new(body.abstract_at(level.succ(), name)),
            ),
            ITerm::Bound(ref b) => ITerm::Bound(*b),
            ITerm::Free(ref n) if n == name => ITerm::Bound(level),
            ITerm::Free(ref n) => ITerm::Free(n.clone()),
            ITerm::App(ref f, ref x) => ITerm::App(
                Rc::new(f.abstract_at(level, name)),
                Rc::new(x.abstract_at(level, name)),
            ),
        }
    }
}

impl Value {
    pub fn instantiate0(&self, x: &Rc<Value>) -> Value {
        self.instantiate_at(Debruijn::zero(), &x)
    }

    pub fn instantiate_at(&self, level: Debruijn, x: &Rc<Value>) -> Value {
        match *self {
            Value::Type => Value::Type,
            Value::Bound(b) if b == level => (**x).clone(),
            Value::Bound(b) => Value::Bound(b),
            Value::Lam(Named(ref name, ref ty), ref body) => Value::Lam(
                Named(name.clone(), ty.as_ref().map(|ty| Rc::new(ty.instantiate_at(level, x)))),
                Rc::new(body.instantiate_at(level.succ(), x)),
            ),
            Value::Pi(Named(ref name, ref ty), ref body) => Value::Pi(
                Named(name.clone(), Rc::new(ty.instantiate_at(level, x))),
                Rc::new(body.instantiate_at(level.succ(), x)),
            ),
            Value::Stuck(ref stuck) => Value::Stuck(Rc::new(stuck.instantiate_at(level, x))),
        }
    }
}

impl SValue {
    pub fn instantiate0(&self, x: &Rc<Value>) -> SValue {
        self.instantiate_at(Debruijn::zero(), &x)
    }

    pub fn instantiate_at(&self, level: Debruijn, x: &Rc<Value>) -> SValue {
        match *self {
            SValue::Free(ref n) => SValue::Free(n.clone()),
            SValue::App(ref fn_expr, ref arg_expr) => SValue::App(
                Rc::new(fn_expr.instantiate_at(level, x)),
                Rc::new(arg_expr.instantiate_at(level, x)),
            ),
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
            ITerm::Bound(ref b) => Ok(Rc::new(Value::Bound(*b))),
            ITerm::Free(ref n) => Ok(Rc::new(Value::Stuck(Rc::new(SValue::Free(n.clone()))))),
            ITerm::App(ref fn_expr, ref arg) => {
                let fn_expr = fn_expr.eval()?;
                let arg = arg.eval()?;
                let result = match *fn_expr {
                    Value::Lam(_, ref body_expr) => body_expr.instantiate0(&arg),
                    Value::Stuck(ref s) => Value::Stuck(Rc::new(SValue::App(s.clone(), arg))),
                    _ => {
                        return Err(EvalError::ArgumentAppliedToNonFunction {
                            arg,
                            expr: fn_expr.clone(),
                        })
                    }
                };

                Ok(Rc::new(result))
            }
        }
    }
}

// Contexts

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
}

// Type checking

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

impl CTerm {
    pub fn check(&self, ty: &Type, context: &Context) -> Result<(), TypeError> {
        match *self {
            CTerm::Inf(ref inf_expr) => match inf_expr.infer(context)? {
                ref inf_ty if &**inf_ty == ty => Ok(()),
                inf_ty => Err(TypeError::Mismatch {
                    found: inf_ty,
                    expected: Rc::new(ty.clone()),
                }),
            },
            CTerm::Lam(_, ref body_expr) => match *ty {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    body_expr.check(ret_ty, &context.extend(param_ty.clone()))
                }
                _ => Err(TypeError::ExpectedFunction),
            },
        }
    }
}

impl ITerm {
    pub fn infer(&self, context: &Context) -> Result<Rc<Type>, TypeError> {
        match *self {
            ITerm::Ann(ref expr, ref ty) => {
                // Check that the type is actually at the type level
                ty.check(&Value::Type, context)?;
                // Simplify the type
                let simp_ty = ty.eval()?;
                // Ensure that the type of the expression is compatible with the
                // simplified annotation
                expr.check(&simp_ty, context)?;
                Ok(simp_ty)
            }
            ITerm::Type => Ok(Rc::new(Value::Type)),
            ITerm::Lam(Named(ref param_name, ref param_ty), ref body_expr) => {
                // Check that the type is actually at the type level
                param_ty.check(&Value::Type, context)?;
                // Simplify the param type
                let simp_param_ty = param_ty.eval()?;
                // Infer the body of the lambda
                let body_ty = body_expr.infer(&context.extend(simp_param_ty.clone()))?;

                Ok(Rc::new(Value::Pi(
                    Named(param_name.clone(), simp_param_ty),
                    body_ty,
                )))
            }
            ITerm::Pi(Named(_, ref param_ty), ref body_ty) => {
                // Check that the type is actually at the type level
                param_ty.check(&Value::Type, context)?;
                // Simplify the type
                let simp_param_ty = param_ty.eval()?;
                // Ensure that the body of the pi type is also a type, when the
                // parameter is added to the context
                body_ty.check(&Value::Type, &context.extend(simp_param_ty))?;
                // If this is true, the type of the pi type is also a type
                Ok(Rc::new(Value::Type))
            }
            ITerm::Bound(b) => Ok(context.lookup(b).expect("ICE: index out of bounds").clone()),
            ITerm::Free(ref n) => Err(TypeError::UnboundVariable(n.clone())),
            ITerm::App(ref fn_expr, ref arg_expr) => {
                match *fn_expr.infer(context)? {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        // Check that the type of the argument matches the
                        // expected type of the parameter
                        arg_expr.check(param_ty, context)?;
                        // Simplify the argument
                        let simp_arg_expr = arg_expr.eval()?;
                        // Apply the argument to the body of the pi type
                        let body_ty = ret_ty.instantiate0(&simp_arg_expr);
                        Ok(Rc::new(body_ty))
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

    #[test]
    fn alpha_eq_lam() {
        assert_eq!(
            parse(r"\x : *, x").eval().unwrap(),
            parse(r"\a : *, a").eval().unwrap(),
        );
    }

    #[test]
    fn alpha_eq_pi() {
        assert_eq!(
            parse(r"[x : *], x").eval().unwrap(),
            parse(r"[a : *], a").eval().unwrap(),
        );
    }

    #[test]
    fn alpha_eq_lam_app() {
        assert_eq!(
            parse(r"\x : (* -> *), \y : *, x y").eval().unwrap(),
            parse(r"\a : (* -> *), \b : *, a b").eval().unwrap(),
        );
    }

    #[test]
    fn alpha_eq_pi_app() {
        assert_eq!(
            parse(r"[x : (* -> *)], [y : *], x y").eval().unwrap(),
            parse(r"[a : (* -> *)], [b : *], a b").eval().unwrap(),
        );
    }

    #[test]
    fn id_type_checks() {
        let ctx = Context::default();

        assert_eq!(
            parse(r"\a : *, \x : a, x").infer(&ctx).unwrap(),
            parse(r"[a: *], a -> a").eval().unwrap(),
        );
    }
}
