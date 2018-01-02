//! Contexts and type checking

use std::rc::Rc;

use core::{CTerm, EvalError, ITerm, Type, Value};
use var::{Debruijn, Name, Named, Var};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeError {
    Eval(EvalError),
    IllegalApplication,
    ExpectedFunction {
        lam_expr: Rc<CTerm>,
        expected: Rc<Type>,
    },
    Mismatch {
        expr: Rc<ITerm>,
        found: Rc<Type>,
        expected: Rc<Type>,
    },
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

    /// Check that the type of an expression is compatible with the expected type
    pub fn check(&self, expr: &CTerm, expected_ty: &Type) -> Result<(), TypeError> {
        match *expr {
            CTerm::Inf(ref inf_expr) => match self.infer(inf_expr)? {
                // Ensure that the inferred type matches the expected type
                ref inf_ty if &**inf_ty == expected_ty => Ok(()),
                inf_ty => Err(TypeError::Mismatch {
                    expr: inf_expr.clone(),
                    found: inf_ty,
                    expected: Rc::new(expected_ty.clone()),
                }),
            },
            CTerm::Lam(_, ref body_expr) => match *expected_ty {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    self.extend(param_ty.clone()).check(body_expr, ret_ty)
                }
                _ => Err(TypeError::ExpectedFunction {
                    lam_expr: Rc::new(expr.clone()),
                    expected: Rc::new(expected_ty.clone()),
                }),
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
                // Check that the parameter type is at the type level
                self.check(param_ty, &Value::Type)?;
                // Simplify the parameter type
                let simp_param_ty = param_ty.eval()?;
                // Infer the body of the lambda
                let body_ty = self.extend(simp_param_ty.clone()).infer(body_expr)?;

                Ok(Rc::new(Value::Pi(
                    Named(param_name.clone(), simp_param_ty),
                    body_ty, // shift??
                )))
            }
            ITerm::Pi(Named(_, ref param_ty), ref body_ty) => {
                // Check that the parameter type is at the type level
                self.check(param_ty, &Value::Type)?;
                // Simplify the parameter type
                let simp_param_ty = param_ty.eval()?;
                // Ensure that the body of the pi type is also a type when the
                // parameter is added to the context
                self.extend(simp_param_ty).check(body_ty, &Value::Type)?;
                // If this is true, the type of the pi type is also a type
                Ok(Rc::new(Value::Type))
            }
            ITerm::Var(Var::Bound(Named(_, b))) => {
                Ok(self.lookup(b).expect("ICE: index out of bounds").clone())
            }
            ITerm::Var(Var::Free(ref name)) => Err(TypeError::UnboundVariable(name.clone())),
            ITerm::App(ref fn_expr, ref arg_expr) => {
                let fn_type = self.infer(fn_expr)?;
                match *fn_type {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        // Check that the type of the argument matches the
                        // expected type of the parameter
                        self.check(arg_expr, param_ty)?;
                        // Simplify the argument
                        let simp_arg_expr = arg_expr.eval()?;
                        // Apply the argument to the body of the pi type
                        let body_ty = Value::instantiate0(ret_ty, &simp_arg_expr)?;
                        Ok(body_ty)
                    }
                    // TODO: More error info
                    _ => Err(TypeError::IllegalApplication),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::SValue;

    fn parse(src: &str) -> ITerm {
        use parse::Term;

        Term::to_core(&src.parse().unwrap()).unwrap()
    }

    #[test]
    fn extend_lookup() {
        let x = Rc::new(Value::from(SValue::Var(Var::Free(Name(String::from("x"))))));
        let y = Rc::new(Value::from(SValue::Var(Var::Free(Name(String::from("y"))))));

        let context0 = Context::Nil;

        assert_eq!(context0.lookup(Debruijn(0)), None);

        let context1 = context0.extend(x.clone());

        assert_eq!(context1.lookup(Debruijn(0)), Some(&x));
        assert_eq!(context1.lookup(Debruijn(1)), None);

        let context2 = context1.extend(y.clone());

        assert_eq!(context2.lookup(Debruijn(0)), Some(&y));
        assert_eq!(context2.lookup(Debruijn(1)), Some(&x));
        assert_eq!(context2.lookup(Debruijn(2)), None);
    }

    mod infer {
        use super::*;

        #[test]
        fn free() {
            let ctx = Context::default();
            let given_expr = r"x";
            let x = Name(String::from("x"));

            assert_eq!(
                ctx.infer(&parse(given_expr)),
                Err(TypeError::UnboundVariable(x)),
            );
        }

        #[test]
        fn ty() {
            let ctx = Context::default();
            let given_expr = r"*";
            let expected_ty = r"*";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_ty_id() {
            let ctx = Context::default();
            let given_expr = r"(\a, a) : * -> *";
            let expected_ty = r"* -> *";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_arrow_ty_id() {
            let ctx = Context::default();
            let given_expr = r"(\a, a) : (* -> *) -> (* -> *)";
            let expected_ty = r"(* -> *) -> (* -> *)";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_id_as_ty() {
            let ctx = Context::default();
            let given_expr = r"(\a, a) : *";

            match ctx.infer(&parse(given_expr)) {
                Err(TypeError::ExpectedFunction { .. }) => {}
                other => panic!("unexpected result: {:#?}", other),
            }
        }

        #[test]
        fn app() {
            let ctx = Context::default();
            let given_expr = r"(\a : *, a) *";
            let expected_ty = r"*";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn app_ty() {
            let ctx = Context::default();
            let given_expr = r"* *";

            assert_eq!(
                ctx.infer(&parse(given_expr)),
                Err(TypeError::IllegalApplication),
            )
        }

        #[test]
        fn lam() {
            let ctx = Context::default();
            let given_expr = r"\a : *, a";
            let expected_ty = r"[a : *] -> *";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn pi() {
            let ctx = Context::default();
            let given_expr = r"[a : *] -> a";
            let expected_ty = r"*";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id() {
            let ctx = Context::default();
            let given_expr = r"\a : *, \x : a, x";
            let expected_ty = r"[a : *] -> a -> a";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id_ann() {
            let ctx = Context::default();
            let given_expr = r"(\a, \x : a, x) : [A : *] -> A -> A";
            let expected_ty = r"[a : *] -> a -> a";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id_app_arr_ty() {
            let ctx = Context::default();
            let given_expr = r"(\a : *, \x : a, x) * (* -> *)";
            let expected_ty = r"* -> *";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn apply() {
            let ctx = Context::default();
            let given_expr = r"
                \a : *, \b : *,
                    \f : (a -> b), \x : a, f x
            ";
            let expected_ty = r"
                [a : *] -> [b : *] ->
                    (a -> b) -> a -> b
            ";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn constant() {
            let ctx = Context::default();
            let given_expr = r"
                \a : *, \b : *,
                    \f : (a -> b), \x : a, \y : b, x
            ";
            let expected_ty = r"
                [a : *] -> [b : *] ->
                    (a -> b) -> a -> b -> a
            ";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn compose() {
            let ctx = Context::default();
            let given_expr = r"
                \a : *, \b : *, \c : *,
                    \f : (b -> c), \g : (a -> b), \x : a,
                        f (g x)
            ";
            let expected_ty = r"
                [a : *] -> [b : *] -> [c : *] ->
                    (b -> c) -> (a -> b) -> (a -> c)
            ";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }
    }
}
