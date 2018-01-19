//! Contexts and type checking

use core::{CTerm, EvalError, ITerm, RcCTerm, RcITerm, RcType, RcValue, Value};
use var::{Debruijn, Name, Named, Var};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    Eval(EvalError),
    IllegalApplication,
    ExpectedFunction {
        lam_expr: RcCTerm,
        expected: RcType,
    },
    Mismatch {
        expr: RcITerm,
        found: RcType,
        expected: RcType,
    },
    UnboundVariable(Name),
}

impl From<EvalError> for TypeError {
    fn from(src: EvalError) -> TypeError {
        TypeError::Eval(src)
    }
}

/// Contexts
pub enum Context<'a> {
    // Γ ::= ε           1. empty context
    //     | Γ,x:τ       2. context extension
    Empty,                         // 1.
    Cons(&'a Context<'a>, RcType), // 2.
}

impl Default for Context<'static> {
    fn default() -> Context<'static> {
        Context::Empty
    }
}

impl<'a> Context<'a> {
    pub fn extend(&'a self, ty: RcType) -> Context<'a> {
        Context::Cons(self, ty)
    }

    pub fn lookup_ty(&'a self, x: Debruijn) -> Option<&'a RcType> {
        match (self, x) {
            (&Context::Empty, _) => None,
            (&Context::Cons(parent, ref value), x) => match x.pred() {
                None => Some(value),
                Some(x) => parent.lookup_ty(x),
            },
        }
    }

    /// Check that the type of an expression is compatible with the expected type
    pub fn check(&self, expr: &RcCTerm, expected_ty: &RcType) -> Result<(), TypeError> {
        // Γ ⊢ e :↓ τ
        match *expr.inner {
            //  1.  Γ ⊢ e :↑ τ
            // ─────────────────── (CHECK/INFER)
            //      Γ ⊢ e :↓ τ
            CTerm::Inf(ref inferrable_expr) => {
                let inferred_ty = self.infer(inferrable_expr)?; // 1.
                match &inferred_ty == expected_ty {
                    true => Ok(()),
                    false => Err(TypeError::Mismatch {
                        expr: inferrable_expr.clone(),
                        found: inferred_ty,
                        expected: expected_ty.clone(),
                    }),
                }
            }

            //  1.  Γ, x:τ₁ ⊢ e :↓ τ₂
            // ─────────────────────────────── (CHECK/LAM)
            //      Γ ⊢ λx→e :↓ (x:τ₁)→τ₂
            CTerm::Lam(_, ref body_expr) => match *expected_ty.inner {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    self.extend(param_ty.clone()).check(body_expr, ret_ty) // 1.
                }
                _ => Err(TypeError::ExpectedFunction {
                    lam_expr: expr.clone(),
                    expected: expected_ty.clone(),
                }),
            },
        }
    }

    pub fn infer(&self, expr: &RcITerm) -> Result<RcType, TypeError> {
        // Γ ⊢ e :↑ τ
        match *expr.inner {
            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ ⇓ τ
            //  3.  Γ ⊢ e :↓ τ
            // ───────────────────────── (INFER/ANN)
            //      Γ ⊢ (e : ρ) :↑ τ
            ITerm::Ann(ref expr, ref ty) => {
                self.check(ty, &Value::Type.into())?; // 1.
                let simp_ty = ty.eval()?; // 2.
                self.check(expr, &simp_ty)?; // 3.
                Ok(simp_ty)
            }

            // ─────────────────── (INFER/TYPE)
            //  Γ ⊢ TYPE :↑ Type
            ITerm::Type => Ok(Value::Type.into()),

            //  1.  Γ ⊢ ρ :↓ Type
            //  2.  ρ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ e :↑ τ₂
            // ─────────────────────────────── (INFER/LAM)
            //      Γ ⊢ λx:ρ→e :↑ (x:τ₁)→τ₂
            ITerm::Lam(Named(ref param_name, ref param_ty), ref body_expr) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = param_ty.eval()?; // 2.
                let body_ty = self.extend(simp_param_ty.clone()).infer(body_expr)?; // 3.

                Ok(
                    Value::Pi(
                        Named(param_name.clone(), simp_param_ty),
                        body_ty, // shift??
                    ).into(),
                )
            }

            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ₁ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ ρ₂ :↓ Type
            // ────────────────────────────── (INFER/PI)
            //      Γ ⊢ (x:ρ₁)→ρ₂ :↑ Type
            ITerm::Pi(Named(_, ref param_ty), ref body_ty) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = param_ty.eval()?; // 2.
                self.extend(simp_param_ty)
                    .check(body_ty, &Value::Type.into())?; // 3.
                Ok(Value::Type.into())
            }

            //  1.  Γ(x) = τ
            // ────────────────────────────── (INFER/VAR)
            //      Γ ⊢ x :↑ τ
            ITerm::Var(ref var) => match *var {
                Var::Free(ref name) => Err(TypeError::UnboundVariable(name.clone())),
                Var::Bound(Named(_, b)) => match self.lookup_ty(b) {
                    Some(ty) => Ok(ty.clone()), // 1.
                    None => panic!("ICE: index out of bounds"),
                },
            },

            //  1.  Γ ⊢ e₁ :↑ (x:τ₁)→τ₂
            //  2.  Γ ⊢ e₂ :↓ τ₁
            //  3.  τ₂[x↦e₂] ⇓ τ₃
            // ────────────────────────────── (INFER/APP)
            //      Γ ⊢ e₁ e₂ :↑ τ₃
            ITerm::App(ref fn_expr, ref arg_expr) => {
                let fn_type = self.infer(fn_expr)?; // 1.
                match *fn_type.inner {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        self.check(arg_expr, param_ty)?; // 2.
                        let body_ty = RcValue::instantiate0(ret_ty, &arg_expr.eval()?)?; // 3.
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
    use core::Neutral;

    fn parse(src: &str) -> RcITerm {
        RcITerm::from_parse(&src.parse().unwrap()).unwrap()
    }

    #[test]
    fn extend_lookup_ty() {
        let x: RcValue = Value::from(Neutral::Var(Var::Free(Name::user("x")))).into();
        let y: RcValue = Value::from(Neutral::Var(Var::Free(Name::user("y")))).into();

        let context0 = Context::Empty;

        assert_eq!(context0.lookup_ty(Debruijn(0)), None);

        let context1 = context0.extend(x.clone());

        assert_eq!(context1.lookup_ty(Debruijn(0)), Some(&x));
        assert_eq!(context1.lookup_ty(Debruijn(1)), None);

        let context2 = context1.extend(y.clone());

        assert_eq!(context2.lookup_ty(Debruijn(0)), Some(&y));
        assert_eq!(context2.lookup_ty(Debruijn(1)), Some(&x));
        assert_eq!(context2.lookup_ty(Debruijn(2)), None);
    }

    mod infer {
        use super::*;

        #[test]
        fn free() {
            let ctx = Context::default();

            let given_expr = r"x";
            let x = Name::user("x");

            assert_eq!(
                ctx.infer(&parse(given_expr)),
                Err(TypeError::UnboundVariable(x)),
            );
        }

        #[test]
        fn ty() {
            let ctx = Context::default();

            let given_expr = r"Type";
            let expected_ty = r"Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_ty_id() {
            let ctx = Context::default();

            let given_expr = r"(\a => a) : Type -> Type";
            let expected_ty = r"Type -> Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_arrow_ty_id() {
            let ctx = Context::default();

            let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";
            let expected_ty = r"(Type -> Type) -> (Type -> Type)";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn ann_id_as_ty() {
            let ctx = Context::default();

            let given_expr = r"(\a => a) : Type";

            match ctx.infer(&parse(given_expr)) {
                Err(TypeError::ExpectedFunction { .. }) => {}
                other => panic!("unexpected result: {:#?}", other),
            }
        }

        #[test]
        fn app() {
            let ctx = Context::default();

            let given_expr = r"(\a : Type => a) Type";
            let expected_ty = r"Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn app_ty() {
            let ctx = Context::default();

            let given_expr = r"Type Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)),
                Err(TypeError::IllegalApplication),
            )
        }

        #[test]
        fn lam() {
            let ctx = Context::default();

            let given_expr = r"\a : Type => a";
            let expected_ty = r"(a : Type) -> Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn pi() {
            let ctx = Context::default();

            let given_expr = r"(a : Type) -> a";
            let expected_ty = r"Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id() {
            let ctx = Context::default();

            let given_expr = r"\a : Type => \x : a => x";
            let expected_ty = r"(a : Type) -> a -> a";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id_ann() {
            let ctx = Context::default();

            let given_expr = r"(\a => \x : a => x) : (A : Type) -> A -> A";
            let expected_ty = r"(a : Type) -> a -> a";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id_app_ty_arr_ty() {
            let ctx = Context::default();

            let given_expr = r"(\a : Type => \x : a => x) Type (Type -> Type)";
            let expected_ty = r"Type -> Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn id_app_arr_pi_ty() {
            let ctx = Context::default();

            let given_expr = r"(\a : Type => \x : a => x) (Type -> Type) (\x : Type => Type)";
            let expected_ty = r"\x : Type => Type";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn apply() {
            let ctx = Context::default();

            let given_expr = r"
                \a : Type => \b : Type =>
                    \f : (a -> b) => \x : a => f x
            ";
            let expected_ty = r"
                (a : Type) -> (b : Type) ->
                    (a -> b) -> a -> b
            ";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn const_() {
            let ctx = Context::default();

            let given_expr = r"\a : Type => \b : Type => \x : a => \y : b => x";
            let expected_ty = r"(a : Type) -> (b : Type) -> a -> b -> a";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        #[test]
        fn compose() {
            let ctx = Context::default();

            let given_expr = r"
                \a : Type => \b : Type => \c : Type =>
                    \f : (b -> c) => \g : (a -> b) => \x : a =>
                        f (g x)
            ";
            let expected_ty = r"
                (a : Type) -> (b : Type) -> (c : Type) ->
                    (b -> c) -> (a -> b) -> (a -> c)
            ";

            assert_eq!(
                ctx.infer(&parse(given_expr)).unwrap(),
                parse(expected_ty).eval().unwrap(),
            );
        }

        mod church_encodings {
            use super::*;

            #[test]
            fn and() {
                let ctx = Context::default();

                let given_expr = r"\p : Type => \q : Type => (c : Type) -> (p -> q -> c) -> c";
                let expected_ty = r"Type -> Type -> Type";

                assert_eq!(
                    ctx.infer(&parse(given_expr)).unwrap(),
                    parse(expected_ty).eval().unwrap(),
                );
            }

            #[test]
            fn and_intro() {
                let ctx = Context::default();

                let given_expr = r"
                    \p : Type => \q : Type => \x : p => \y : q =>
                        \c : Type => \f : (p -> q -> c) => f x y
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) -> p -> q ->
                        ((c : Type) -> (p -> q -> c) -> c)
                ";

                assert_eq!(
                    ctx.infer(&parse(given_expr)).unwrap(),
                    parse(expected_ty).eval().unwrap(),
                );
            }

            #[test]
            fn and_proj_left() {
                let ctx = Context::default();

                let given_expr = r"
                    \p : Type => \q : Type => \pq : (c : Type) -> (p -> q -> c) -> c =>
                        pq p (\x => \y => x)
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) ->
                        ((c : Type) -> (p -> q -> c) -> c) -> p
                ";

                assert_eq!(
                    ctx.infer(&parse(given_expr)).unwrap(),
                    parse(expected_ty).eval().unwrap(),
                );
            }

            #[test]
            fn and_proj_right() {
                let ctx = Context::default();

                let given_expr = r"
                    \p : Type => \q : Type => \pq : (c : Type) -> (p -> q -> c) -> c =>
                        pq q (\x => \y => y)
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) ->
                        ((c : Type) -> (p -> q -> c) -> c) -> q
                ";

                assert_eq!(
                    ctx.infer(&parse(given_expr)).unwrap(),
                    parse(expected_ty).eval().unwrap(),
                );
            }
        }
    }
}
