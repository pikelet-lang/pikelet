//! Contexts and type checking

use rpds::List;

use core::{Module, RcTerm, RcType, RcValue, Term, Value};
use var::{Debruijn, Name, Named, Var};

pub struct CheckedModule {
    pub name: String,
    pub definitions: Vec<Definition>,
}

pub struct Definition {
    pub name: String,
    pub term: RcValue,
    pub ann: RcType,
}

pub fn check_module(module: &Module) -> Result<CheckedModule, TypeError> {
    let mut context = Context::new();
    let mut definitions = Vec::with_capacity(module.definitions.len());

    for definition in &module.definitions {
        let ann = match definition.ann {
            None => context.infer(&definition.term)?,
            Some(ref ann) => {
                let ann = context.eval(&ann);
                context.check(&definition.term, &ann)?;
                ann
            },
        };
        let term = context.eval(&definition.term);
        let name = definition.name.clone();
        context = context.extend(Binder::Let(term.clone(), ann.clone()));

        definitions.push(Definition { name, term, ann })
    }

    Ok(CheckedModule {
        name: module.name.clone(),
        definitions,
    })
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    IllegalApplication,
    TypeAnnotationsNeeded,
    ExpectedFunction {
        lam_expr: RcTerm,
        expected: RcType,
    },
    Mismatch {
        expr: RcTerm,
        found: RcType,
        expected: RcType,
    },
    UnboundVariable(Name),
}

/// A binder that introduces a variable into the context
#[derive(Debug, Clone)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam(Option<RcType>),
    /// A type introduced after entering a pi type
    Pi(RcType),
    /// A value and type binding that was introduced by passing over a let binding
    Let(RcValue, RcType),
}

impl Binder {
    /// Return the value associated with a binder
    pub fn value(&self) -> Option<&RcValue> {
        match *self {
            Binder::Lam(_) | Binder::Pi(_) => None,
            Binder::Let(_, ref ty) => Some(ty),
        }
    }

    /// Return the type associated with a binder
    pub fn ty(&self) -> Option<&RcType> {
        match *self {
            Binder::Lam(ref ty) => ty.as_ref(),
            Binder::Pi(ref ty) | Binder::Let(_, ref ty) => Some(ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    // Γ ::= ε           1. empty context
    //     | Γ,x:τ       2. context extension
    binders: List<Binder>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    pub fn extend(&self, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front(binder),
        }
    }

    pub fn lookup_binder(&self, index: Debruijn) -> Option<&Binder> {
        self.binders.iter().nth(index.0 as usize)
    }

    /// Evaluation of core terms to normal forms
    pub fn eval(&self, term: &RcTerm) -> RcValue {
        // FIXME: judgements do not mention the context :/
        // e ⇓ v
        match *term.inner {
            //  1.  e ⇓ v
            // ────────────────── (EVAL/ANN)
            //      e : ρ ⇓ v
            Term::Ann(ref expr, _) => {
                self.eval(expr) // 1.
            },

            // ───────────── (EVAL/TYPE)
            //  Type ⇓ Type
            Term::Type => Value::Type.into(),

            Term::Var(ref var) => match *var {
                // ─────── (EVAL/Var)
                //  x ⇓ x
                Var::Free(_) => Value::Var(var.clone()).into(),
                Var::Bound(Named(_, index)) => {
                    // FIXME: Blegh
                    match self.lookup_binder(index) {
                        Some(binder) => match binder.value() {
                            Some(value) => value.clone(),
                            None => Value::Var(var.clone()).into(),
                        },
                        None => panic!("ICE: index {} out of bounds", index),
                    }
                },
            },

            //  1. e ⇓ v
            // ───────────────── (EVAL/LAM)
            //     λx.e ⇓ λx→v
            Term::Lam(Named(ref name, None), ref body_expr) => {
                let binder = Binder::Lam(None);
                let body_expr = self.extend(binder).eval(body_expr); // 1.

                Value::Lam(Named(name.clone(), None), body_expr).into()
            },

            //  1.  ρ ⇓ τ
            //  2.  e ⇓ v
            // ──────────────────────── (EVAL/LAM-ANN)
            //      λx:ρ→e ⇓ λx:τ→v
            Term::Lam(Named(ref name, Some(ref param_ty)), ref body_expr) => {
                let param_ty = self.eval(param_ty); // 1.
                let binder = Binder::Lam(Some(param_ty.clone()));
                let body_expr = self.extend(binder).eval(body_expr); // 2.

                Value::Lam(Named(name.clone(), Some(param_ty)), body_expr).into()
            },

            //  1.  ρ₁ ⇓ τ₁
            //  2.  ρ₂ ⇓ τ₂
            // ─────────────────────────── (EVAL/PI-ANN)
            //      (x:ρ₁)→ρ₂ ⇓ (x:τ₁)→τ₂
            Term::Pi(Named(ref name, ref param_ty), ref body_expr) => {
                let param_ty = self.eval(param_ty); // 1.
                let binder = Binder::Pi(param_ty.clone());
                let body_expr = self.extend(binder).eval(body_expr); // 2.

                Value::Pi(Named(name.clone(), param_ty), body_expr).into()
            },

            //  1.  e₁ ⇓ λx→v₁
            //  2.  v₁[x↦e₂] ⇓ v₂
            // ───────────────────── (EVAL/APP)
            //      e₁ e₂ ⇓ v₂
            Term::App(ref fn_expr, ref arg) => {
                let fn_expr = self.eval(fn_expr); // 1.
                let arg = self.eval(arg); // 2.

                match *fn_expr.inner {
                    Value::Lam(_, ref body) => body.instantiate0(&arg),
                    _ => Value::App(fn_expr.clone(), arg).into(),
                }
            },
        }
    }

    /// Check that the given term has the expected type
    pub fn check(&self, term: &RcTerm, expected: &RcType) -> Result<(), TypeError> {
        // Γ ⊢ e :↓ τ
        match *term.inner {
            //  1.  Γ, x:τ₁ ⊢ e :↓ τ₂
            // ─────────────────────────────── (CHECK/LAM)
            //      Γ ⊢ λx→e :↓ (x:τ₁)→τ₂
            Term::Lam(Named(_, None), ref body_expr) => match *expected.inner {
                Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                    self.extend(Binder::Pi(param_ty.clone()))
                        .check(body_expr, ret_ty) // 1.
                },
                _ => Err(TypeError::ExpectedFunction {
                    lam_expr: term.clone(),
                    expected: expected.clone(),
                }),
            },

            //  1.  Γ ⊢ e :↑ τ
            // ─────────────────── (CHECK/INFER)
            //      Γ ⊢ e :↓ τ
            _ => {
                let inferred_ty = self.infer(term)?; // 1.
                match &inferred_ty == expected {
                    true => Ok(()),
                    false => Err(TypeError::Mismatch {
                        expr: term.clone(),
                        found: inferred_ty,
                        expected: expected.clone(),
                    }),
                }
            },
        }
    }

    /// Infer the type of the given term
    pub fn infer(&self, term: &RcTerm) -> Result<RcType, TypeError> {
        // Γ ⊢ e :↑ τ
        match *term.inner {
            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ ⇓ τ
            //  3.  Γ ⊢ e :↓ τ
            // ───────────────────────── (INFER/ANN)
            //      Γ ⊢ (e : ρ) :↑ τ
            Term::Ann(ref expr, ref ty) => {
                self.check(ty, &Value::Type.into())?; // 1.
                let simp_ty = self.eval(&ty); // 2.
                self.check(expr, &simp_ty)?; // 3.
                Ok(simp_ty)
            },

            // ─────────────────── (INFER/TYPE)
            //  Γ ⊢ TYPE :↑ Type
            Term::Type => Ok(Value::Type.into()),

            Term::Lam(Named(_, None), _) => {
                // TODO: More error info
                Err(TypeError::TypeAnnotationsNeeded)
            },

            //  1.  Γ ⊢ ρ :↓ Type
            //  2.  ρ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ e :↑ τ₂
            // ─────────────────────────────── (INFER/LAM)
            //      Γ ⊢ λx:ρ→e :↑ (x:τ₁)→τ₂
            Term::Lam(Named(ref param_name, Some(ref param_ty)), ref body_expr) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.eval(&param_ty); // 2.
                let body_ty = self.extend(Binder::Pi(simp_param_ty.clone()))
                    .infer(body_expr)?; // 3.

                Ok(Value::Pi(Named(param_name.clone(), simp_param_ty), body_ty).into())
            },

            //  1.  Γ ⊢ ρ₁ :↓ Type
            //  2.  ρ₁ ⇓ τ₁
            //  3.  Γ, x:τ₁ ⊢ ρ₂ :↓ Type
            // ────────────────────────────── (INFER/PI)
            //      Γ ⊢ (x:ρ₁)→ρ₂ :↑ Type
            Term::Pi(Named(_, ref param_ty), ref body_ty) => {
                self.check(param_ty, &Value::Type.into())?; // 1.
                let simp_param_ty = self.eval(&param_ty); // 2.
                self.extend(Binder::Pi(simp_param_ty))
                    .check(body_ty, &Value::Type.into())?; // 3.
                Ok(Value::Type.into())
            },

            //  1.  Γ(x) = τ
            // ────────────────────────────── (INFER/VAR)
            //      Γ ⊢ x :↑ τ
            Term::Var(ref var) => match *var {
                Var::Free(ref name) => Err(TypeError::UnboundVariable(name.clone())),
                Var::Bound(Named(_, index)) => match self.lookup_binder(index) {
                    Some(binder) => match binder.ty() {
                        Some(ty) => Ok(ty.clone()), // 1.
                        None => Err(TypeError::TypeAnnotationsNeeded),
                    },
                    None => panic!("ICE: index {} out of bounds", index),
                },
            },

            //  1.  Γ ⊢ e₁ :↑ (x:τ₁)→τ₂
            //  2.  Γ ⊢ e₂ :↓ τ₁
            //  3.  τ₂[x↦e₂] ⇓ τ₃
            // ────────────────────────────── (INFER/APP)
            //      Γ ⊢ e₁ e₂ :↑ τ₃
            Term::App(ref fn_expr, ref arg_expr) => {
                let fn_type = self.infer(fn_expr)?; // 1.
                match *fn_type.inner {
                    Value::Pi(Named(_, ref param_ty), ref ret_ty) => {
                        self.check(arg_expr, param_ty)?; // 2.
                        let body_ty = ret_ty.instantiate0(&self.eval(&arg_expr)); // 3.
                        Ok(body_ty)
                    },
                    // TODO: More error info
                    _ => Err(TypeError::IllegalApplication),
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> RcTerm {
        RcTerm::from_parse(&src.parse().unwrap())
    }

    mod eval {
        use super::*;

        #[test]
        fn var() {
            let context = Context::new();

            let x = Name::user("x");

            assert_eq!(context.eval(&parse(r"x")), Value::Var(Var::Free(x)).into(),);
        }

        #[test]
        fn ty() {
            let context = Context::new();

            let ty: RcValue = Value::Type.into();

            assert_eq!(context.eval(&parse(r"Type")), ty);
        }

        #[test]
        fn lam() {
            let context = Context::new();

            let x = Name::user("x");
            let ty: RcValue = Value::Type.into();

            assert_eq!(
                context.eval(&parse(r"\x : Type => x")),
                Value::Lam(
                    Named(x.clone(), Some(ty)),
                    Value::Var(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            );
        }

        #[test]
        fn pi() {
            let context = Context::new();

            let x = Name::user("x");
            let ty: RcValue = Value::Type.into();

            assert_eq!(
                context.eval(&parse(r"(x : Type) -> x")),
                Value::Pi(
                    Named(x.clone(), ty),
                    Value::Var(Var::Bound(Named(x, Debruijn(0)))).into(),
                ).into(),
            );
        }

        #[test]
        fn lam_app() {
            let context = Context::new();

            let x = Name::user("x");
            let y = Name::user("y");
            let ty: RcValue = Value::Type.into();
            let ty_arr: RcValue = Value::Pi(Named(Name::Abstract, ty.clone()), ty.clone()).into();

            assert_eq!(
                context.eval(&parse(r"\x : Type -> Type => \y : Type => x y")),
                Value::Lam(
                    Named(x.clone(), Some(ty_arr)),
                    Value::Lam(
                        Named(y.clone(), Some(ty)),
                        Value::App(
                            Value::Var(Var::Bound(Named(x, Debruijn(1)))).into(),
                            Value::Var(Var::Bound(Named(y, Debruijn(0)))).into(),
                        ).into(),
                    ).into(),
                ).into(),
            );
        }

        #[test]
        fn pi_app() {
            let context = Context::new();

            let x = Name::user("x");
            let y = Name::user("y");
            let ty: RcValue = Value::Type.into();
            let ty_arr: RcValue = Value::Pi(Named(Name::Abstract, ty.clone()), ty.clone()).into();

            assert_eq!(
                context.eval(&parse(r"(x : Type -> Type) -> \y : Type => x y")),
                Value::Pi(
                    Named(x.clone(), ty_arr),
                    Value::Lam(
                        Named(y.clone(), Some(ty)),
                        Value::App(
                            Value::Var(Var::Bound(Named(x, Debruijn(1)))).into(),
                            Value::Var(Var::Bound(Named(y, Debruijn(0)))).into(),
                        ).into(),
                    ).into(),
                ).into(),
            );
        }
    }

    mod infer {
        use super::*;

        #[test]
        fn free() {
            let context = Context::new();

            let given_expr = r"x";
            let x = Name::user("x");

            assert_eq!(
                context.infer(&parse(given_expr)),
                Err(TypeError::UnboundVariable(x)),
            );
        }

        #[test]
        fn ty() {
            let context = Context::new();

            let given_expr = r"Type";
            let expected_ty = r"Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn ann_ty_id() {
            let context = Context::new();

            let given_expr = r"(\a => a) : Type -> Type";
            let expected_ty = r"Type -> Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn ann_arrow_ty_id() {
            let context = Context::new();

            let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";
            let expected_ty = r"(Type -> Type) -> (Type -> Type)";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn ann_id_as_ty() {
            let context = Context::new();

            let given_expr = r"(\a => a) : Type";

            match context.infer(&parse(given_expr)) {
                Err(TypeError::ExpectedFunction { .. }) => {},
                other => panic!("unexpected result: {:#?}", other),
            }
        }

        #[test]
        fn app() {
            let context = Context::new();

            let given_expr = r"(\a : Type => a) Type";
            let expected_ty = r"Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn app_ty() {
            let context = Context::new();

            let given_expr = r"Type Type";

            assert_eq!(
                context.infer(&parse(given_expr)),
                Err(TypeError::IllegalApplication),
            )
        }

        #[test]
        fn lam() {
            let context = Context::new();

            let given_expr = r"\a : Type => a";
            let expected_ty = r"(a : Type) -> Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn pi() {
            let context = Context::new();

            let given_expr = r"(a : Type) -> a";
            let expected_ty = r"Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn id() {
            let context = Context::new();

            let given_expr = r"\a : Type => \x : a => x";
            let expected_ty = r"(a : Type) -> a -> a";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn id_ann() {
            let context = Context::new();

            let given_expr = r"(\a => \x : a => x) : (A : Type) -> A -> A";
            let expected_ty = r"(a : Type) -> a -> a";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn id_app_ty_arr_ty() {
            let context = Context::new();

            let given_expr = r"(\a : Type => \x : a => x) Type (Type -> Type)";
            let expected_ty = r"Type -> Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn id_app_arr_pi_ty() {
            let context = Context::new();

            let given_expr = r"(\a : Type => \x : a => x) (Type -> Type) (\x : Type => Type)";
            let expected_ty = r"\x : Type => Type";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn apply() {
            let context = Context::new();

            let given_expr = r"
                \a : Type => \b : Type =>
                    \f : (a -> b) => \x : a => f x
            ";
            let expected_ty = r"
                (a : Type) -> (b : Type) ->
                    (a -> b) -> a -> b
            ";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn const_() {
            let context = Context::new();

            let given_expr = r"\a : Type => \b : Type => \x : a => \y : b => x";
            let expected_ty = r"(a : Type) -> (b : Type) -> a -> b -> a";

            assert_eq!(
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        #[test]
        fn compose() {
            let context = Context::new();

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
                context.infer(&parse(given_expr)).unwrap(),
                context.eval(&parse(expected_ty)),
            );
        }

        mod church_encodings {
            use super::*;

            #[test]
            fn and() {
                let context = Context::new();

                let given_expr = r"\p : Type => \q : Type => (c : Type) -> (p -> q -> c) -> c";
                let expected_ty = r"Type -> Type -> Type";

                assert_eq!(
                    context.infer(&parse(given_expr)).unwrap(),
                    context.eval(&parse(expected_ty)),
                );
            }

            #[test]
            fn and_intro() {
                let context = Context::new();

                let given_expr = r"
                    \p : Type => \q : Type => \x : p => \y : q =>
                        \c : Type => \f : (p -> q -> c) => f x y
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) -> p -> q ->
                        ((c : Type) -> (p -> q -> c) -> c)
                ";

                assert_eq!(
                    context.infer(&parse(given_expr)).unwrap(),
                    context.eval(&parse(expected_ty)),
                );
            }

            #[test]
            fn and_proj_left() {
                let context = Context::new();

                let given_expr = r"
                    \p : Type => \q : Type => \pq : (c : Type) -> (p -> q -> c) -> c =>
                        pq p (\x => \y => x)
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) ->
                        ((c : Type) -> (p -> q -> c) -> c) -> p
                ";

                assert_eq!(
                    context.infer(&parse(given_expr)).unwrap(),
                    context.eval(&parse(expected_ty)),
                );
            }

            #[test]
            fn and_proj_right() {
                let context = Context::new();

                let given_expr = r"
                    \p : Type => \q : Type => \pq : (c : Type) -> (p -> q -> c) -> c =>
                        pq q (\x => \y => y)
                ";
                let expected_ty = r"
                    (p : Type) -> (q : Type) ->
                        ((c : Type) -> (p -> q -> c) -> c) -> q
                ";

                assert_eq!(
                    context.infer(&parse(given_expr)).unwrap(),
                    context.eval(&parse(expected_ty)),
                );
            }
        }
    }

    mod check_module {
        use super::*;

        #[test]
        fn check_prelude() {
            let module = Module::from_parse(&include_str!("../prelude.lp").parse().unwrap());

            check_module(&module).unwrap();
        }
    }
}
