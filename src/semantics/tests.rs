use super::*;

fn parse(src: &str) -> RcTerm {
    RcTerm::from_concrete(&src.parse().unwrap())
}

mod normalize {
    use super::*;

    #[test]
    fn var() {
        let context = Context::new();

        let x = Name::user("x");

        assert_eq!(
            normalize(&context, &parse(r"x")),
            Err(InternalError::UndefinedName(x)),
        );
    }

    #[test]
    fn ty() {
        let context = Context::new();

        assert_eq!(
            normalize(&context, &parse(r"Type")).unwrap(),
            Value::Universe(Level::ZERO).into()
        );
    }

    #[test]
    fn lam() {
        let context = Context::new();

        let x = Name::user("x");

        assert_eq!(
            normalize(&context, &parse(r"\x : Type => x")).unwrap(),
            Value::Lam(ValueLam::bind(
                Named::new(x.clone(), Some(Value::Universe(Level::ZERO).into())),
                Value::Var(Var::Free(x)).into(),
            )).into(),
        );
    }

    #[test]
    fn pi() {
        let context = Context::new();

        let x = Name::user("x");

        assert_eq!(
            normalize(&context, &parse(r"(x : Type) -> x")).unwrap(),
            Value::Pi(ValuePi::bind(
                Named::new(x.clone(), Value::Universe(Level::ZERO).into()),
                Value::Var(Var::Free(x)).into(),
            )).into(),
        );
    }

    #[test]
    fn lam_app() {
        let context = Context::new();

        let x = Name::user("x");
        let y = Name::user("y");
        let ty_arr: RcValue = Value::Pi(ValuePi::bind(
            Named::new(Name::user("_"), Value::Universe(Level::ZERO).into()),
            Value::Universe(Level::ZERO).into(),
        )).into();

        assert_eq!(
            normalize(&context, &parse(r"\(x : Type -> Type) (y : Type) => x y")).unwrap(),
            Value::Lam(ValueLam::bind(
                Named::new(x.clone(), Some(ty_arr)),
                Value::Lam(ValueLam::bind(
                    Named::new(y.clone(), Some(Value::Universe(Level::ZERO).into())),
                    Value::App(
                        Value::Var(Var::Free(x)).into(),
                        Value::Var(Var::Free(y)).into(),
                    ).into(),
                )).into(),
            )).into(),
        );
    }

    #[test]
    fn pi_app() {
        let context = Context::new();

        let x = Name::user("x");
        let y = Name::user("y");
        let ty_arr: RcValue = Value::Pi(ValuePi::bind(
            Named::new(Name::user("_"), Value::Universe(Level::ZERO).into()),
            Value::Universe(Level::ZERO).into(),
        )).into();

        assert_eq!(
            normalize(&context, &parse(r"(x : Type -> Type) -> \y : Type => x y")).unwrap(),
            Value::Pi(ValuePi::bind(
                Named::new(x.clone(), ty_arr),
                Value::Lam(ValueLam::bind(
                    Named::new(y.clone(), Some(Value::Universe(Level::ZERO).into())),
                    Value::App(
                        Value::Var(Var::Free(x)).into(),
                        Value::Var(Var::Free(y)).into(),
                    ).into(),
                )).into(),
            )).into(),
        );
    }

    // Passing `Type` to the polymorphic identity function should yeild the type
    // identity function
    #[test]
    fn id_app_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type) (x : a) => x) Type";
        let expected_expr = r"\x : Type => x";

        assert_eq!(
            normalize(&context, &parse(given_expr)).unwrap(),
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    // Passing `Type` to the `Type` identity function should yeild `Type`
    #[test]
    fn id_app_ty_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type) (x : a) => x) Type Type";
        let expected_expr = r"Type";

        assert_eq!(
            normalize(&context, &parse(given_expr)).unwrap(),
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    // Passing `Type -> Type` to the `Type` identity function should yeild
    // `Type -> Type`
    #[test]
    fn id_app_ty_arr_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type) (x : a) => x) Type (Type -> Type)";
        let expected_expr = r"Type -> Type";

        assert_eq!(
            normalize(&context, &parse(given_expr)).unwrap(),
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    // Passing the id function to itself should yield the id function
    #[test]
    fn id_app_id() {
        let context = Context::new();

        let given_expr = r"
            (\(a : Type) (x : a) => x)
                ((a : Type) -> a -> a)
                (\(a : Type) (x : a) => x)
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_eq!(
            normalize(&context, &parse(given_expr)).unwrap(),
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    // Passing the id function to the 'const' combinator should yeild a
    // function that always returns the id function
    #[test]
    fn const_app_id_ty() {
        let context = Context::new();

        let given_expr = r"
            (\(a b : Type) (x : a) (y : b) => x)
                ((a : Type) -> a -> a)
                Type
                (\(a : Type) (x : a) => x)
                Type
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_eq!(
            normalize(&context, &parse(given_expr)).unwrap(),
            normalize(&context, &parse(expected_expr)).unwrap(),
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
            infer(&context, &parse(given_expr)),
            Err(TypeError::UndefinedName(x)),
        );
    }

    #[test]
    fn ty() {
        let context = Context::new();

        let given_expr = r"Type";
        let expected_ty = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ty_levels() {
        let context = Context::new();

        let given_expr = r"Type 0 : Type 1 : Type 2 : Type 3"; //... Type ∞       ...+:｡(ﾉ･ω･)ﾉﾞ
        let expected_ty = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ann_ty_id() {
        let context = Context::new();

        let given_expr = r"(\a => a) : Type -> Type";
        let expected_ty = r"Type -> Type";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ann_arrow_ty_id() {
        let context = Context::new();

        let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";
        let expected_ty = r"(Type -> Type) -> (Type -> Type)";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ann_id_as_ty() {
        let context = Context::new();

        let given_expr = r"(\a => a) : Type";

        match infer(&context, &parse(given_expr)) {
            Err(TypeError::ExpectedFunction { .. }) => {},
            other => panic!("unexpected result: {:#?}", other),
        }
    }

    #[test]
    fn app() {
        let context = Context::new();

        let given_expr = r"(\a : Type 1 => a) Type";
        let expected_ty = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn app_ty() {
        let context = Context::new();

        let given_expr = r"Type Type";

        assert_eq!(
            infer(&context, &parse(given_expr)),
            Err(TypeError::NotAFunctionType {
                expr: Term::Universe(Level::ZERO).into(),
                found: Value::Universe(Level::ZERO.succ()).into(),
            }),
        )
    }

    #[test]
    fn lam() {
        let context = Context::new();

        let given_expr = r"\a : Type => a";
        let expected_ty = r"(a : Type) -> Type";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn pi() {
        let context = Context::new();

        let given_expr = r"(a : Type) -> a";
        let expected_ty = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id() {
        let context = Context::new();

        let given_expr = r"\(a : Type) (x : a) => x";
        let expected_ty = r"(a : Type) -> a -> a";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id_ann() {
        let context = Context::new();

        let given_expr = r"(\a (x : a) => x) : (A : Type) -> A -> A";
        let expected_ty = r"(a : Type) -> a -> a";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    // Passing `Type` to the polymorphic identity function should yeild the type
    // identity function
    #[test]
    fn id_app_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
        let expected_expr = r"Type -> Type";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    // Passing `Type` to the `Type` identity function should yeild `Type`
    #[test]
    fn id_app_ty_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
        let expected_expr = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_expr)).unwrap(),
        );
    }

    #[test]
    fn id_app_ty_arr_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
        let expected_ty = r"Type 1";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id_app_arr_pi_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 1) (x : a) => x) (Type -> Type) (\x => x)";
        let expected_ty = r"Type -> Type";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn apply() {
        let context = Context::new();

        let given_expr = r"\(a b : Type) (f : a -> b) (x : a) => f x";
        let expected_ty = r"(a b : Type) -> (a -> b) -> a -> b";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn const_() {
        let context = Context::new();

        let given_expr = r"\(a b : Type) (x : a) (y : b) => x";
        let expected_ty = r"(a b : Type) -> a -> b -> a";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn const_flipped() {
        let context = Context::new();

        let given_expr = r"\(a b : Type) (x : a) (y : b) => y";
        let expected_ty = r"(a b : Type) -> a -> b -> b";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn flip() {
        let context = Context::new();

        let given_expr = r"\(a b c : Type) (f : a -> b -> c) (y : b) (x : a) => f x y";
        let expected_ty = r"(a b c : Type) -> (a -> b -> c) -> (b -> a -> c)";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn compose() {
        let context = Context::new();

        let given_expr = r"\(a b c : Type) (f : b -> c) (g : a -> b) (x : a) => f (g x)";
        let expected_ty = r"(a b c : Type) -> (b -> c) -> (a -> b) -> (a -> c)";

        assert_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse(expected_ty)).unwrap(),
        );
    }

    mod church_encodings {
        use super::*;

        #[test]
        fn and() {
            let context = Context::new();

            let given_expr = r"\(p q : Type) => (c : Type) -> (p -> q -> c) -> c";
            let expected_ty = r"Type -> Type -> Type 1";

            assert_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse(expected_ty)).unwrap(),
            );
        }

        #[test]
        fn and_intro() {
            let context = Context::new();

            let given_expr = r"
                \(p q : Type) (x : p) (y : q) =>
                    \c : Type => \f : (p -> q -> c) => f x y
            ";
            let expected_ty = r"
                (p q : Type) -> p -> q ->
                    ((c : Type) -> (p -> q -> c) -> c)
            ";

            assert_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse(expected_ty)).unwrap(),
            );
        }

        #[test]
        fn and_proj_left() {
            let context = Context::new();

            let given_expr = r"
                \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                    pq p (\x y => x)
            ";
            let expected_ty = r"
                (p q : Type) ->
                    ((c : Type) -> (p -> q -> c) -> c) -> p
            ";

            assert_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse(expected_ty)).unwrap(),
            );
        }

        #[test]
        fn and_proj_right() {
            let context = Context::new();

            let given_expr = r"
                \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                    pq q (\x y => y)
            ";
            let expected_ty = r"
                (p q : Type) -> ((c : Type) -> (p -> q -> c) -> c) -> q
            ";

            assert_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse(expected_ty)).unwrap(),
            );
        }
    }
}

mod check_module {
    use super::*;

    #[test]
    fn check_prelude() {
        let module = Module::from_concrete(&include_str!("../../prelude.lp").parse().unwrap());

        check_module(&module).unwrap();
    }
}
