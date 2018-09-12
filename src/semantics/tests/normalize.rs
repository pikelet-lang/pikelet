use super::*;

mod nf_term {
    use syntax::core::{Head, Neutral, RcNeutral};

    use super::*;

    #[test]
    fn var() {
        let tc_env = TcEnv::default();

        let x = FreeVar::fresh_named("x");
        let var = RcTerm::from(Term::Var(Var::Free(x.clone())));

        assert_eq!(
            nf_term(&tc_env, &var).unwrap(),
            RcValue::from(Value::from(Var::Free(x))),
        );
    }

    #[test]
    fn ty() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        assert_eq!(
            parse_nf_term(&mut codemap, &tc_env, r"Type"),
            RcValue::from(Value::universe(0)),
        );
    }

    #[test]
    fn lam() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, r"\x : Type => x"),
            RcValue::from(Value::Lam(Scope::new(
                (Binder(x.clone()), Embed(RcValue::from(Value::universe(0)))),
                RcValue::from(Value::from(Var::Free(x))),
            ))),
        );
    }

    #[test]
    fn pi() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, r"(x : Type) -> x"),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(x.clone()), Embed(RcValue::from(Value::universe(0)))),
                RcValue::from(Value::from(Var::Free(x))),
            ))),
        );
    }

    #[test]
    fn lam_app() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"\(x : Type -> Type) (y : Type) => x y";

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let ty_arr = RcValue::from(Value::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(RcValue::from(Value::universe(0))),
            ),
            RcValue::from(Value::universe(0)),
        )));

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr,),
            RcValue::from(Value::Lam(Scope::new(
                (Binder(x.clone()), Embed(ty_arr)),
                RcValue::from(Value::Lam(Scope::new(
                    (Binder(y.clone()), Embed(RcValue::from(Value::universe(0)))),
                    RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Head(Head::Var(Var::Free(x)))),
                        vec![RcValue::from(Value::from(Var::Free(y)))],
                    )),
                ))),
            ))),
        );
    }

    #[test]
    fn pi_app() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(x : Type -> Type) -> (y : Type) -> x y";

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let ty_arr = RcValue::from(Value::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(RcValue::from(Value::universe(0))),
            ),
            RcValue::from(Value::universe(0)),
        )));

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(x.clone()), Embed(ty_arr)),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(y.clone()), Embed(RcValue::from(Value::universe(0)))),
                    RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Head(Head::Var(Var::Free(x)))),
                        vec![RcValue::from(Value::from(Var::Free(y)))],
                    )),
                ))),
            ))),
        );
    }

    // Passing `Type` to the polymorphic identity function should yield the type
    // identity function
    #[test]
    fn id_app_ty() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
        let expected_expr = r"\x : Type => x";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    // Passing `Type` to the `Type` identity function should yield `Type`
    #[test]
    fn id_app_ty_ty() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
        let expected_expr = r"Type";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    // Passing `Type -> Type` to the `Type` identity function should yield
    // `Type -> Type`
    #[test]
    fn id_app_ty_arr_ty() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
        let expected_expr = r"Type -> Type";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    // Passing the id function to itself should yield the id function
    #[test]
    fn id_app_id() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"
            (\(a : Type 1) (x : a) => x)
                ((a : Type) -> a -> a)
                (\(a : Type) (x : a) => x)
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    // Passing the id function to the 'const' combinator should yield a
    // function that always returns the id function
    #[test]
    fn const_app_id_ty() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"
            (\(a : Type 1) (b : Type 2) (x : a) (y : b) => x)
                ((a : Type) -> a -> a)
                (Type 1)
                (\(a : Type) (x : a) => x)
                Type
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn horrifying_app_1() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(\(t : Type) (f : (a : Type) -> Type) => f t) String (\(a : Type) => a)";
        let expected_expr = r"String";

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn horrifying_app_2() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"(\(t: String) (f: String -> String) => f t) "hello""#;
        let expected_expr = r#"\(f : String -> String) => f "hello""#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn let_expr_1() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"
            let x = "helloo";
            in
                x
        "#;
        let expected_expr = r#"
            "helloo"
        "#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn let_expr_2() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"
            let x = "helloo";
                y = x;
            in
                x
        "#;
        let expected_expr = r#"
            "helloo"
        "#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn if_true() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"
            if true then "true" else "false"
        "#;
        let expected_expr = r#"
            "true"
        "#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn if_false() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"
            if false then "true" else "false"
        "#;
        let expected_expr = r#"
            "false"
        "#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn if_eval_cond() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r#"
            let is-hi (greeting : String) = case greeting of {
                    "hi" => true;
                    _ => false;
                };
            in
                record {
                    test-hi = if is-hi "hi" then "true" else "false";
                    test-bye = if is-hi "bye" then "true" else "false";
                }
        "#;
        let expected_expr = r#"
            record {
                test-hi = "true";
                test-bye = "false";
            }
        "#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }

    #[test]
    fn record_shadow() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let given_expr = r"(\t : Type => Record { String : Type; x : t; y : String }) String";
        let expected_expr = r#"Record { String as String1 : Type; x : String; y : String1 }"#;

        assert_term_eq!(
            parse_nf_term(&mut codemap, &tc_env, given_expr),
            parse_nf_term(&mut codemap, &tc_env, expected_expr),
        );
    }
}
