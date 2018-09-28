use super::*;

#[test]
fn prelude() {
    use library;

    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), library::PRELUDE.into());
    let writer = StandardStream::stdout(ColorChoice::Always);

    let (concrete_term, _import_paths, errors) = parse::term(&filemap);
    if !errors.is_empty() {
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());
    let raw_term = match concrete_term.desugar(&desugar_env) {
        Ok(raw_term) => raw_term,
        Err(err) => {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
            panic!("desugar error!")
        },
    };

    if let Err(err) = infer_term(&tc_env, &raw_term) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn undefined_name() {
    use syntax::LevelShift;

    let tc_env = TcEnv::default();

    let x = FreeVar::fresh_named("x");
    let given_expr = raw::RcTerm::from(raw::Term::Var(
        ByteSpan::default(),
        Var::Free(x.clone()),
        LevelShift(0),
    ));

    assert_eq!(
        infer_term(&tc_env, &given_expr),
        Err(TypeError::UndefinedName {
            span: ByteSpan::default(),
            free_var: x.clone(),
        }),
    );
}

#[test]
fn extern_not_found() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"extern "does-not-exist" : Record {}"#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::UndefinedExternName { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}

#[test]
fn ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ty_levels() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"Type^0 : Type^1 : Type^2 : Type^3"; //... Type^∞       ...+:｡(ﾉ･ω･)ﾉﾞ

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_ty_id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\a => a) : Type -> Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_arrow_ty_id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(Type -> Type) -> (Type -> Type)";
    let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_id_as_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r"(\a => a) : Type";

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::UnexpectedFunction { .. }) => {},
        other => panic!("unexpected result: {:#?}", other),
    }
}

#[test]
fn app() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"(\a : Type^1 => a) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn app_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r"Type Type";

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    assert_eq!(
        infer_term(&tc_env, &raw_term),
        Err(TypeError::ArgAppliedToNonFunction {
            fn_span: ByteSpan::new(ByteIndex(1), ByteIndex(5)),
            arg_span: ByteSpan::new(ByteIndex(6), ByteIndex(10)),
            found: Box::new(concrete::Term::Universe(ByteSpan::default(), Some(1))),
        }),
    )
}

#[test]
fn lam() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> Type";
    let given_expr = r"\a : Type => a";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn pi() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"(a : Type) -> a";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_ann() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"(\a (x : a) => x) : (A : Type) -> A -> A";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

// Passing `Type` to the polymorphic identity function should yield the type
// identity function
#[test]
fn id_app_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type^1) (x : a) => x) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

// Passing `Type` to the `Type` identity function should yield `Type`
#[test]
fn id_app_ty_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"(\(a : Type^2) (x : a) => x) (Type^1) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_app_ty_arr_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"(\(a : Type^2) (x : a) => x) (Type^1) (Type -> Type)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_app_arr_pi_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type^1) (x : a) => x) (Type -> Type) (\x => x)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn apply() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> (a -> b) -> a -> b";
    let given_expr = r"\(a b : Type) (f : a -> b) (x : a) => f x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn const_() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> a -> b -> a";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn const_flipped() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> a -> b -> b";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => y";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn flip() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b c : Type) -> (a -> b -> c) -> (b -> a -> c)";
    let given_expr = r"\(a b c : Type) (f : a -> b -> c) (y : b) (x : a) => f x y";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn compose() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b c : Type) -> (b -> c) -> (a -> b) -> (a -> c)";
    let given_expr = r"\(a b c : Type) (f : b -> c) (g : a -> b) (x : a) => f (g x)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn let_expr_1() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"
        let x = "helloo";
        in
            x
    "#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn let_expr_2() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"
        let x = "helloo";
            y = x;
        in
            x
    "#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn let_shift_universes() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            test1 = id String "hello";
            test2 = id S32 1;
            test3 = id^1 Type String;
            test4 = id^2 Type String;
            test5 = id^2 Type^1 String;
            test6 = id^2 Type^1 Type;

            id1 : (a : Type^1) -> a -> a = id^1;
            id2 : (a : Type^2) -> a -> a = id^2;
            id11 : (a : Type^2) -> a -> a = id1^1;
            id22 : (a : Type^4) -> a -> a = id2^2;
        in
            record {}
    "#;

    parse_infer_term(&mut codemap, &tc_env, given_expr);
}

#[test]
fn let_shift_universes_id_self_application() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    // Here is a curious example from the Idris docs:
    // http://docs.idris-lang.org/en/v1.3.0/tutorial/miscellany.html#cumulativity
    //
    // ```idris
    // myid : (a : Type) -> a -> a
    // myid _ x = x
    //
    // idid : (a : Type) -> a -> a
    // idid = myid _ myid
    // ```
    //
    // This would cause a cycle in the universe hierarchy in Idris, but is
    // perfectly ok when implemented using explicit universe shifting.

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            id-id : (a : Type) -> a -> a;
            id-id = id^1 ((a : Type) -> a -> a) id;
        in
            record {}
    "#;

    parse_infer_term(&mut codemap, &tc_env, given_expr);
}

#[test]
fn let_shift_universes_literals() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            test2 = "hello" : id^1 Type String;
        in
            record {}
    "#;

    parse_infer_term(&mut codemap, &tc_env, given_expr);
}

#[test]
fn let_shift_universes_literals_bad() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            test2 = "hello" : id^2 Type^1 String^1;
        in
            record {}
    "#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn let_shift_universes_too_little() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            test1 = id^1 Type^1 Type;
        in
            record {}
    "#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::Mismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn let_shift_universes_too_much() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let given_expr = r#"
        let
            id : (a : Type) -> a -> a;
            id a x = x;

            test1 = id^2 Type String;
        in
            record {}
    "#;

    parse_infer_term(&mut codemap, &tc_env, given_expr);
}

#[test]
fn case_expr() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        "hello" => "byee";
        greeting => (extern "string-append" : String -> String -> String) greeting "!!";
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn case_expr_bool() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case true of {
        true => "hello";
        false => "hi";
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn case_expr_bool_bad() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"case "hello" of {
        true => "hello";
        false => "hi";
    }"#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::Mismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}

#[test]
fn case_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        test => test;
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn case_expr_empty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"case "helloo" of {}"#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::AmbiguousEmptyCase { .. }) => {},
        other => panic!("unexpected result: {:#?}", other),
    }
}

mod church_encodings {
    use super::*;

    #[test]
    fn and() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"Type -> Type -> Type^1";
        let given_expr = r"\(p q : Type) => (c : Type) -> (p -> q -> c) -> c";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_intro() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) -> p -> q ->
                ((c : Type) -> (p -> q -> c) -> c)
        ";
        let given_expr = r"
            \(p q : Type) (x : p) (y : q) =>
                \c : Type => \f : (p -> q -> c) => f x y
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_proj_left() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) ->
                ((c : Type) -> (p -> q -> c) -> c) -> p
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq p (\x y => x)
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_proj_right() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) -> ((c : Type) -> (p -> q -> c) -> c) -> q
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq q (\x y => y)
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }
}

#[test]
fn empty_record_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type";
    let given_expr = r"Record {}";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn empty_record() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Record {}";
    let given_expr = r"record {}";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn dependent_record_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^2";
    let given_expr = r"Record { t : Type^1; x : t }";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Record { t : Type; x : String }";
    let given_expr = r#"record { t = String; x = "Hello" }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn proj() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"(record { t = String; x = "hello" } : Record { t : Type; x : String }).x"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn proj_missing() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"(record { x = "hello" } : Record { x : String }).bloop"#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::NoFieldInType { .. }) => {},
        x => panic!("expected a field lookup error, found {:?}", x),
    }
}

#[test]
fn proj_weird1() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"Record {
        data : Record {
            t : Type;
            x : t;
        };

        f : data.t -> Type;
        test : f data.x;
    }";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn proj_weird2() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type^1";
    let given_expr = r"Record {
        Array : U16 -> Type -> Type;
        t : Record { n : U16; x : Array n S8; y : Array n S8 };
        inner-prod : (len : U16) -> Array len S8 -> Array len S8 -> S32;

        test1 : S32 -> Type;
        test2 : test1 (inner-prod t.n t.x t.y);
    }";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn proj_shift() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type^1) -> a -> a";
    let given_expr = r"record {
        id = \(a : Type) (x : a) => x;
    }.id^1";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn array_ambiguous() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"[1; 2 : S32]"#;

    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::AmbiguousArrayLiteral { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}
