use super::*;

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : String }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record_propagate_types() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = I32; x = 1 }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        "hello" => "byee";
        greeting => (extern "string-append" : String -> String -> String) greeting "!!";
    }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr_bad_literal() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        1 => "byee";
    }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    match check_term(
        &prim_env,
        &context,
        &parse(&mut codemap, given_expr),
        &expected_ty,
    ) {
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn case_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"I32";
    let given_expr = r#"case "helloo" of {
        _ => 123;
    }"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr_empty() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {}"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn array_0_string() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn array_3_string() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; "byee"]"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    parse_check_term(&mut codemap, &prim_env, &context, given_expr, &expected_ty);
}

#[test]
fn array_len_mismatch() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"]"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    match check_term(
        &prim_env,
        &context,
        &parse(&mut codemap, given_expr),
        &expected_ty,
    ) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let prim_env = PrimEnv::default();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; 4]"#;

    let expected_ty = parse_normalize(&mut codemap, &prim_env, &context, expected_ty);
    match check_term(
        &prim_env,
        &context,
        &parse(&mut codemap, given_expr),
        &expected_ty,
    ) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
