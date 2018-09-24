use super::*;

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Record { t : Type; x : String }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn record_field_mismatch_lt() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Record { x : String; y : String }";
    let given_expr = r#"record { x = "hello" }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::RecordSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn record_field_mismatch_gt() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Record { x : String }";
    let given_expr = r#"record { x = "hello"; y = "hello" }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::RecordSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn dependent_record() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn dependent_record_propagate_types() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = S32; x = 1 }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
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

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn case_expr_bad_literal() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        1 => "byee";
    }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn case_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"S32";
    let given_expr = r#"case "helloo" of {
        _ => 123;
    }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn case_expr_empty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {}"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_0_string() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_3_string() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; "byee"]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_len_mismatch() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; 4]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
