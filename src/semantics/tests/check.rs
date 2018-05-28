use super::*;

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : String }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record_propagate_types() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = I32; x = 1 }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_0_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_3_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; "byee"]"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_len_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"]"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    match check(&context, &parse(&mut codemap, given_expr), &expected_ty) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; 4]"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    match check(&context, &parse(&mut codemap, given_expr), &expected_ty) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
