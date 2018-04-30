use super::*;

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : String }";
    let given_expr = r#"record { t = String, x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : t }";
    let given_expr = r#"record { t = String, x = "hello" }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn dependent_record_propagate_types() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : t }";
    let given_expr = r#"record { t = I32, x = 1 }"#;

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}
