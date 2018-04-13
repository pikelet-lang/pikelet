use super::*;

#[test]
fn record() {
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : String }";
    let given_expr = r#"record { t = String, x = "hello" }"#;

    check(
        &context,
        &parse(given_expr),
        &normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    ).unwrap();
}

#[test]
fn dependent_record() {
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : t }";
    let given_expr = r#"record { t = String, x = "hello" }"#;

    check(
        &context,
        &parse(given_expr),
        &normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    ).unwrap();
}
