use super::*;

#[test]
fn record() {
    let context = Context::default();

    let expected_ty = r"Record { x : F32, y : F32 }";
    let given_expr = r"record { x = 3.0, y = 3.0 }";

    check(
        &context,
        &parse(given_expr),
        &normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    ).unwrap();
}
