use super::*;

#[test]
fn record() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { x : F32, y : F32 }";
    let given_expr = r"record { x = 3.0, y = 3.0 }";

    let expected_ty = parse_normalize(&mut codemap, &context, expected_ty);
    parse_check(&mut codemap, &context, given_expr, &expected_ty);
}
