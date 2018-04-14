use super::*;

#[test]
fn free() {
    let context = Context::new();

    let given_expr = r"x";
    let x = Name::user("x");

    assert_eq!(
        infer(&context, &parse(given_expr)),
        Err(TypeError::UndefinedName {
            var_span: ByteSpan::new(ByteIndex(1), ByteIndex(2)),
            name: x,
        }),
    );
}

#[test]
fn ty() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"Type";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn ty_levels() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"Type 0 : Type 1 : Type 2 : Type 3"; //... Type ∞       ...+:｡(ﾉ･ω･)ﾉﾞ

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn ann_ty_id() {
    let context = Context::new();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\a => a) : Type -> Type";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn ann_arrow_ty_id() {
    let context = Context::new();

    let expected_ty = r"(Type -> Type) -> (Type -> Type)";
    let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn ann_id_as_ty() {
    let context = Context::new();

    let given_expr = r"(\a => a) : Type";

    match infer(&context, &parse(given_expr)) {
        Err(TypeError::UnexpectedFunction { .. }) => {},
        other => panic!("unexpected result: {:#?}", other),
    }
}

#[test]
fn app() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"(\a : Type 1 => a) Type";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn app_ty() {
    let context = Context::new();

    let given_expr = r"Type Type";

    assert_eq!(
        infer(&context, &parse(given_expr)),
        Err(TypeError::ArgAppliedToNonFunction {
            fn_span: ByteSpan::new(ByteIndex(1), ByteIndex(5)),
            arg_span: ByteSpan::new(ByteIndex(6), ByteIndex(10)),
            found: Box::new(concrete::Term::Universe(ByteSpan::default(), Some(1))),
        }),
    )
}

#[test]
fn lam() {
    let context = Context::new();

    let expected_ty = r"(a : Type) -> Type";
    let given_expr = r"\a : Type => a";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn pi() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"(a : Type) -> a";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn id() {
    let context = Context::new();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn id_ann() {
    let context = Context::new();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"(\a (x : a) => x) : (A : Type) -> A -> A";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

// Passing `Type` to the polymorphic identity function should yeild the type
// identity function
#[test]
fn id_app_ty() {
    let context = Context::new();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type 1) (x : a) => x) Type";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

// Passing `Type` to the `Type` identity function should yeild `Type`
#[test]
fn id_app_ty_ty() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn id_app_ty_arr_ty() {
    let context = Context::new();

    let expected_ty = r"Type 1";
    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn id_app_arr_pi_ty() {
    let context = Context::new();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type 1) (x : a) => x) (Type -> Type) (\x => x)";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn apply() {
    let context = Context::new();

    let expected_ty = r"(a b : Type) -> (a -> b) -> a -> b";
    let given_expr = r"\(a b : Type) (f : a -> b) (x : a) => f x";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn const_() {
    let context = Context::new();

    let expected_ty = r"(a b : Type) -> a -> b -> a";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => x";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn const_flipped() {
    let context = Context::new();

    let expected_ty = r"(a b : Type) -> a -> b -> b";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => y";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn flip() {
    let context = Context::new();

    let expected_ty = r"(a b c : Type) -> (a -> b -> c) -> (b -> a -> c)";
    let given_expr = r"\(a b c : Type) (f : a -> b -> c) (y : b) (x : a) => f x y";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn compose() {
    let context = Context::new();

    let expected_ty = r"(a b c : Type) -> (b -> c) -> (a -> b) -> (a -> c)";
    let given_expr = r"\(a b c : Type) (f : b -> c) (g : a -> b) (x : a) => f (g x)";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

mod church_encodings {
    use super::*;

    #[test]
    fn and() {
        let context = Context::new();

        let expected_ty = r"Type -> Type -> Type 1";
        let given_expr = r"\(p q : Type) => (c : Type) -> (p -> q -> c) -> c";

        assert_term_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
        );
    }

    #[test]
    fn and_intro() {
        let context = Context::new();

        let expected_ty = r"
            (p q : Type) -> p -> q ->
                ((c : Type) -> (p -> q -> c) -> c)
        ";
        let given_expr = r"
            \(p q : Type) (x : p) (y : q) =>
                \c : Type => \f : (p -> q -> c) => f x y
        ";

        assert_term_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
        );
    }

    #[test]
    fn and_proj_left() {
        let context = Context::new();

        let expected_ty = r"
            (p q : Type) ->
                ((c : Type) -> (p -> q -> c) -> c) -> p
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq p (\x y => x)
        ";

        assert_term_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
        );
    }

    #[test]
    fn and_proj_right() {
        let context = Context::new();

        let expected_ty = r"
            (p q : Type) -> ((c : Type) -> (p -> q -> c) -> c) -> q
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq q (\x y => y)
        ";

        assert_term_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
        );
    }
}

#[test]
fn empty_record_ty() {
    let context = Context::new();

    let expected_ty = r"Type";
    let given_expr = r"Record {}";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn empty_record() {
    let context = Context::new();

    let expected_ty = r"Record {}";
    let given_expr = r"record {}";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn record_ty() {
    let context = Context::default();

    let expected_ty = r"Type 2";
    let given_expr = r"Record { t : Type 1, x : String }";

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn record() {
    let context = Context::default();

    let expected_ty = r"Record { t : Type, x : String }";
    let given_expr = r#"record { t = String, x = "hello" }"#;

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn proj() {
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"record { t = String, x = "hello" }.x"#;

    assert_term_eq!(
        infer(&context, &parse(given_expr)).unwrap().1,
        normalize(&context, &parse_infer(&context, expected_ty)).unwrap(),
    );
}

#[test]
fn proj_missing() {
    let context = Context::new();

    let given_expr = r#"record { x = "hello" }.bloop"#;

    match infer(&context, &parse(given_expr)) {
        Err(TypeError::NoFieldInType { .. }) => {},
        x => panic!("expected a field lookup error, found {:?}", x),
    }
}
