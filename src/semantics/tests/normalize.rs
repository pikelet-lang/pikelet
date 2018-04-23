use nameless::Ignore;

use super::*;

#[test]
fn var() {
    let context = Context::new();

    let x = Name::user("x");
    let var = Rc::new(Term::Var(Ignore::default(), Var::Free(x.clone())));

    assert_eq!(
        normalize(&context, &var).unwrap(),
        Rc::new(Value::from(Neutral::Var(Var::Free(x)))),
    );
}

#[test]
fn ty() {
    let context = Context::new();

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, r"Type")).unwrap(),
        Rc::new(Value::Universe(Level(0)))
    );
}

#[test]
fn lam() {
    let context = Context::new();

    let x = Name::user("x");

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, r"\x : Type => x")).unwrap(),
        Rc::new(Value::Lam(nameless::bind(
            (x.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
            Rc::new(Value::from(Neutral::Var(Var::Free(x)))),
        ))),
    );
}

#[test]
fn pi() {
    let context = Context::new();

    let x = Name::user("x");

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, r"(x : Type) -> x")).unwrap(),
        Rc::new(Value::Pi(nameless::bind(
            (x.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
            Rc::new(Value::from(Neutral::Var(Var::Free(x)))),
        ))),
    );
}

#[test]
fn lam_app() {
    let context = Context::new();

    let x = Name::user("x");
    let y = Name::user("y");
    let ty_arr = Rc::new(Value::Pi(nameless::bind(
        (Name::user("_"), Embed(Rc::new(Value::Universe(Level(0))))),
        Rc::new(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        normalize(
            &context,
            &parse_infer(&context, r"\(x : Type -> Type) (y : Type) => x y")
        ).unwrap(),
        Rc::new(Value::Lam(nameless::bind(
            (x.clone(), Embed(ty_arr)),
            Rc::new(Value::Lam(nameless::bind(
                (y.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
                Rc::new(Value::from(Neutral::App(
                    Rc::new(Neutral::Var(Var::Free(x))),
                    Rc::new(Value::from(Neutral::Var(Var::Free(y)))),
                ))),
            ))),
        ))),
    );
}

#[test]
fn pi_app() {
    let context = Context::new();

    let x = Name::user("x");
    let y = Name::user("y");
    let ty_arr = Rc::new(Value::Pi(nameless::bind(
        (Name::user("_"), Embed(Rc::new(Value::Universe(Level(0))))),
        Rc::new(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        normalize(
            &context,
            &parse_infer(&context, r"(x : Type -> Type) -> (y : Type) -> x y")
        ).unwrap(),
        Rc::new(Value::Pi(nameless::bind(
            (x.clone(), Embed(ty_arr)),
            Rc::new(Value::Pi(nameless::bind(
                (y.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
                Rc::new(Value::from(Neutral::App(
                    Rc::new(Neutral::Var(Var::Free(x))),
                    Rc::new(Value::from(Neutral::Var(Var::Free(y)))),
                ))),
            ))),
        ))),
    );
}

// Passing `Type` to the polymorphic identity function should yeild the type
// identity function
#[test]
fn id_app_ty() {
    let context = Context::new();

    let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
    let expected_expr = r"\x : Type => x";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

// Passing `Type` to the `Type` identity function should yeild `Type`
#[test]
fn id_app_ty_ty() {
    let context = Context::new();

    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
    let expected_expr = r"Type";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

// Passing `Type -> Type` to the `Type` identity function should yeild
// `Type -> Type`
#[test]
fn id_app_ty_arr_ty() {
    let context = Context::new();

    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
    let expected_expr = r"Type -> Type";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

// Passing the id function to itself should yield the id function
#[test]
fn id_app_id() {
    let context = Context::new();

    let given_expr = r"
        (\(a : Type 1) (x : a) => x)
            ((a : Type) -> a -> a)
            (\(a : Type) (x : a) => x)
    ";
    let expected_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

// Passing the id function to the 'const' combinator should yeild a
// function that always returns the id function
#[test]
fn const_app_id_ty() {
    let context = Context::new();

    let given_expr = r"
        (\(a : Type 1) (b : Type 2) (x : a) (y : b) => x)
            ((a : Type) -> a -> a)
            (Type 1)
            (\(a : Type) (x : a) => x)
            Type
    ";
    let expected_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

#[test]
fn horrifying_app_1() {
    let context = Context::default();

    let given_expr = r"(\(t : Type) (f : (a : Type) -> Type) => f t) String (\(a : Type) => a)";
    let expected_expr = r"String";

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}

#[test]
fn horrifying_app_2() {
    let context = Context::default();

    let given_expr = r#"(\(t: String) (f: String -> String) => f t) "hello""#;
    let expected_expr = r#"\(f : String -> String) => f "hello""#;

    assert_term_eq!(
        normalize(&context, &parse_infer(&context, given_expr)).unwrap(),
        normalize(&context, &parse_infer(&context, expected_expr)).unwrap(),
    );
}
