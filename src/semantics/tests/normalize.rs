use goldenfile::Mint;

use std::io::Write;

use super::*;

fn golden(filename: &str, literal: &str) {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let path = "src/semantics/tests/goldenfiles";

    let mut mint = Mint::new(path);
    let mut file = mint.new_goldenfile(filename).unwrap();

    let term = parse_normalize(&mut codemap, &context, literal);

    write!(file, "{:#?}", term).unwrap();
}

#[test]
fn var() {
    let context = Context::new();

    let x = FreeVar::user("x");
    let var = RcTerm::from(Term::Var(Var::Free(x.clone())));

    assert_eq!(
        normalize(&context, &var).unwrap(),
        RcValue::from(Value::from(Var::Free(x))),
    );
}

#[test]
fn ty() {
    golden("ty", r"Type");
}

#[test]
fn lam() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = FreeVar::user("x");

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, r"\x : Type => x"),
        RcValue::from(Value::Lam(Scope::new(
            (
                Binder(x.clone()),
                Embed(RcValue::from(Value::Universe(Level(0))))
            ),
            RcValue::from(Value::from(Var::Free(x))),
        ))),
    );
}

#[test]
fn pi() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = FreeVar::user("x");

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, r"(x : Type) -> x"),
        RcValue::from(Value::Pi(Scope::new(
            (
                Binder(x.clone()),
                Embed(RcValue::from(Value::Universe(Level(0))))
            ),
            RcValue::from(Value::from(Var::Free(x))),
        ))),
    );
}

#[test]
fn lam_app() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = FreeVar::user("x");
    let y = FreeVar::user("y");
    let ty_arr = RcValue::from(Value::Pi(Scope::new(
        (
            Binder::user("_"),
            Embed(RcValue::from(Value::Universe(Level(0)))),
        ),
        RcValue::from(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        parse_normalize(
            &mut codemap,
            &context,
            r"\(x : Type -> Type) (y : Type) => x y"
        ),
        RcValue::from(Value::Lam(Scope::new(
            (Binder(x.clone()), Embed(ty_arr)),
            RcValue::from(Value::Lam(Scope::new(
                (
                    Binder(y.clone()),
                    Embed(RcValue::from(Value::Universe(Level(0))))
                ),
                RcValue::from(Value::from(Neutral::App(
                    Head::Var(Var::Free(x)),
                    vec![RcValue::from(Value::from(Var::Free(y)))],
                ))),
            ))),
        ))),
    );
}

#[test]
fn pi_app() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = FreeVar::user("x");
    let y = FreeVar::user("y");
    let ty_arr = RcValue::from(Value::Pi(Scope::new(
        (
            Binder::user("_"),
            Embed(RcValue::from(Value::Universe(Level(0)))),
        ),
        RcValue::from(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        parse_normalize(
            &mut codemap,
            &context,
            r"(x : Type -> Type) -> (y : Type) -> x y"
        ),
        RcValue::from(Value::Pi(Scope::new(
            (Binder(x.clone()), Embed(ty_arr)),
            RcValue::from(Value::Pi(Scope::new(
                (
                    Binder(y.clone()),
                    Embed(RcValue::from(Value::Universe(Level(0))))
                ),
                RcValue::from(Value::from(Neutral::App(
                    Head::Var(Var::Free(x)),
                    vec![RcValue::from(Value::from(Var::Free(y)))],
                ))),
            ))),
        ))),
    );
}

// Passing `Type` to the polymorphic identity function should yield the type
// identity function
#[test]
fn id_app_ty() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
    let expected_expr = r"\x : Type => x";

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

// Passing `Type` to the `Type` identity function should yield `Type`
#[test]
fn id_app_ty_ty() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
    let expected_expr = r"Type";

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

// Passing `Type -> Type` to the `Type` identity function should yield
// `Type -> Type`
#[test]
fn id_app_ty_arr_ty() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
    let expected_expr = r"Type -> Type";

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

// Passing the id function to itself should yield the id function
#[test]
fn id_app_id() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let given_expr = r"
        (\(a : Type 1) (x : a) => x)
            ((a : Type) -> a -> a)
            (\(a : Type) (x : a) => x)
    ";
    let expected_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

// Passing the id function to the 'const' combinator should yield a
// function that always returns the id function
#[test]
fn const_app_id_ty() {
    let mut codemap = CodeMap::new();
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
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

#[test]
fn horrifying_app_1() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let given_expr = r"(\(t : Type) (f : (a : Type) -> Type) => f t) String (\(a : Type) => a)";
    let expected_expr = r"String";

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}

#[test]
fn horrifying_app_2() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let given_expr = r#"(\(t: String) (f: String -> String) => f t) "hello""#;
    let expected_expr = r#"\(f : String -> String) => f "hello""#;

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, given_expr),
        parse_normalize(&mut codemap, &context, expected_expr),
    );
}
