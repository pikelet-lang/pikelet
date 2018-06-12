use super::*;

#[test]
fn var() {
    let context = Context::new();

    let x = Name::user("x");
    let var = Rc::new(Term::Var(Var::Free(x.clone())));

    assert_eq!(
        normalize(&context, &var).unwrap(),
        Rc::new(Value::from(Var::Free(x))),
    );
}

#[test]
fn ty() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, r"Type"),
        Rc::new(Value::Universe(Level(0)))
    );
}

#[test]
fn lam() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = Name::user("x");

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, r"\x : Type => x"),
        Rc::new(Value::Lam(nameless::bind(
            (x.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
            Rc::new(Value::from(Var::Free(x))),
        ))),
    );
}

#[test]
fn pi() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = Name::user("x");

    assert_term_eq!(
        parse_normalize(&mut codemap, &context, r"(x : Type) -> x"),
        Rc::new(Value::Pi(nameless::bind(
            (x.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
            Rc::new(Value::from(Var::Free(x))),
        ))),
    );
}

#[test]
fn lam_app() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = Name::user("x");
    let y = Name::user("y");
    let ty_arr = Rc::new(Value::Pi(nameless::bind(
        (Name::user("_"), Embed(Rc::new(Value::Universe(Level(0))))),
        Rc::new(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        parse_normalize(
            &mut codemap,
            &context,
            r"\(x : Type -> Type) (y : Type) => x y"
        ),
        Rc::new(Value::Lam(nameless::bind(
            (x.clone(), Embed(ty_arr)),
            Rc::new(Value::Lam(nameless::bind(
                (y.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
                Rc::new(Value::from(Neutral::App(
                    Head::Var(Var::Free(x)),
                    vec![Rc::new(Value::from(Var::Free(y)))],
                ))),
            ))),
        ))),
    );
}

#[test]
fn pi_app() {
    let mut codemap = CodeMap::new();
    let context = Context::new();

    let x = Name::user("x");
    let y = Name::user("y");
    let ty_arr = Rc::new(Value::Pi(nameless::bind(
        (Name::user("_"), Embed(Rc::new(Value::Universe(Level(0))))),
        Rc::new(Value::Universe(Level(0))),
    )));

    assert_term_eq!(
        parse_normalize(
            &mut codemap,
            &context,
            r"(x : Type -> Type) -> (y : Type) -> x y"
        ),
        Rc::new(Value::Pi(nameless::bind(
            (x.clone(), Embed(ty_arr)),
            Rc::new(Value::Pi(nameless::bind(
                (y.clone(), Embed(Rc::new(Value::Universe(Level(0))))),
                Rc::new(Value::from(Neutral::App(
                    Head::Var(Var::Free(x)),
                    vec![Rc::new(Value::from(Var::Free(y)))],
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
