use codespan::{ByteIndex, CodeMap, FileName};

use syntax::core::SourceMeta;
use syntax::translation::ToCore;
use syntax::parse;

use super::*;

fn parse(src: &str) -> RcRawTerm {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.to_core()
}

fn parse_infer(src: &str) -> RcTerm {
    infer(&Context::new(), &parse(src)).unwrap().0
}

mod normalize {
    use super::*;

    #[test]
    fn var() {
        let context = Context::new();

        let x = Name::user("x");
        let var = Term::Var(SourceMeta::default(), Var::Free(x.clone())).into();

        assert_eq!(
            normalize(&context, &var),
            Err(InternalError::UndefinedName {
                var_span: ByteSpan::default(),
                name: x,
            }),
        );
    }

    #[test]
    fn ty() {
        let context = Context::new();

        assert_alpha_eq!(
            normalize(&context, &parse_infer(r"Type")).unwrap(),
            Value::Universe(Level::ZERO).into()
        );
    }

    #[test]
    fn lam() {
        let context = Context::new();

        let x = Name::user("x");

        assert_alpha_eq!(
            normalize(&context, &parse_infer(r"\x : Type => x")).unwrap(),
            Value::Lam(Scope::bind(
                Named::new(x.clone(), Value::Universe(Level::ZERO).into()),
                Neutral::Var(Var::Free(x)).into(),
            )).into(),
        );
    }

    #[test]
    fn pi() {
        let context = Context::new();

        let x = Name::user("x");

        assert_alpha_eq!(
            normalize(&context, &parse_infer(r"(x : Type) -> x")).unwrap(),
            Value::Pi(Scope::bind(
                Named::new(x.clone(), Value::Universe(Level::ZERO).into()),
                Neutral::Var(Var::Free(x)).into(),
            )).into(),
        );
    }

    #[test]
    fn lam_app() {
        let context = Context::new();

        let x = Name::user("x");
        let y = Name::user("y");
        let ty_arr: RcValue = Value::Pi(Scope::bind(
            Named::new(Name::user("_"), Value::Universe(Level::ZERO).into()),
            Value::Universe(Level::ZERO).into(),
        )).into();

        assert_alpha_eq!(
            normalize(
                &context,
                &parse_infer(r"\(x : Type -> Type) (y : Type) => x y")
            ).unwrap(),
            Value::Lam(Scope::bind(
                Named::new(x.clone(), ty_arr),
                Value::Lam(Scope::bind(
                    Named::new(y.clone(), Value::Universe(Level::ZERO).into()),
                    Neutral::App(
                        Neutral::Var(Var::Free(x)).into(),
                        Term::Var(SourceMeta::default(), Var::Free(y)).into(),
                    ).into(),
                )).into(),
            )).into(),
        );
    }

    #[test]
    fn pi_app() {
        let context = Context::new();

        let x = Name::user("x");
        let y = Name::user("y");
        let ty_arr: RcValue = Value::Pi(Scope::bind(
            Named::new(Name::user("_"), Value::Universe(Level::ZERO).into()),
            Value::Universe(Level::ZERO).into(),
        )).into();

        assert_alpha_eq!(
            normalize(
                &context,
                &parse_infer(r"(x : Type -> Type) -> (y : Type) -> x y")
            ).unwrap(),
            Value::Pi(Scope::bind(
                Named::new(x.clone(), ty_arr),
                Value::Pi(Scope::bind(
                    Named::new(y.clone(), Value::Universe(Level::ZERO).into()),
                    Neutral::App(
                        Neutral::Var(Var::Free(x)).into(),
                        Term::Var(SourceMeta::default(), Var::Free(y)).into(),
                    ).into(),
                )).into(),
            )).into(),
        );
    }

    // Passing `Type` to the polymorphic identity function should yeild the type
    // identity function
    #[test]
    fn id_app_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
        let expected_expr = r"\x : Type => x";

        assert_alpha_eq!(
            normalize(&context, &parse_infer(given_expr)).unwrap(),
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
        );
    }

    // Passing `Type` to the `Type` identity function should yeild `Type`
    #[test]
    fn id_app_ty_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
        let expected_expr = r"Type";

        assert_alpha_eq!(
            normalize(&context, &parse_infer(given_expr)).unwrap(),
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
        );
    }

    // Passing `Type -> Type` to the `Type` identity function should yeild
    // `Type -> Type`
    #[test]
    fn id_app_ty_arr_ty() {
        let context = Context::new();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
        let expected_expr = r"Type -> Type";

        assert_alpha_eq!(
            normalize(&context, &parse_infer(given_expr)).unwrap(),
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
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

        assert_alpha_eq!(
            normalize(&context, &parse_infer(given_expr)).unwrap(),
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
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

        assert_alpha_eq!(
            normalize(&context, &parse_infer(given_expr)).unwrap(),
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
        );
    }
}

mod infer {
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

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ty_levels() {
        let context = Context::new();

        let expected_ty = r"Type 1";
        let given_expr = r"Type 0 : Type 1 : Type 2 : Type 3"; //... Type ∞       ...+:｡(ﾉ･ω･)ﾉﾞ

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ann_ty_id() {
        let context = Context::new();

        let expected_ty = r"Type -> Type";
        let given_expr = r"(\a => a) : Type -> Type";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn ann_arrow_ty_id() {
        let context = Context::new();

        let expected_ty = r"(Type -> Type) -> (Type -> Type)";
        let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
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

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
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
                found: Value::Universe(Level::ZERO.succ()).into(),
            }),
        )
    }

    #[test]
    fn lam() {
        let context = Context::new();

        let expected_ty = r"(a : Type) -> Type";
        let given_expr = r"\a : Type => a";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn pi() {
        let context = Context::new();

        let expected_ty = r"Type 1";
        let given_expr = r"(a : Type) -> a";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id() {
        let context = Context::new();

        let expected_ty = r"(a : Type) -> a -> a";
        let given_expr = r"\(a : Type) (x : a) => x";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id_ann() {
        let context = Context::new();

        let expected_ty = r"(a : Type) -> a -> a";
        let given_expr = r"(\a (x : a) => x) : (A : Type) -> A -> A";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    // Passing `Type` to the polymorphic identity function should yeild the type
    // identity function
    #[test]
    fn id_app_ty() {
        let context = Context::new();

        let expected_expr = r"Type -> Type";
        let given_expr = r"(\(a : Type 1) (x : a) => x) Type";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
        );
    }

    // Passing `Type` to the `Type` identity function should yeild `Type`
    #[test]
    fn id_app_ty_ty() {
        let context = Context::new();

        let expected_expr = r"Type 1";
        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_expr)).unwrap(),
        );
    }

    #[test]
    fn id_app_ty_arr_ty() {
        let context = Context::new();

        let expected_ty = r"Type 1";
        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn id_app_arr_pi_ty() {
        let context = Context::new();

        let expected_ty = r"Type -> Type";
        let given_expr = r"(\(a : Type 1) (x : a) => x) (Type -> Type) (\x => x)";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn apply() {
        let context = Context::new();

        let expected_ty = r"(a b : Type) -> (a -> b) -> a -> b";
        let given_expr = r"\(a b : Type) (f : a -> b) (x : a) => f x";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn const_() {
        let context = Context::new();

        let expected_ty = r"(a b : Type) -> a -> b -> a";
        let given_expr = r"\(a b : Type) (x : a) (y : b) => x";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn const_flipped() {
        let context = Context::new();

        let expected_ty = r"(a b : Type) -> a -> b -> b";
        let given_expr = r"\(a b : Type) (x : a) (y : b) => y";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn flip() {
        let context = Context::new();

        let expected_ty = r"(a b c : Type) -> (a -> b -> c) -> (b -> a -> c)";
        let given_expr = r"\(a b c : Type) (f : a -> b -> c) (y : b) (x : a) => f x y";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    #[test]
    fn compose() {
        let context = Context::new();

        let expected_ty = r"(a b c : Type) -> (b -> c) -> (a -> b) -> (a -> c)";
        let given_expr = r"\(a b c : Type) (f : b -> c) (g : a -> b) (x : a) => f (g x)";

        assert_alpha_eq!(
            infer(&context, &parse(given_expr)).unwrap().1,
            normalize(&context, &parse_infer(expected_ty)).unwrap(),
        );
    }

    mod church_encodings {
        use super::*;

        #[test]
        fn and() {
            let context = Context::new();

            let expected_ty = r"Type -> Type -> Type 1";
            let given_expr = r"\(p q : Type) => (c : Type) -> (p -> q -> c) -> c";

            assert_alpha_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse_infer(expected_ty)).unwrap(),
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

            assert_alpha_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse_infer(expected_ty)).unwrap(),
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

            assert_alpha_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse_infer(expected_ty)).unwrap(),
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

            assert_alpha_eq!(
                infer(&context, &parse(given_expr)).unwrap().1,
                normalize(&context, &parse_infer(expected_ty)).unwrap(),
            );
        }
    }
}

mod check_module {
    use library;
    use codespan_reporting;

    use super::*;

    #[test]
    fn check_prelude() {
        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), library::PRELUDE.into());

        let (concrete_module, errors) = parse::module(&filemap);
        assert!(errors.is_empty());

        let module = concrete_module.to_core();
        if let Err(err) = check_module(&module) {
            codespan_reporting::emit(&codemap, &err.to_diagnostic());
            panic!("type error!")
        }
    }
}
