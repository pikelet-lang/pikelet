use codespan::{CodeMap, FileName};

use syntax::parse;
use syntax::translation::ToCore;

use super::*;

fn parse(src: &str) -> RcRawTerm {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let (concrete_term, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    concrete_term.to_core()
}

mod term_eq {
    use super::*;

    #[test]
    fn var() {
        assert_term_eq!(parse(r"x"), parse(r"x"));
    }

    #[test]
    #[should_panic]
    fn var_diff() {
        assert_term_eq!(parse(r"x"), parse(r"y"));
    }

    #[test]
    fn ty() {
        assert_term_eq!(parse(r"Type"), parse(r"Type"));
    }

    #[test]
    fn lam() {
        assert_term_eq!(parse(r"\x : Type => x"), parse(r"\a : Type => a"));
    }

    #[test]
    fn pi() {
        assert_term_eq!(parse(r"(x : Type) -> x"), parse(r"(a : Type) -> a"));
    }

    #[test]
    fn lam_app() {
        assert_term_eq!(
            parse(r"\x : Type -> Type => x Type"),
            parse(r"\a : Type -> Type => a Type")
        );
    }

    #[test]
    fn pi_app() {
        assert_term_eq!(
            parse(r"(x : Type -> Type) -> x Type"),
            parse(r"(a : Type -> Type) -> a Type")
        );
    }

    #[test]
    fn lam_lam_app() {
        assert_term_eq!(
            parse(r"\x : Type -> Type => \y : Type => x y"),
            parse(r"\a : Type -> Type => \b : Type => a b"),
        );
    }

    #[test]
    fn pi_pi_app() {
        assert_term_eq!(
            parse(r"(x : Type -> Type) -> (y : Type) -> x y"),
            parse(r"(a : Type -> Type) -> (b : Type) -> a b"),
        );
    }
}
