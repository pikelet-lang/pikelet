extern crate codespan;
extern crate codespan_reporting;
extern crate moniker;
extern crate pikelet_concrete;
extern crate pikelet_core;

use codespan::CodeMap;

use pikelet_concrete::desugar::{Desugar, DesugarEnv};
use pikelet_concrete::elaborate::{self, Context, TypeError};

mod support;

#[test]
fn record_intro() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : String }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn record_intro_field_mismatch_lt() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Record { x : String; y : String }";
    let given_expr = r#"record { x = "hello" }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match elaborate::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::RecordSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn record_intro_field_mismatch_gt() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Record { x : String }";
    let given_expr = r#"record { x = "hello"; y = "hello" }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match elaborate::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::RecordSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn record_intro_dependent_record_ty() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = String; x = "hello" }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn record_intro_dependent_record_ty_propagate_types() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Record { t : Type; x : t }";
    let given_expr = r#"record { t = S32; x = 1 }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" {
        "hi" => "haha";
        "hello" => "byee";
        greeting => (import "prim/string/append") greeting "!!";
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr_bad_literal() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" {
        "hi" => "haha";
        1 => "byee";
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match elaborate::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn case_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"S32";
    let given_expr = r#"case "helloo" {
        _ => 123;
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn case_expr_empty() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" {}"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_intro_0_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_intro_3_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; "byee"]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_intro_len_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match elaborate::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_intro_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello"; "hi"; 4]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match elaborate::check_term(&context, &raw_term, &expected_ty) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
