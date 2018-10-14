#![allow(dead_code)]

use codespan::{CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use pikelet::semantics::{self, TcEnv};
use pikelet::syntax::concrete;
use pikelet::syntax::core::{RcTerm, RcType, RcValue};
use pikelet::syntax::parse;
use pikelet::syntax::translation::{Desugar, DesugarEnv};

pub fn parse_term(codemap: &mut CodeMap, src: &str) -> concrete::Term {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, _import_paths, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_term
}

pub fn parse_infer_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str) -> (RcTerm, RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(tc_env.mappings()))
        .unwrap();
    match semantics::infer_term(tc_env, &raw_term) {
        Ok((term, ty)) => (term, ty),
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

pub fn parse_nf_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str) -> RcValue {
    let term = parse_infer_term(codemap, tc_env, src).0;
    match semantics::nf_term(tc_env, &term) {
        Ok(value) => value,
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("internal error!");
        },
    }
}

pub fn parse_check_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str, expected: &RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(tc_env.mappings()))
        .unwrap();
    match semantics::check_term(tc_env, &raw_term, expected) {
        Ok(_) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}
