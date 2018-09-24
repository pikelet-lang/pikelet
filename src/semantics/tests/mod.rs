use codespan::{ByteIndex, CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use syntax::concrete;
use syntax::parse;
use syntax::translation::{Desugar, DesugarEnv};

use super::*;

fn parse_module(codemap: &mut CodeMap, src: &str) -> concrete::Module {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_module, _import_paths, errors) = parse::module(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_module
}

fn parse_term(codemap: &mut CodeMap, src: &str) -> concrete::Term {
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

fn parse_infer_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str) -> (RcTerm, RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(tc_env.mappings()))
        .unwrap();
    match infer_term(tc_env, &raw_term) {
        Ok((term, ty)) => (term, ty),
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

fn parse_nf_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str) -> RcValue {
    let term = parse_infer_term(codemap, tc_env, src).0;
    match nf_term(tc_env, &term) {
        Ok(value) => value,
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("internal error!");
        },
    }
}

fn parse_check_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str, expected: &RcType) {
    let raw_term = parse_term(codemap, src)
        .desugar(&DesugarEnv::new(tc_env.mappings()))
        .unwrap();
    match check_term(tc_env, &raw_term, expected) {
        Ok(_) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

mod check_module;
mod check_term;
mod infer_term;
mod normalize;
