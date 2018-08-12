use codespan::{ByteIndex, CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use syntax::concrete;
use syntax::parse;
use syntax::translation::{Desugar, DesugarEnv};

use super::*;

fn parse(codemap: &mut CodeMap, src: &str) -> raw::RcTerm {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_term.desugar(&DesugarEnv::new())
}

fn parse_module(codemap: &mut CodeMap, src: &str) -> raw::Module {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_module, errors) = parse::module(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_module.desugar(&DesugarEnv::new())
}

fn parse_infer_term(codemap: &mut CodeMap, tc_env: &TcEnv, src: &str) -> (RcTerm, RcType) {
    match infer_term(tc_env, &parse(codemap, src)) {
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
    match check_term(tc_env, &parse(codemap, src), expected) {
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
