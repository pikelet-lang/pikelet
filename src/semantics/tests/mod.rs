use codespan::{ByteIndex, CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use syntax::concrete;
use syntax::parse;
use syntax::translation::ToCore;

use super::*;

fn parse(codemap: &mut CodeMap, src: &str) -> Rc<RawTerm> {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    Rc::new(concrete_term.to_core())
}

fn parse_infer(codemap: &mut CodeMap, context: &Context, src: &str) -> (Rc<Term>, Rc<Type>) {
    match infer(context, &parse(codemap, src)) {
        Ok((term, ty)) => (term, ty),
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

fn parse_normalize(codemap: &mut CodeMap, context: &Context, src: &str) -> Rc<Value> {
    match normalize(context, &parse_infer(codemap, context, src).0) {
        Ok(value) => value,
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("internal error!");
        },
    }
}

fn parse_check(codemap: &mut CodeMap, context: &Context, src: &str, expected: &Rc<Type>) {
    match check(context, &parse(codemap, src), expected) {
        Ok(_) => {},
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

mod check;
mod check_module;
mod infer;
mod normalize;
