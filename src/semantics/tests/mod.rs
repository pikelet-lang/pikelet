use codespan::{ByteIndex, CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use syntax::concrete;
use syntax::core::SourceMeta;
use syntax::parse;
use syntax::translation::ToCore;

use super::*;

fn parse(src: &str) -> Rc<RawTerm> {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let writer = StandardStream::stderr(ColorChoice::Always);
    let (concrete_term, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    Rc::new(concrete_term.to_core())
}

fn parse_infer(context: &Context, src: &str) -> Rc<Term> {
    infer(context, &parse(src)).unwrap().0
}

mod check;
mod check_module;
mod infer;
mod normalize;
