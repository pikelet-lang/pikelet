use codespan::{ByteIndex, CodeMap, FileName};

use syntax::concrete;
use syntax::core::SourceMeta;
use syntax::parse;
use syntax::translation::ToCore;

use super::*;

fn parse(src: &str) -> Rc<RawTerm> {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);
    assert!(errors.is_empty());

    Rc::new(concrete_term.to_core())
}

fn parse_infer(src: &str) -> Rc<Term> {
    infer(&Context::new(), &parse(src)).unwrap().0
}

mod check_module;
mod infer;
mod normalize;
