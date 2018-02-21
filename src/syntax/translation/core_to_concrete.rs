// TODO

use syntax::concrete;
use syntax::core;

/// Translate something to the corresponding concrete representation
pub trait ToConcrete<T> {
    fn to_concrete(&self) -> T;
}

impl ToConcrete<concrete::Module> for core::Module {
    fn to_concrete(&self) -> concrete::Module {
        unimplemented!()
    }
}

impl ToConcrete<concrete::Term> for core::RcTerm {
    fn to_concrete(&self) -> concrete::Term {
        unimplemented!()
    }
}
