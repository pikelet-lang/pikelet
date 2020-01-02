//! Delaborate the core language into the surface language.

use crate::core::{Constant, Term, UniverseLevel, UniverseOffset};
use crate::surface;

pub fn delaborate_term(term: &Term) -> surface::Term<String> {
    match term {
        Term::Universe(UniverseLevel(0)) => surface::Term::Name(0..0, "Type".to_owned()),
        Term::Universe(UniverseLevel(level)) => {
            let universe0 = Box::new(surface::Term::Name(0..0, "Type".to_owned()));
            surface::Term::Lift(0..0, universe0, *level)
        }
        Term::Global(name) => surface::Term::Name(0..0, name.to_owned()),
        Term::Constant(constant) => delaborate_constant(constant),
        Term::Sequence(entry_terms) => {
            let core_entry_terms = entry_terms
                .iter()
                .map(|entry_term| delaborate_term(entry_term))
                .collect();

            surface::Term::Sequence(0..0, core_entry_terms)
        }
        Term::Ann(term, r#type) => surface::Term::Ann(
            Box::new(delaborate_term(term)),
            Box::new(delaborate_term(r#type)),
        ),
        Term::RecordType(type_entries) => {
            let core_type_entries = type_entries
                .iter()
                .map(|(name, r#type)| (name.clone(), delaborate_term(r#type)))
                .collect();

            surface::Term::RecordType(0..0, core_type_entries)
        }
        Term::RecordTerm(term_entries) => {
            let core_term_entries = term_entries
                .iter()
                .map(|(name, term)| (name.clone(), delaborate_term(term)))
                .collect();

            surface::Term::RecordTerm(0..0, core_term_entries)
        }
        Term::ArrayType(len, entry_type) => surface::Term::ArrayType(
            0..0,
            Box::new(delaborate_term(len)),
            Box::new(delaborate_term(entry_type)),
        ),
        Term::ListType(entry_type) => {
            surface::Term::ListType(0..0, Box::new(delaborate_term(entry_type)))
        }
        Term::Lift(term, UniverseOffset(offset)) => {
            surface::Term::Lift(0..0, Box::new(delaborate_term(term)), *offset)
        }
        Term::Error => surface::Term::Error(0..0),
    }
}

pub fn delaborate_constant(constant: &Constant) -> surface::Term<String> {
    use crate::surface::Literal;

    match constant {
        Constant::U8(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::U16(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::U32(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::U64(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::S8(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::S16(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::S32(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::S64(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::F32(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::F64(value) => surface::Term::Literal(0..0, Literal::Number(value.to_string())),
        Constant::Char(value) => surface::Term::Literal(0..0, Literal::Char(value.to_string())),
        Constant::String(value) => surface::Term::Literal(0..0, Literal::String(value.to_string())),
    }
}
