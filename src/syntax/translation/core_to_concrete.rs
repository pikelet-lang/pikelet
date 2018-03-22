use codespan::ByteSpan;
use nameless::{self, Embed, Var};
use std::collections::HashSet;

use syntax::concrete;
use syntax::core;

/// An environment used to reconstruct concrete terms
pub struct Env {
    // precedence: ?,
    #[allow(dead_code)]
    used_names: HashSet<String>,
    // mappings: HashMap<Name, String>,
}

const USED_NAMES: &[&str] = &[
    // Keywords
    "as",
    "_",
    "module",
    "import",
    "Type",

    // Primitives
    "String",
    "Char",
    "U8",
    "U16",
    "U32",
    "U64",
    "I8",
    "I16",
    "I32",
    "I64",
    "F32",
    "F64",
];

impl Default for Env {
    fn default() -> Env {
        Env {
            used_names: USED_NAMES.iter().map(|&n| String::from(n)).collect(),
        }
    }
}

/// Translate something to the corresponding concrete representation
pub trait ToConcrete<T> {
    fn to_concrete(&self, env: &Env) -> T;
}

impl ToConcrete<concrete::Module> for core::RawModule {
    fn to_concrete(&self, env: &Env) -> concrete::Module {
        use std::iter;

        let declarations = self.definitions
            .iter()
            .flat_map(|definition| {
                let name = (ByteSpan::default(), definition.name.clone());

                // build up the type claim
                let new_ann = concrete::Declaration::Claim {
                    name: name.clone(),
                    ann: definition.ann.to_concrete(env),
                };

                // build up the concrete definition
                let new_definition = {
                    // pull lambda arguments from the body into the definition
                    let (params, body) = match definition.term.to_concrete(env) {
                        concrete::Term::Lam(_, params, body) => (params, *body),
                        body => (vec![], body),
                    };

                    concrete::Declaration::Definition { name, params, body }
                };

                iter::once(new_ann).chain(iter::once(new_definition))
            })
            .collect();

        concrete::Module::Valid {
            name: (ByteSpan::default(), self.name.clone()),
            declarations,
        }
    }
}

impl ToConcrete<Option<u32>> for core::Level {
    fn to_concrete(&self, _env: &Env) -> Option<u32> {
        match *self {
            core::Level(0) => None,
            core::Level(level) => Some(level),
        }
    }
}

impl ToConcrete<concrete::Term> for core::RcRawTerm {
    fn to_concrete(&self, env: &Env) -> concrete::Term {
        // FIXME: add concrete::Term::Paren where needed
        match *self.inner {
            core::RawTerm::Ann(_, ref term, ref ty) => concrete::Term::Ann(
                Box::new(term.to_concrete(env)),
                Box::new(ty.to_concrete(env)),
            ),
            core::RawTerm::Universe(meta, level) => {
                concrete::Term::Universe(meta.span, level.to_concrete(env))
            },
            core::RawTerm::Hole(meta) => concrete::Term::Hole(meta.span),
            core::RawTerm::Constant(meta, ref c) => {
                let span = meta.span;
                match *c {
                    core::RawConstant::String(ref value) => {
                        concrete::Term::String(meta.span, value.clone())
                    },
                    core::RawConstant::Char(value) => concrete::Term::Char(span, value),
                    core::RawConstant::Int(value) => concrete::Term::Int(span, value),
                    core::RawConstant::Float(value) => concrete::Term::Float(span, value),
                    core::RawConstant::StringType => {
                        concrete::Term::Var(span, String::from("String"))
                    },
                    core::RawConstant::CharType => concrete::Term::Var(span, String::from("Char")),
                    core::RawConstant::U8Type => concrete::Term::Var(span, String::from("U8")),
                    core::RawConstant::U16Type => concrete::Term::Var(span, String::from("U16")),
                    core::RawConstant::U32Type => concrete::Term::Var(span, String::from("U32")),
                    core::RawConstant::U64Type => concrete::Term::Var(span, String::from("U64")),
                    core::RawConstant::I8Type => concrete::Term::Var(span, String::from("I8")),
                    core::RawConstant::I16Type => concrete::Term::Var(span, String::from("I16")),
                    core::RawConstant::I32Type => concrete::Term::Var(span, String::from("I32")),
                    core::RawConstant::I64Type => concrete::Term::Var(span, String::from("I64")),
                    core::RawConstant::F32Type => concrete::Term::Var(span, String::from("F32")),
                    core::RawConstant::F64Type => concrete::Term::Var(span, String::from("F64")),
                }
            },
            core::RawTerm::Var(meta, Var::Free(core::Name::User(ref name))) => {
                concrete::Term::Var(meta.span, name.to_string()) // FIXME
            },
            core::RawTerm::Var(_, Var::Free(core::Name::Gen(ref _name, ref _gen))) => {
                // TODO: use name if it is present, and not used in the current scope
                // otherwise create a pretty name
                unimplemented!()
            },
            core::RawTerm::Var(_, Var::Bound(_, _)) => {
                // TODO: Better message
                panic!("Tried to convert a term that was not locally closed");
            },
            core::RawTerm::Pi(_, ref scope) => {
                let ((name, Embed(param_ann)), body) = nameless::unbind(scope.clone());
                if body.free_vars().contains(&name) {
                    // use name if it is present, and not used in the current scope
                    // otherwise create a pretty name
                    // add the used name to the environment
                    // convert the body using the new environment

                    // // match body.to_concrete(env) {
                    //     // check if the body can be collapsed to form a 'sugary' pi
                    //     concrete::Term::Pi(_, params, body) => unimplemented!(),
                    //     body => concrete::Term::Pi(ByteSpan::default(), vec![param], body),
                    // }

                    unimplemented!()
                } else {
                    // The body is not dependent on the parameter - so let's use an arrow instead!
                    concrete::Term::Arrow(
                        Box::new(param_ann.to_concrete(env)),
                        Box::new(body.to_concrete(env)),
                    )
                }
            },
            core::RawTerm::Lam(_, ref scope) => {
                let (_param, _body) = nameless::unbind(scope.clone());
                // use name if it is present, and not used in the current scope
                // otherwise create a pretty name
                // add the used name to the environment

                // // convert the body using the new environment
                // match body.to_concrete(env) {
                //     // check if the body can be collapsed to form a 'sugary' lambda
                //     concrete::Term::Lam(_, params, body) => unimplemented!(),
                //     body => concrete::Term::Lam(ByteSpan::default(), vec![param], body),
                // }

                unimplemented!()
            },
            core::RawTerm::App(_, ref fn_term, ref arg) => concrete::Term::Ann(
                Box::new(fn_term.to_concrete(env)),
                Box::new(arg.to_concrete(env)),
            ),
        }
    }
}
