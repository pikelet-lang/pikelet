use codespan::{ByteIndex, ByteSpan};
use nameless::{self, Embed, Name, Var};
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

impl ToConcrete<concrete::Module> for core::Module {
    fn to_concrete(&self, env: &Env) -> concrete::Module {
        use std::iter;

        let declarations = self.definitions
            .iter()
            .flat_map(|definition| {
                // build up the type claim
                let new_ann = concrete::Declaration::Claim {
                    name: (ByteIndex::default(), definition.name.clone()),
                    ann: core::Term::from(&*definition.ann).to_concrete(env),
                };

                // build up the concrete definition
                let new_definition = {
                    // pull lambda arguments from the body into the definition
                    let (params, body) = match definition.term.to_concrete(env) {
                        concrete::Term::Lam(_, params, body) => (params, *body),
                        body => (vec![], body),
                    };

                    concrete::Declaration::Definition {
                        span: ByteSpan::default(),
                        name: definition.name.clone(),
                        params,
                        body,
                        wheres: vec![],
                    }
                };

                iter::once(new_ann).chain(iter::once(new_definition))
            })
            .collect();

        concrete::Module::Valid {
            name: (ByteIndex::default(), self.name.clone()),
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

impl ToConcrete<concrete::Term> for core::Term {
    fn to_concrete(&self, env: &Env) -> concrete::Term {
        // FIXME: add concrete::Term::Paren where needed
        match *self {
            core::Term::Ann(_, ref term, ref ty) => concrete::Term::Ann(
                Box::new(term.to_concrete(env)),
                Box::new(ty.to_concrete(env)),
            ),
            core::Term::Universe(meta, level) => {
                concrete::Term::Universe(meta.span, level.to_concrete(env))
            },
            core::Term::Constant(meta, ref c) => {
                let span = meta.span;
                match *c {
                    core::Constant::String(ref value) => {
                        concrete::Term::Literal(span, concrete::Literal::String(value.clone()))
                    },
                    core::Constant::Char(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Char(value))
                    },
                    core::Constant::U8(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Int(value as u64))
                    },
                    core::Constant::U16(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Int(value as u64))
                    },
                    core::Constant::U32(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Int(value as u64))
                    },
                    core::Constant::U64(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Int(value))
                    },
                    core::Constant::I8(_) => unimplemented!(),
                    core::Constant::I16(_) => unimplemented!(),
                    core::Constant::I32(_) => unimplemented!(),
                    core::Constant::I64(_) => unimplemented!(),
                    core::Constant::F32(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Float(value as f64))
                    },
                    core::Constant::F64(value) => {
                        concrete::Term::Literal(span, concrete::Literal::Float(value))
                    },
                    core::Constant::StringType => {
                        concrete::Term::Var(span.start(), String::from("String"))
                    },
                    core::Constant::CharType => {
                        concrete::Term::Var(span.start(), String::from("Char"))
                    },
                    core::Constant::U8Type => concrete::Term::Var(span.start(), String::from("U8")),
                    core::Constant::U16Type => {
                        concrete::Term::Var(span.start(), String::from("U16"))
                    },
                    core::Constant::U32Type => {
                        concrete::Term::Var(span.start(), String::from("U32"))
                    },
                    core::Constant::U64Type => {
                        concrete::Term::Var(span.start(), String::from("U64"))
                    },
                    core::Constant::I8Type => concrete::Term::Var(span.start(), String::from("I8")),
                    core::Constant::I16Type => {
                        concrete::Term::Var(span.start(), String::from("I16"))
                    },
                    core::Constant::I32Type => {
                        concrete::Term::Var(span.start(), String::from("I32"))
                    },
                    core::Constant::I64Type => {
                        concrete::Term::Var(span.start(), String::from("I64"))
                    },
                    core::Constant::F32Type => {
                        concrete::Term::Var(span.start(), String::from("F32"))
                    },
                    core::Constant::F64Type => {
                        concrete::Term::Var(span.start(), String::from("F64"))
                    },
                }
            },
            core::Term::Var(meta, Var::Free(Name::User(ref name))) => {
                concrete::Term::Var(meta.span.start(), name.to_string()) // FIXME
            },
            core::Term::Var(_, Var::Free(Name::Gen(ref _name, ref _gen))) => {
                // TODO: use name if it is present, and not used in the current scope
                // otherwise create a pretty name
                unimplemented!()
            },
            core::Term::Var(_, Var::Bound(_, _)) => {
                // TODO: Better message
                panic!("Tried to convert a term that was not locally closed");
            },
            core::Term::Pi(_, ref scope) => {
                let ((_name, Embed(_param_ann)), _body) = nameless::unbind(scope.clone());
                unimplemented!()
                // if body.free_vars().contains(&name) {
                //     // use name if it is present, and not used in the current scope
                //     // otherwise create a pretty name
                //     // add the used name to the environment
                //     // convert the body using the new environment

                //     // // match body.to_concrete(env) {
                //     //     // check if the body can be collapsed to form a 'sugary' pi
                //     //     concrete::Term::Pi(_, params, body) => unimplemented!(),
                //     //     body => concrete::Term::Pi(ByteSpan::default(), vec![param], body),
                //     // }

                //     unimplemented!()
                // } else {
                //     // The body is not dependent on the parameter - so let's use an arrow instead!
                //     concrete::Term::Arrow(
                //         Box::new(param_ann.to_concrete(env)),
                //         Box::new(body.to_concrete(env)),
                //     )
                // }
            },
            core::Term::Lam(_, ref scope) => {
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
            core::Term::App(_, ref fn_term, ref arg) => concrete::Term::App(
                Box::new(fn_term.to_concrete(env)),
                vec![arg.to_concrete(env)], // TODO
            ),
        }
    }
}
