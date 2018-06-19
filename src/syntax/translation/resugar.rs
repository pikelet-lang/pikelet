use codespan::{ByteIndex, ByteSpan};
use nameless::{self, Bind, BoundTerm, Embed, Name, Var};
use std::rc::Rc;

use syntax::concrete;
use syntax::core;
use syntax::Level;

/// Translate something to the corresponding concrete representation
pub trait Resugar<T> {
    fn resugar(&self) -> T;
}

impl Resugar<(concrete::Declaration, concrete::Declaration)> for core::Definition {
    fn resugar(&self) -> (concrete::Declaration, concrete::Declaration) {
        // pull lambda arguments from the body into the definition
        let (params, body) = match resugar_term(&self.term, Prec::ANN) {
            concrete::Term::Lam(_, params, body) => (params, *body),
            body => (vec![], body),
        };

        (
            concrete::Declaration::Claim {
                name: (ByteIndex::default(), self.name.clone()),
                ann: resugar_term(&core::Term::from(&*self.ann), Prec::ANN),
            },
            concrete::Declaration::Definition {
                span: ByteSpan::default(),
                name: self.name.clone(),
                ann: None,
                params,
                body,
                wheres: vec![],
            },
        )
    }
}

impl Resugar<concrete::Module> for core::Module {
    fn resugar(&self) -> concrete::Module {
        let mut declarations = Vec::with_capacity(self.definitions.len() * 2);

        for definition in &self.definitions {
            let (new_ann, new_definition) = definition.resugar();
            declarations.push(new_ann);
            declarations.push(new_definition);
        }

        concrete::Module::Valid { declarations }
    }
}

/// The precedence of a term
///
/// This is used to reconstruct the parentheses needed to reconstruct a valid
/// syntax tree in the concrete syntax
#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Prec(i8);

impl Prec {
    /// This term will never be wrapped in parentheses
    pub const NO_WRAP: Prec = Prec(-1);
    /// Precedence corresponding to `Term` in the parser
    pub const ANN: Prec = Prec(0);
    /// Precedence corresponding to `LamTerm` in the parser
    pub const LAM: Prec = Prec(1);
    /// Precedence corresponding to `PiTerm` in the parser
    pub const PI: Prec = Prec(2);
    /// Precedence corresponding to `AppTerm` in the parser
    pub const APP: Prec = Prec(3);
    /// Precedence corresponding to `AtomicTerm` in the parser
    pub const ATOMIC: Prec = Prec(4);
}

fn parens_if(should_wrap: bool, inner: concrete::Term) -> concrete::Term {
    if should_wrap {
        concrete::Term::Parens(ByteSpan::default(), Box::new(inner))
    } else {
        inner
    }
}

// TODO: Use this for name-avoidance
// const USED_NAMES: &[&str] = &[
//     // Keywords
//     "as",
//     "else",
//     "_",
//     "if",
//     "import",
//     "then",
//     "Type",
//     // Primitives
//     "true",
//     "false",
//     "Bool",
//     "String",
//     "Char",
//     "U8",
//     "U16",
//     "U32",
//     "U64",
//     "I8",
//     "I16",
//     "I32",
//     "I64",
//     "F32",
//     "F64",
// ];

fn resugar_literal(constant: &core::Literal) -> concrete::Term {
    let span = ByteSpan::default();

    match *constant {
        // FIXME: Draw these names from some environment?
        core::Literal::Bool(true) => concrete::Term::Var(span.start(), String::from("true")),
        core::Literal::Bool(false) => concrete::Term::Var(span.start(), String::from("false")),

        core::Literal::String(ref value) => concrete::Term::String(span, value.clone()),
        core::Literal::Char(value) => concrete::Term::Char(span, value),

        core::Literal::U8(value) => concrete::Term::Int(span, u64::from(value)),
        core::Literal::U16(value) => concrete::Term::Int(span, u64::from(value)),
        core::Literal::U32(value) => concrete::Term::Int(span, u64::from(value)),
        core::Literal::U64(value) => concrete::Term::Int(span, value),

        // FIXME: Underflow for negative numbers
        core::Literal::I8(value) => concrete::Term::Int(span, value as u64),
        core::Literal::I16(value) => concrete::Term::Int(span, value as u64),
        core::Literal::I32(value) => concrete::Term::Int(span, value as u64),
        core::Literal::I64(value) => concrete::Term::Int(span, value as u64),

        core::Literal::F32(value) => concrete::Term::Float(span, f64::from(value)),
        core::Literal::F64(value) => concrete::Term::Float(span, value),
    }
}

fn resugar_pi(
    scope: &Bind<(Name, Embed<Rc<core::Term>>), Rc<core::Term>>,
    prec: Prec,
) -> concrete::Term {
    let ((name, Embed(mut ann)), mut body) = nameless::unbind(scope.clone());

    // Only use explicit parameter names if the body is dependent on
    // the parameter or there is a human-readable name given.
    //
    // We'll be checking for readable names as we go, because if they've
    // survived until now they're probably desirable to retain!
    if body.free_vars().contains(&name) || name.ident().is_some() {
        // TODO: use name if it is present, and not used in the current scope
        // TODO: otherwise create a pretty name
        // TODO: add the used name to the environment

        let mut params = vec![(
            vec![(ByteIndex::default(), name.to_string())],
            resugar_term(&ann, Prec::APP),
        )];

        // Argument resugaring
        #[cfg_attr(feature = "cargo-clippy", allow(while_let_loop))] // Need NLL in stable!
        loop {
            // Share a parameter list if another pi is nested directly inside.
            // For example:
            //
            // ```
            // (a : Type) -> (b : Type -> Type) -> ...
            // (a : Type) (b : Type -> Type) -> ...
            // ```
            let ((next_name, Embed(next_ann)), next_body) = match *body {
                core::Term::Pi(ref scope) => nameless::unbind(scope.clone()),
                _ => break,
            };

            if core::Term::term_eq(&ann, &next_ann) && next_name.ident().is_some() {
                // Combine the parameters if the type annotations are
                // alpha-equivalent. For example:
                //
                // ```
                // (a : Type) (b : Type) -> ...
                // (a b : Type) -> ...
                // ```
                let next_name = (ByteIndex::default(), next_name.to_string());
                params.last_mut().unwrap().0.push(next_name);
            } else if next_body.free_vars().contains(&next_name) || next_name.ident().is_some() {
                // Add a new parameter if the body is dependent on the parameter
                // or there is a human-readable name given
                params.push((
                    vec![(ByteIndex::default(), next_name.to_string())],
                    resugar_term(&next_ann, Prec::APP),
                ));
            } else {
                // Stop collapsing parameters if we encounter a non-dependent pi type.
                return parens_if(
                    Prec::PI < prec,
                    concrete::Term::Pi(
                        ByteIndex::default(),
                        params,
                        Box::new(concrete::Term::Arrow(
                            Box::new(resugar_term(&next_ann, Prec::APP)),
                            Box::new(resugar_term(&next_body, Prec::LAM)),
                        )),
                    ),
                );
            }

            ann = next_ann;
            body = next_body;
        }

        parens_if(
            Prec::PI < prec,
            concrete::Term::Pi(
                ByteIndex::default(),
                params,
                Box::new(resugar_term(&body, Prec::LAM)),
            ),
        )
    } else {
        // The body is not dependent on the parameter - so let's use an arrow
        // instead! For example:
        //
        // ```
        // (a : Type) -> Type
        // Type -> Type
        // ```
        parens_if(
            Prec::PI < prec,
            concrete::Term::Arrow(
                Box::new(resugar_term(&ann, Prec::APP)),
                Box::new(resugar_term(&body, Prec::LAM)),
            ),
        )
    }
}

fn resugar_lam(
    scope: &Bind<(Name, Embed<Rc<core::Term>>), Rc<core::Term>>,
    prec: Prec,
) -> concrete::Term {
    let ((name, Embed(mut ann)), mut body) = nameless::unbind(scope.clone());

    // TODO: use name if it is present, and not used in the current scope
    // TODO: otherwise create a pretty name
    // TODO: add the used name to the environment
    let mut params = vec![(
        vec![(ByteIndex::default(), name.to_string())],
        Some(Box::new(resugar_term(&ann, Prec::LAM))),
    )];

    // Argument resugaring
    #[cfg_attr(feature = "cargo-clippy", allow(while_let_loop))] // Need NLL in stable!
    loop {
        // Share a parameter list if another lambda is nested directly inside.
        // For example:
        //
        // ```
        // \(a : Type) => \(b : Type -> Type) => ...
        // \(a : Type) (b : Type -> Type) => ...
        // ```
        let ((next_name, Embed(next_ann)), next_body) = match *body {
            core::Term::Lam(ref scope) => nameless::unbind(scope.clone()),
            _ => break,
        };

        // Combine the parameters if the type annotations are alpha-equivalent.
        // For example:
        //
        // ```
        // \(a : Type) (b : Type) => ...
        // \(a b : Type) => ...
        // ```
        if core::Term::term_eq(&ann, &next_ann) {
            let next_name = (ByteIndex::default(), next_name.to_string());
            params.last_mut().unwrap().0.push(next_name);
        } else {
            params.push((
                vec![(ByteIndex::default(), next_name.to_string())],
                Some(Box::new(resugar_term(&next_ann, Prec::LAM))),
            ));
        }

        ann = next_ann;
        body = next_body;
    }

    parens_if(
        Prec::LAM < prec,
        concrete::Term::Lam(
            ByteIndex::default(),
            params,
            Box::new(resugar_term(&body, Prec::LAM)),
        ),
    )
}

fn resugar_term(term: &core::Term, prec: Prec) -> concrete::Term {
    match *term {
        core::Term::Ann(ref term, ref ty) => parens_if(
            Prec::ANN < prec,
            concrete::Term::Ann(
                Box::new(resugar_term(term, Prec::LAM)),
                Box::new(resugar_term(ty, Prec::ANN)),
            ),
        ),
        core::Term::Universe(level) => {
            let level = match level {
                Level(0) => None,
                Level(level) => Some(level),
            };

            parens_if(
                Prec::APP < prec && level.is_some(),
                concrete::Term::Universe(ByteSpan::default(), level),
            )
        },
        core::Term::Literal(ref lit) => resugar_literal(lit),
        core::Term::Var(Var::Free(Name::User(ref name))) => {
            concrete::Term::Var(ByteIndex::default(), name.to_string())
        },
        // core::Term::Var(Var::Free(Name::Gen(ref _name, ref _gen))) => {}
        core::Term::Var(Var::Free(ref name)) => {
            // TODO: use name if it is present, and not used in the current scope
            // TODO: otherwise create a pretty name
            concrete::Term::Var(ByteIndex::default(), name.to_string())
        },
        core::Term::Var(Var::Bound(_, _)) => {
            // TODO: Better message
            panic!("Tried to convert a term that was not locally closed");
        },
        core::Term::Pi(ref scope) => resugar_pi(scope, prec),
        core::Term::Lam(ref scope) => resugar_lam(scope, prec),
        core::Term::App(ref fn_term, ref arg) => parens_if(
            Prec::APP < prec,
            concrete::Term::App(
                Box::new(resugar_term(fn_term, Prec::NO_WRAP)),
                vec![resugar_term(arg, Prec::NO_WRAP)], // TODO
            ),
        ),
        core::Term::If(ref cond, ref if_true, ref if_false) => parens_if(
            Prec::LAM < prec,
            concrete::Term::If(
                ByteIndex::default(),
                Box::new(resugar_term(cond, Prec::APP)),
                Box::new(resugar_term(if_true, Prec::APP)),
                Box::new(resugar_term(if_false, Prec::APP)),
            ),
        ),
        core::Term::RecordType(ref scope) => {
            let mut fields = vec![];
            let mut scope = scope.clone();

            loop {
                let ((label, Embed(expr)), body) = nameless::unbind(scope);

                fields.push((
                    ByteIndex::default(),
                    label.0.to_string(),
                    resugar_term(&expr, Prec::NO_WRAP),
                ));

                match *body {
                    core::Term::RecordType(ref next_scope) => scope = next_scope.clone(),
                    core::Term::RecordTypeEmpty => break,
                    _ => panic!("ill-formed record type"), // FIXME: better error
                }
            }

            concrete::Term::RecordType(ByteSpan::default(), fields)
        },
        core::Term::RecordTypeEmpty => concrete::Term::RecordType(ByteSpan::default(), vec![]),
        core::Term::Record(ref scope) => {
            let mut fields = vec![];
            let mut scope = scope.clone();

            loop {
                let ((label, Embed(expr)), body) = nameless::unbind(scope);
                let (expr_params, expr_body) = match resugar_term(&expr, Prec::NO_WRAP) {
                    concrete::Term::Lam(_, params, expr_body) => (params, *expr_body),
                    expr_body => (vec![], expr_body),
                };

                fields.push((
                    ByteIndex::default(),
                    label.0.to_string(),
                    expr_params,
                    None,
                    expr_body,
                ));

                match *body {
                    core::Term::Record(ref next_scope) => scope = next_scope.clone(),
                    core::Term::RecordEmpty => break,
                    _ => panic!("ill-formed record"), // FIXME: better error
                }
            }

            concrete::Term::Record(ByteSpan::default(), fields)
        },
        core::Term::RecordEmpty => concrete::Term::Record(ByteSpan::default(), vec![]),
        core::Term::Array(ref elems) => concrete::Term::Array(
            ByteSpan::default(),
            elems
                .iter()
                .map(|elem| resugar_term(elem, Prec::NO_WRAP))
                .collect(),
        ),
        core::Term::Proj(ref expr, ref label) => concrete::Term::Proj(
            Box::new(resugar_term(expr, Prec::ATOMIC)),
            ByteIndex::default(),
            label.0.clone().to_string(),
        ),
    }
}

impl Resugar<concrete::Term> for core::Literal {
    fn resugar(&self) -> concrete::Term {
        resugar_literal(self)
    }
}

impl Resugar<concrete::Term> for core::Term {
    fn resugar(&self) -> concrete::Term {
        resugar_term(self, Prec::NO_WRAP)
    }
}

impl Resugar<concrete::Term> for core::Value {
    fn resugar(&self) -> concrete::Term {
        // FIXME: Make this more efficient?
        resugar_term(&core::Term::from(self), Prec::NO_WRAP)
    }
}

impl Resugar<concrete::Term> for core::Neutral {
    fn resugar(&self) -> concrete::Term {
        // FIXME: Make this more efficient?
        resugar_term(&core::Term::from(self), Prec::NO_WRAP)
    }
}
