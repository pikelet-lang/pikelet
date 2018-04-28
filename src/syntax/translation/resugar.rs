use codespan::{ByteIndex, ByteSpan};
use nameless::{self, BoundTerm, Embed, Name, Var};

use syntax::concrete;
use syntax::core;

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
    match should_wrap {
        false => inner,
        true => concrete::Term::Parens(ByteSpan::default(), Box::new(inner)),
    }
}

// TODO: Use this for name-avoidance
// const USED_NAMES: &[&str] = &[
//     // Keywords
//     "as",
//     "else",
//     "_",
//     "module",
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

/// Translate something to the corresponding concrete representation
pub trait Resugar<T> {
    fn resugar(&self) -> T {
        self.resugar_prec(Prec::NO_WRAP)
    }

    fn resugar_prec(&self, prec: Prec) -> T;
}

impl Resugar<(concrete::Declaration, concrete::Declaration)> for core::Definition {
    fn resugar_prec(&self, _: Prec) -> (concrete::Declaration, concrete::Declaration) {
        // build up the type claim
        let new_ann = concrete::Declaration::Claim {
            name: (ByteIndex::default(), self.name.clone()),
            ann: self.ann.resugar_prec(Prec::ANN),
        };

        // build up the concrete definition
        let new_definition = {
            // pull lambda arguments from the body into the definition
            let (params, body) = match self.term.resugar_prec(Prec::ANN) {
                concrete::Term::Lam(_, params, body) => (params, *body),
                body => (vec![], body),
            };

            concrete::Declaration::Definition {
                span: ByteSpan::default(),
                name: self.name.clone(),
                params,
                body,
                wheres: vec![],
            }
        };

        (new_ann, new_definition)
    }
}

impl Resugar<concrete::Module> for core::Module {
    fn resugar_prec(&self, _: Prec) -> concrete::Module {
        use std::iter;

        let declarations = self.definitions
            .iter()
            .flat_map(|definition| {
                let (new_ann, new_definition) = definition.resugar();
                iter::once(new_ann).chain(iter::once(new_definition))
            })
            .collect();

        concrete::Module::Valid {
            name: (ByteIndex::default(), self.name.clone()),
            declarations,
        }
    }
}

impl Resugar<concrete::Term> for core::Constant {
    fn resugar_prec(&self, _: Prec) -> concrete::Term {
        use syntax::concrete::{Literal, Term};
        use syntax::core::Constant;

        let span = ByteSpan::default();

        match *self {
            // FIXME: Draw these names from some environment?
            Constant::Bool(true) => Term::Var(span.start(), String::from("true")),
            Constant::Bool(false) => Term::Var(span.start(), String::from("false")),

            Constant::String(ref value) => Term::Literal(span, Literal::String(value.clone())),
            Constant::Char(value) => Term::Literal(span, Literal::Char(value)),

            Constant::U8(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::U16(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::U32(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::U64(value) => Term::Literal(span, Literal::Int(value)),

            // FIXME: Underflow for negative numbers
            Constant::I8(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::I16(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::I32(value) => Term::Literal(span, Literal::Int(value as u64)),
            Constant::I64(value) => Term::Literal(span, Literal::Int(value as u64)),

            Constant::F32(value) => Term::Literal(span, Literal::Float(value as f64)),
            Constant::F64(value) => Term::Literal(span, Literal::Float(value)),

            // FIXME: Draw these names from some environment?
            Constant::BoolType => Term::Var(span.start(), String::from("Bool")),
            Constant::StringType => Term::Var(span.start(), String::from("String")),
            Constant::CharType => Term::Var(span.start(), String::from("Char")),
            Constant::U8Type => Term::Var(span.start(), String::from("U8")),
            Constant::U16Type => Term::Var(span.start(), String::from("U16")),
            Constant::U32Type => Term::Var(span.start(), String::from("U32")),
            Constant::U64Type => Term::Var(span.start(), String::from("U64")),
            Constant::I8Type => Term::Var(span.start(), String::from("I8")),
            Constant::I16Type => Term::Var(span.start(), String::from("I16")),
            Constant::I32Type => Term::Var(span.start(), String::from("I32")),
            Constant::I64Type => Term::Var(span.start(), String::from("I64")),
            Constant::F32Type => Term::Var(span.start(), String::from("F32")),
            Constant::F64Type => Term::Var(span.start(), String::from("F64")),
        }
    }
}

impl Resugar<concrete::Term> for core::Term {
    fn resugar_prec(&self, prec: Prec) -> concrete::Term {
        match *self {
            core::Term::Ann(_, ref term, ref ty) => parens_if(
                Prec::ANN < prec,
                concrete::Term::Ann(
                    Box::new(term.resugar_prec(Prec::LAM)),
                    Box::new(ty.resugar_prec(Prec::ANN)),
                ),
            ),
            core::Term::Universe(_, level) => {
                let level = match level {
                    core::Level(0) => None,
                    core::Level(level) => Some(level),
                };

                parens_if(
                    Prec::APP < prec && level.is_some(),
                    concrete::Term::Universe(ByteSpan::default(), level),
                )
            },
            core::Term::Constant(_, ref c) => c.resugar(),
            core::Term::Var(_, Var::Free(Name::User(ref name))) => {
                concrete::Term::Var(ByteIndex::default(), name.to_string())
            },
            // core::Term::Var(_, Var::Free(Name::Gen(ref _name, ref _gen))) => {}
            core::Term::Var(_, Var::Free(ref name)) => {
                // TODO: use name if it is present, and not used in the current scope
                // TODO: otherwise create a pretty name
                concrete::Term::Var(ByteIndex::default(), name.to_string())
            },
            core::Term::Var(_, Var::Bound(_, _)) => {
                // TODO: Better message
                panic!("Tried to convert a term that was not locally closed");
            },
            core::Term::Pi(_, ref scope) => {
                let ((name, Embed(mut ann)), mut body) = nameless::unbind(scope.clone());

                // Only use explict parameter names if the body is dependent on
                // the parameter or there is a human-readable name given
                if body.free_vars().contains(&name) || name.name().is_some() {
                    // TODO: use name if it is present, and not used in the current scope
                    // TODO: otherwise create a pretty name
                    // TODO: add the used name to the environment

                    let mut params = vec![(
                        vec![(ByteIndex::default(), name.to_string())],
                        ann.resugar_prec(Prec::APP),
                    )];

                    // Argument resugaring
                    loop {
                        // Share a parameter list if another pi is nested
                        // directly inside. For example:
                        //
                        // ```
                        // (a : Type) -> (b : Type -> Type) -> ...
                        // (a : Type) (b : Type -> Type) -> ...
                        // ```
                        let ((next_name, Embed(next_ann)), next_body) = match *body {
                            core::Term::Pi(_, ref scope) => nameless::unbind(scope.clone()),
                            _ => break,
                        };

                        if core::Term::term_eq(&ann, &next_ann) && next_name.name().is_some() {
                            // Combine the parameters if the type annotations are
                            // alpha-equivalent. For example:
                            //
                            // ```
                            // (a : Type) (b : Type) -> ...
                            // (a b : Type) -> ...
                            // ```
                            let next_name = (ByteIndex::default(), next_name.to_string());
                            params.last_mut().unwrap().0.push(next_name);
                        } else if next_body.free_vars().contains(&next_name)
                            || next_name.name().is_some()
                        {
                            // Add a new parameter if the body is dependent on the
                            // parameter or there is a human-readable name given
                            params.push((
                                vec![(ByteIndex::default(), next_name.to_string())],
                                next_ann.resugar_prec(Prec::APP),
                            ));
                        } else {
                            // Stop collapsing parameters if we encounter a
                            // non-dependent pi type.
                            return parens_if(
                                Prec::PI < prec,
                                concrete::Term::Pi(
                                    ByteIndex::default(),
                                    params,
                                    Box::new(concrete::Term::Arrow(
                                        Box::new(next_ann.resugar_prec(Prec::APP)),
                                        Box::new(next_body.resugar_prec(Prec::LAM)),
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
                            Box::new(body.resugar_prec(Prec::LAM)),
                        ),
                    )
                } else {
                    // The body is not dependent on the parameter - so let's use
                    // an arrow instead! For example:
                    //
                    // ```
                    // (a : Type) -> Type
                    // Type -> Type
                    // ```
                    parens_if(
                        Prec::PI < prec,
                        concrete::Term::Arrow(
                            Box::new(ann.resugar_prec(Prec::APP)),
                            Box::new(body.resugar_prec(Prec::LAM)),
                        ),
                    )
                }
            },
            core::Term::Lam(_, ref scope) => {
                let ((name, Embed(mut ann)), mut body) = nameless::unbind(scope.clone());

                // TODO: use name if it is present, and not used in the current scope
                // TODO: otherwise create a pretty name
                // TODO: add the used name to the environment
                let mut params = vec![(
                    vec![(ByteIndex::default(), name.to_string())],
                    Some(Box::new(ann.resugar_prec(Prec::LAM))),
                )];

                // Argument resugaring
                loop {
                    // Share a parameter list if another lambda is nested
                    // directly inside. For example:
                    //
                    // ```
                    // \(a : Type) => \(b : Type -> Type) => ...
                    // \(a : Type) (b : Type -> Type) => ...
                    // ```
                    let ((next_name, Embed(next_ann)), next_body) = match *body {
                        core::Term::Lam(_, ref scope) => nameless::unbind(scope.clone()),
                        _ => break,
                    };

                    // Combine the parameters if the type annotations are
                    // alpha-equivalent. For example:
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
                            Some(Box::new(next_ann.resugar_prec(Prec::LAM))),
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
                        Box::new(body.resugar_prec(Prec::LAM)),
                    ),
                )
            },
            core::Term::App(ref fn_term, ref arg) => parens_if(
                Prec::APP < prec,
                concrete::Term::App(
                    Box::new(fn_term.resugar_prec(Prec::NO_WRAP)),
                    vec![arg.resugar_prec(Prec::NO_WRAP)], // TODO
                ),
            ),
            core::Term::If(_, ref cond, ref if_true, ref if_false) => parens_if(
                Prec::LAM < prec,
                concrete::Term::If(
                    ByteIndex::default(),
                    Box::new(cond.resugar_prec(Prec::APP)),
                    Box::new(if_true.resugar_prec(Prec::APP)),
                    Box::new(if_false.resugar_prec(Prec::APP)),
                ),
            ),
            core::Term::RecordType(_, ref label, ref ann, ref rest) => {
                let mut fields = vec![];
                let mut label = label;
                let mut ann = ann;
                let mut rest = rest;

                loop {
                    fields.push((
                        ByteIndex::default(),
                        label.0.clone(),
                        Box::new(ann.resugar_prec(Prec::NO_WRAP)),
                    ));

                    match **rest {
                        core::Term::RecordType(_, ref next_label, ref next_ann, ref next_rest) => {
                            label = next_label;
                            ann = next_ann;
                            rest = next_rest;
                        },
                        core::Term::EmptyRecordType(_) => break,
                        _ => panic!("ill-formed record type"), // FIXME: better error
                    }
                }

                concrete::Term::RecordType(ByteSpan::default(), fields)
            },
            core::Term::Record(_, ref label, ref expr, ref rest) => {
                let mut fields = vec![];
                let mut label = label;
                let mut expr = expr;
                let mut rest = rest;

                loop {
                    fields.push((
                        ByteIndex::default(),
                        label.0.clone(),
                        Box::new(expr.resugar_prec(Prec::NO_WRAP)),
                    ));

                    match **rest {
                        core::Term::Record(_, ref next_label, ref next_expr, ref next_rest) => {
                            label = next_label;
                            expr = next_expr;
                            rest = next_rest;
                        },
                        core::Term::EmptyRecord(_) => break,
                        _ => panic!("ill-formed record"), // FIXME: better error
                    }
                }

                concrete::Term::Record(ByteSpan::default(), fields)
            },
            core::Term::EmptyRecordType(_) => {
                concrete::Term::RecordType(ByteSpan::default(), vec![])
            },
            core::Term::EmptyRecord(_) => concrete::Term::Record(ByteSpan::default(), vec![]),
            core::Term::Proj(_, ref expr, _, ref label) => concrete::Term::Proj(
                Box::new(expr.resugar_prec(Prec::ATOMIC)),
                ByteIndex::default(),
                label.0.clone(),
            ),
        }
    }
}

impl Resugar<concrete::Term> for core::Value {
    fn resugar_prec(&self, prec: Prec) -> concrete::Term {
        // FIXME: Make this more efficient?
        core::Term::from(self).resugar_prec(prec)
    }
}

impl Resugar<concrete::Term> for core::Neutral {
    fn resugar_prec(&self, prec: Prec) -> concrete::Term {
        // FIXME: Make this more efficient?
        core::Term::from(self).resugar_prec(prec)
    }
}
