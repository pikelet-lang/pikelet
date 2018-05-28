use codespan::{ByteIndex, ByteOffset, ByteSpan};
use nameless::{self, Embed, GenId, Ignore, Name, Var};
use std::rc::Rc;

use syntax::concrete;
use syntax::core;

#[cfg(test)]
mod test;

/// Translate something to the corresponding core representation
pub trait Desugar<T> {
    fn desugar(&self) -> T;
}

/// Convert a sugary pi type from something like:
///
/// ```text
/// (a b : t1) -> t3
/// ```
///
/// To a bunch of nested pi types like:
///
/// ```text
/// (a : t1) -> (b : t1) -> t3
/// ```
fn desugar_pi(
    params: &[(Vec<(ByteIndex, String)>, concrete::Term)],
    body: &concrete::Term,
) -> core::RawTerm {
    let mut term = body.desugar();

    for &(ref names, ref ann) in params.iter().rev() {
        let ann = Rc::new(ann.desugar());
        for &(start, ref name) in names.iter().rev() {
            // This could be wrong... :/
            term = core::RawTerm::Pi(
                Ignore(ByteSpan::new(start, term.span().end())),
                nameless::bind(
                    (Name::user(name.clone()), Embed(ann.clone())),
                    Rc::new(term),
                ),
            );
        }
    }

    term
}

/// Convert a sugary lambda from something like:
///
/// ```text
/// \(a b : t1) c (d : t2) => t3
/// ```
///
/// To a bunch of nested lambdas like:
///
/// ```text
/// \(a : t1) => \(b : t1) => \c => \(d : t2) => t3
/// ```
fn desugar_lam(
    params: &[(Vec<(ByteIndex, String)>, Option<Box<concrete::Term>>)],
    ret_ann: Option<&concrete::Term>,
    body: &concrete::Term,
) -> core::RawTerm {
    let mut term = body.desugar();
    if let Some(ann) = ret_ann {
        term = core::RawTerm::Ann(Ignore::default(), Rc::new(term), Rc::new(ann.desugar()));
    }

    for &(ref names, ref ann) in params.iter().rev() {
        for &(start, ref name) in names.iter().rev() {
            let name = Name::user(name.clone());
            let ann = match *ann {
                None => Rc::new(core::RawTerm::Hole(Ignore::default())),
                Some(ref ann) => Rc::new(ann.desugar()),
            };

            term = core::RawTerm::Lam(
                Ignore(ByteSpan::new(start, term.span().end())),
                nameless::bind((name, Embed(ann)), Rc::new(term)),
            );
        }
    }

    term
}

fn desugar_app(fn_expr: &concrete::Term, args: &[concrete::Term]) -> core::RawTerm {
    let mut term = fn_expr.desugar();

    for arg in args.iter() {
        term = core::RawTerm::App(Rc::new(term), Rc::new(arg.desugar()))
    }

    term
}

fn desugar_record_ty(
    span: ByteSpan,
    fields: &[(ByteIndex, String, concrete::Term)],
) -> core::RawTerm {
    let mut term = core::RawTerm::RecordTypeEmpty(Ignore(ByteSpan::new(span.end(), span.end())));

    for &(start, ref label, ref ann) in fields.iter().rev() {
        term = core::RawTerm::RecordType(
            Ignore(ByteSpan::new(start, term.span().end())),
            nameless::bind(
                (
                    core::Label(Name::user(label.clone())),
                    Embed(Rc::new(ann.desugar())),
                ),
                Rc::new(term),
            ),
        );
    }

    term
}

fn desugar_record(
    span: ByteSpan,
    fields: &[(
        ByteIndex,
        String,
        concrete::LamParams,
        Option<Box<concrete::Term>>,
        concrete::Term,
    )],
) -> core::RawTerm {
    let mut term = core::RawTerm::RecordEmpty(Ignore(ByteSpan::new(span.end(), span.end())));

    for &(start, ref label, ref params, ref ret_ann, ref value) in fields.iter().rev() {
        term = core::RawTerm::Record(
            Ignore(ByteSpan::new(start, term.span().end())),
            nameless::bind(
                (
                    core::Label(Name::user(label.clone())),
                    Embed(Rc::new(desugar_lam(
                        params,
                        ret_ann.as_ref().map(<_>::as_ref),
                        value,
                    ))),
                ),
                Rc::new(term),
            ),
        );
    }

    term
}

impl Desugar<core::RawModule> for concrete::Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    fn desugar(&self) -> core::RawModule {
        match *self {
            concrete::Module::Valid { ref declarations } => {
                // The type claims that we have encountered so far! We'll use these when
                // we encounter their corresponding definitions later as type annotations
                let mut prev_claim = None;
                // The definitions, desugared from the concrete syntax
                let mut definitions = Vec::<core::RawDefinition>::new();

                for declaration in declarations {
                    match *declaration {
                        concrete::Declaration::Claim {
                            name: (_, ref name),
                            ref ann,
                            ..
                        } => match prev_claim.take() {
                            Some((name, ann)) => {
                                let term = Rc::new(core::RawTerm::Hole(Ignore::default()));
                                definitions.push(core::RawDefinition { name, term, ann });
                            },
                            None => prev_claim = Some((name.clone(), Rc::new(ann.desugar()))),
                        },
                        concrete::Declaration::Definition {
                            ref name,
                            ref params,
                            ref ann,
                            ref body,
                            ref wheres,
                            ..
                        } => {
                            let default_span = Ignore::default();

                            if !wheres.is_empty() {
                                unimplemented!("where clauses");
                            }

                            match prev_claim.take() {
                                None => definitions.push(core::RawDefinition {
                                    name: name.clone(),
                                    ann: Rc::new(core::RawTerm::Hole(default_span)),
                                    term: Rc::new(desugar_lam(
                                        params,
                                        ann.as_ref().map(<_>::as_ref),
                                        body,
                                    )),
                                }),
                                Some((claim_name, ann)) => {
                                    if claim_name == *name {
                                        definitions.push(core::RawDefinition {
                                            name: name.clone(),
                                            ann,
                                            term: Rc::new(desugar_lam(params, None, body)),
                                        });
                                    } else {
                                        definitions.push(core::RawDefinition {
                                            name: claim_name.clone(),
                                            ann,
                                            term: Rc::new(core::RawTerm::Hole(default_span)),
                                        });
                                        definitions.push(core::RawDefinition {
                                            name: name.clone(),
                                            ann: Rc::new(core::RawTerm::Hole(default_span)),
                                            term: Rc::new(desugar_lam(params, None, body)),
                                        });
                                    }
                                },
                            };
                        },
                        concrete::Declaration::Error(_) => unimplemented!("error recovery"),
                    }
                }

                core::RawModule { definitions }
            },
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<core::RawTerm> for concrete::Term {
    /// Convert a term in the concrete syntax into a core term
    fn desugar(&self) -> core::RawTerm {
        let span = Ignore(self.span());
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = Rc::new(expr.desugar());
                let ty = Rc::new(ty.desugar());

                core::RawTerm::Ann(span, expr, ty)
            },
            concrete::Term::Universe(_, level) => {
                core::RawTerm::Universe(span, core::Level(level.unwrap_or(0)))
            },
            concrete::Term::String(_, ref value) => {
                core::RawTerm::Literal(span, core::RawLiteral::String(value.clone()))
            },
            concrete::Term::Char(_, value) => {
                core::RawTerm::Literal(span, core::RawLiteral::Char(value))
            },
            concrete::Term::Int(_, value) => {
                core::RawTerm::Literal(span, core::RawLiteral::Int(value))
            },
            concrete::Term::Float(_, value) => {
                core::RawTerm::Literal(span, core::RawLiteral::Float(value))
            },
            concrete::Term::Array(_, ref elems) => core::RawTerm::Array(
                span,
                elems.iter().map(|elem| Rc::new(elem.desugar())).collect(),
            ),
            concrete::Term::Hole(_) => core::RawTerm::Hole(span),
            concrete::Term::Var(_, ref x) => {
                core::RawTerm::Var(span, Var::Free(Name::user(x.clone())))
            },
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(params, None, body),
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = Name::from(GenId::fresh());
                let ann = Rc::new(ann.desugar());
                let body = Rc::new(body.desugar());

                core::RawTerm::Pi(span, nameless::bind((name, Embed(ann)), body))
            },
            concrete::Term::App(ref fn_expr, ref args) => desugar_app(fn_expr, args),
            concrete::Term::Let(_, ref _declarations, ref _body) => unimplemented!("let bindings"),
            concrete::Term::If(start, ref cond, ref if_true, ref if_false) => core::RawTerm::If(
                Ignore(start),
                Rc::new(cond.desugar()),
                Rc::new(if_true.desugar()),
                Rc::new(if_false.desugar()),
            ),
            concrete::Term::RecordType(span, ref fields) => desugar_record_ty(span, fields),
            concrete::Term::Record(span, ref fields) => desugar_record(span, fields),
            concrete::Term::Proj(ref tm, label_start, ref label) => {
                let label_span = Ignore(ByteSpan::from_offset(
                    label_start,
                    ByteOffset::from_str(label),
                ));

                core::RawTerm::Proj(
                    span,
                    Rc::new(tm.desugar()),
                    label_span,
                    core::Label(Name::user(label.clone())),
                )
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}
