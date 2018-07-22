use codespan::{ByteOffset, ByteSpan};
use moniker::{Embed, FreeVar, GenId, Ignore, Nest, Scope, Var};

use syntax::concrete;
use syntax::raw;
use syntax::{Label, Level};

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
fn desugar_pi(params: &[concrete::PiParamGroup], body: &concrete::Term) -> raw::RcTerm {
    let mut term = body.desugar();

    for &(ref names, ref ann) in params.iter().rev() {
        let ann = raw::RcTerm::from(ann.desugar());
        for &(start, ref name) in names.iter().rev() {
            // This could be wrong... :/
            term = raw::RcTerm::from(raw::Term::Pi(
                Ignore(ByteSpan::new(start, term.span().end())),
                Scope::new((FreeVar::user(name.clone()), Embed(ann.clone())), term),
            ));
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
    params: &[concrete::LamParamGroup],
    ret_ann: Option<&concrete::Term>,
    body: &concrete::Term,
) -> raw::RcTerm {
    let mut term = body.desugar();
    if let Some(ann) = ret_ann {
        term = raw::RcTerm::from(raw::Term::Ann(Ignore::default(), term, ann.desugar()));
    }

    for &(ref names, ref ann) in params.iter().rev() {
        for &(start, ref name) in names.iter().rev() {
            let name = FreeVar::user(name.clone());
            let ann = match *ann {
                None => raw::RcTerm::from(raw::Term::Hole(Ignore::default())),
                Some(ref ann) => ann.desugar(),
            };

            term = raw::RcTerm::from(raw::Term::Lam(
                Ignore(ByteSpan::new(start, term.span().end())),
                Scope::new((name, Embed(ann)), term),
            ));
        }
    }

    term
}

fn desugar_app(fn_expr: &concrete::Term, args: &[concrete::Term]) -> raw::RcTerm {
    let mut term = fn_expr.desugar();

    for arg in args.iter() {
        term = raw::RcTerm::from(raw::Term::App(raw::RcTerm::from(term), arg.desugar()));
    }

    term
}

fn desugar_record_ty(span: ByteSpan, fields: &[concrete::RecordTypeField]) -> raw::RcTerm {
    let span = ByteSpan::new(span.end(), span.end());
    let mut term = raw::RcTerm::from(raw::Term::RecordTypeEmpty(Ignore(span)));

    for &(start, ref label, ref ann) in fields.iter().rev() {
        term = raw::RcTerm::from(raw::Term::RecordType(
            Ignore(ByteSpan::new(start, term.span().end())),
            Scope::new(
                (
                    Label(FreeVar::user(label.clone())),
                    Embed(raw::RcTerm::from(ann.desugar())),
                ),
                raw::RcTerm::from(term),
            ),
        ));
    }

    term
}

fn desugar_record(span: ByteSpan, fields: &[concrete::RecordField]) -> raw::RcTerm {
    let span = ByteSpan::new(span.end(), span.end());
    let mut term = raw::RcTerm::from(raw::Term::RecordEmpty(Ignore(span)));

    for &(start, ref label, ref params, ref ret_ann, ref value) in fields.iter().rev() {
        term = raw::RcTerm::from(raw::Term::Record(
            Ignore(ByteSpan::new(start, term.span().end())),
            Scope::new(
                (
                    Label(FreeVar::user(label.clone())),
                    Embed(desugar_lam(
                        params,
                        ret_ann.as_ref().map(<_>::as_ref),
                        value,
                    )),
                ),
                raw::RcTerm::from(term),
            ),
        ));
    }

    term
}

impl Desugar<raw::Module> for concrete::Module {
    /// Convert the module in the concrete syntax to a module in the core syntax
    fn desugar(&self) -> raw::Module {
        match *self {
            concrete::Module::Valid { ref declarations } => {
                // The type claims that we have encountered so far! We'll use these when
                // we encounter their corresponding definitions later as type annotations
                let mut prev_claim = None;
                // The definitions, desugared from the concrete syntax
                let mut definitions = Vec::<(FreeVar<String>, Embed<raw::Definition>)>::new();

                for declaration in declarations {
                    match *declaration {
                        concrete::Declaration::Claim {
                            name: (_, ref name),
                            ref ann,
                            ..
                        } => match prev_claim.take() {
                            Some((name, ann)) => {
                                let term = raw::RcTerm::from(raw::Term::Hole(Ignore::default()));
                                definitions.push((
                                    FreeVar::user(name),
                                    Embed(raw::Definition { term, ann }),
                                ));
                            },
                            None => prev_claim = Some((name.clone(), ann.desugar())),
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
                                None => definitions.push((
                                    FreeVar::user(name.as_ref()),
                                    Embed(raw::Definition {
                                        ann: raw::RcTerm::from(raw::Term::Hole(default_span)),
                                        term: desugar_lam(
                                            params,
                                            ann.as_ref().map(<_>::as_ref),
                                            body,
                                        ),
                                    }),
                                )),
                                Some((claim_name, ann)) => {
                                    if claim_name == *name {
                                        definitions.push((
                                            FreeVar::user(name.as_ref()),
                                            Embed(raw::Definition {
                                                ann,
                                                term: desugar_lam(params, None, body),
                                            }),
                                        ));
                                    } else {
                                        definitions.push((
                                            FreeVar::user(name.as_ref()),
                                            Embed(raw::Definition {
                                                ann,
                                                term: raw::RcTerm::from(raw::Term::Hole(
                                                    default_span,
                                                )),
                                            }),
                                        ));
                                        definitions.push((
                                            FreeVar::user(name.as_ref()),
                                            Embed(raw::Definition {
                                                ann: raw::RcTerm::from(raw::Term::Hole(
                                                    default_span,
                                                )),
                                                term: desugar_lam(params, None, body),
                                            }),
                                        ));
                                    }
                                },
                            };
                        },
                        concrete::Declaration::Error(_) => unimplemented!("error recovery"),
                    }
                }

                raw::Module {
                    definitions: Nest::new(definitions),
                }
            },
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<raw::RcTerm> for concrete::Term {
    /// Convert a term in the concrete syntax into a core term
    fn desugar(&self) -> raw::RcTerm {
        let span = Ignore(self.span());
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(),
            concrete::Term::Ann(ref expr, ref ty) => {
                let expr = expr.desugar();
                let ty = ty.desugar();

                raw::RcTerm::from(raw::Term::Ann(span, expr, ty))
            },
            concrete::Term::Universe(_, level) => {
                raw::RcTerm::from(raw::Term::Universe(span, Level(level.unwrap_or(0))))
            },
            concrete::Term::String(_, ref value) => raw::RcTerm::from(raw::Term::Literal(
                span,
                raw::Literal::String(value.clone()),
            )),
            concrete::Term::Char(_, value) => {
                raw::RcTerm::from(raw::Term::Literal(span, raw::Literal::Char(value)))
            },
            concrete::Term::Int(_, value) => {
                raw::RcTerm::from(raw::Term::Literal(span, raw::Literal::Int(value)))
            },
            concrete::Term::Float(_, value) => {
                raw::RcTerm::from(raw::Term::Literal(span, raw::Literal::Float(value)))
            },
            concrete::Term::Array(_, ref elems) => raw::RcTerm::from(raw::Term::Array(
                span,
                elems.iter().map(|elem| elem.desugar()).collect(),
            )),
            concrete::Term::Hole(_) => raw::RcTerm::from(raw::Term::Hole(span)),
            concrete::Term::Var(_, ref x) => {
                raw::RcTerm::from(raw::Term::Var(span, Var::Free(FreeVar::user(x.clone()))))
            },
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(params, None, body),
            concrete::Term::Arrow(ref ann, ref body) => {
                let name = FreeVar::from(GenId::fresh());
                let ann = ann.desugar();
                let body = body.desugar();

                raw::RcTerm::from(raw::Term::Pi(span, Scope::new((name, Embed(ann)), body)))
            },
            concrete::Term::App(ref fn_expr, ref args) => desugar_app(fn_expr, args),
            concrete::Term::Let(_, ref _declarations, ref _body) => unimplemented!("let bindings"),
            concrete::Term::If(start, ref cond, ref if_true, ref if_false) => {
                raw::RcTerm::from(raw::Term::If(
                    Ignore(start),
                    cond.desugar(),
                    if_true.desugar(),
                    if_false.desugar(),
                ))
            },
            concrete::Term::RecordType(span, ref fields) => desugar_record_ty(span, fields),
            concrete::Term::Record(span, ref fields) => desugar_record(span, fields),
            concrete::Term::Proj(ref tm, label_start, ref label) => {
                let label_span = Ignore(ByteSpan::from_offset(
                    label_start,
                    ByteOffset::from_str(label),
                ));

                raw::RcTerm::from(raw::Term::Proj(
                    span,
                    tm.desugar(),
                    label_span,
                    Label(FreeVar::user(label.clone())),
                ))
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}
