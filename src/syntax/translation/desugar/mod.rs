use codespan::{ByteOffset, ByteSpan};
use im::HashMap;
use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};

use syntax::concrete;
use syntax::raw;
use syntax::{Label, Level, LevelShift};

#[cfg(test)]
mod tests;

/// The environment used when desugaring from the concrete to raw syntax
#[derive(Debug, Clone)]
pub struct DesugarEnv {
    /// An environment that maps strings to unique free variables
    ///
    /// This is a persistent map so that we can create new environments as we enter
    /// new scopes, allowing us to properly model variable shadowing.
    ///
    /// If we arrive at a variable that has not already been assigned a free name,
    /// we assume that it is a global name.
    locals: HashMap<String, FreeVar<String>>,
}

impl DesugarEnv {
    pub fn new(mappings: HashMap<String, FreeVar<String>>) -> DesugarEnv {
        DesugarEnv { locals: mappings }
    }

    pub fn on_item(&mut self, name: &str) -> Binder<String> {
        if let Some(free_var) = self.locals.get(name) {
            return Binder(free_var.clone());
        }
        Binder(self.on_binding(name))
    }

    pub fn on_binding(&mut self, name: &str) -> FreeVar<String> {
        let name = name.to_owned();
        let free_var = FreeVar::fresh_named(name.clone());
        self.locals.insert(name, free_var.clone());
        free_var
    }

    pub fn on_name(&self, span: ByteSpan, name: &str, shift: u32) -> raw::RcTerm {
        let free_var = match self.locals.get(name) {
            None => FreeVar::fresh_named(name),
            Some(free_var) => free_var.clone(),
        };

        raw::RcTerm::from(raw::Term::Var(span, Var::Free(free_var), LevelShift(shift)))
    }
}

/// Translate something to the corresponding core representation
pub trait Desugar<T> {
    fn desugar(&self, env: &DesugarEnv) -> T;
}

/// Convert a sugary pi type from something like:
///
/// ```text
/// (a b : t1) (c : t2) -> t3
/// ```
///
/// To a bunch of nested pi types like:
///
/// ```text
/// (a : t1) -> (b : t1) -> (c : t2) -> t3
/// ```
fn desugar_pi(
    env: &DesugarEnv,
    param_groups: &[concrete::PiParamGroup],
    body: &concrete::Term,
) -> raw::RcTerm {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = raw::RcTerm::from(ann.desugar(&env));
        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    params
        .into_iter()
        .rev()
        .fold(body.desugar(&env), |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Pi(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        })
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
    env: &DesugarEnv,
    param_groups: &[concrete::LamParamGroup],
    return_ann: Option<&concrete::Term>,
    body: &concrete::Term,
) -> raw::RcTerm {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = match *ann {
            None => raw::RcTerm::from(raw::Term::Hole(ByteSpan::default())),
            Some(ref ann) => ann.desugar(&env),
        };

        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    let body = match return_ann {
        None => body.desugar(&env),
        Some(ann) => raw::RcTerm::from(raw::Term::Ann(body.desugar(&env), ann.desugar(&env))),
    };

    params
        .into_iter()
        .rev()
        .fold(body, |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Lam(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        })
}

fn desugar_bindings(env: &mut DesugarEnv, items: &[concrete::Item]) -> Vec<raw::Item> {
    items
        .iter()
        .map(|concrete_item| match *concrete_item {
            concrete::Item::Declaration {
                name: (start, ref name),
                ref ann,
            } => {
                let term = ann.desugar(&env);

                raw::Item::Declaration {
                    label_span: ByteSpan::from_offset(start, ByteOffset::from_str(name)),
                    label: Label(name.clone()),
                    binder: env.on_item(name),
                    term,
                }
            },
            concrete::Item::Definition {
                name: (start, ref name),
                ref params,
                ref return_ann,
                ref body,
            } => {
                let return_ann = return_ann.as_ref().map(<_>::as_ref);
                let term = desugar_lam(&env, params, return_ann, body);

                raw::Item::Definition {
                    label_span: ByteSpan::from_offset(start, ByteOffset::from_str(name)),
                    label: Label(name.clone()),
                    binder: env.on_item(name),
                    term,
                }
            },
            concrete::Item::Error(_) => unimplemented!("error recovery"),
        }).collect()
}

fn desugar_let(env: &DesugarEnv, items: &[concrete::Item], body: &concrete::Term) -> raw::RcTerm {
    let mut env = env.clone();
    let items = desugar_bindings(&mut env, items);
    let body = body.desugar(&env);

    let hole = raw::RcTerm::from(raw::Term::Hole(ByteSpan::default()));

    items.into_iter().rev().fold(body, |acc, item| match item {
        raw::Item::Declaration { .. } => acc, // TODO: Let declarations (maybe not necessary?)
        raw::Item::Definition {
            label_span,
            label: _,
            binder,
            term,
        } => raw::RcTerm::from(raw::Term::Let(
            label_span.with_end(term.span().end()),
            Scope::new((binder, Embed((hole.clone(), term))), acc),
        )),
    })
}

fn desugar_record_ty(
    env: &DesugarEnv,
    span: ByteSpan,
    fields: &[concrete::RecordTypeField],
) -> raw::RcTerm {
    let mut env = env.clone();

    let fields = fields
        .iter()
        .map(|field| {
            let (_, ref label) = field.label;
            let ann = field.ann.desugar(&env);
            let free_var = match field.binder {
                Some((_, ref binder)) => env.on_binding(binder),
                None => env.on_binding(label),
            };

            (Label(label.clone()), Binder(free_var), Embed(ann))
        }).collect::<Vec<_>>();

    raw::RcTerm::from(raw::Term::RecordType(
        span,
        Scope::new(Nest::new(fields), ()),
    ))
}

fn desugar_record(
    env: &DesugarEnv,
    span: ByteSpan,
    fields: &[concrete::RecordField],
) -> raw::RcTerm {
    let mut env = env.clone();

    let fields = fields
        .iter()
        .map(|field| {
            let expr = desugar_lam(
                &env,
                &field.params,
                field.return_ann.as_ref().map(<_>::as_ref),
                &field.term,
            );
            let free_var = env.on_binding(&field.label.1);
            (Label(field.label.1.clone()), Binder(free_var), Embed(expr))
        }).collect::<Vec<_>>();

    raw::RcTerm::from(raw::Term::Record(span, Scope::new(Nest::new(fields), ())))
}

impl Desugar<raw::Module> for concrete::Module {
    fn desugar(&self, env: &DesugarEnv) -> raw::Module {
        let concrete_items = match *self {
            concrete::Module::Valid { ref items } => items,
            concrete::Module::Error(_) => unimplemented!("error recovery"),
        };

        raw::Module {
            items: desugar_bindings(&mut env.clone(), concrete_items),
        }
    }
}

impl Desugar<raw::Literal> for concrete::Literal {
    fn desugar(&self, _: &DesugarEnv) -> raw::Literal {
        match *self {
            concrete::Literal::String(span, ref value) => raw::Literal::String(span, value.clone()),
            concrete::Literal::Char(span, value) => raw::Literal::Char(span, value),
            concrete::Literal::Int(span, value) => raw::Literal::Int(span, value),
            concrete::Literal::Float(span, value) => raw::Literal::Float(span, value),
        }
    }
}

impl Desugar<(raw::RcPattern, DesugarEnv)> for concrete::Pattern {
    fn desugar(&self, env: &DesugarEnv) -> (raw::RcPattern, DesugarEnv) {
        let span = self.span();
        match *self {
            concrete::Pattern::Parens(_, ref pattern) => pattern.desugar(env),
            concrete::Pattern::Ann(ref pattern, ref ty) => {
                let ty = ty.desugar(env);
                let (pattern, env) = pattern.desugar(env);
                let ann_pattern = raw::RcPattern::from(raw::Pattern::Ann(pattern, Embed(ty)));

                (ann_pattern, env)
            },
            concrete::Pattern::Name(_, ref name, shift) => match (env.locals.get(name), shift) {
                (Some(free_var), shift) => {
                    let var = Var::Free(free_var.clone());
                    let shift = LevelShift(shift.unwrap_or(0));
                    let pattern = raw::RcPattern::from(raw::Pattern::Var(span, Embed(var), shift));

                    (pattern, env.clone())
                },
                (None, Some(shift)) => {
                    let var = Var::Free(FreeVar::fresh_named(name.clone()));
                    let shift = LevelShift(shift);
                    let pattern = raw::RcPattern::from(raw::Pattern::Var(span, Embed(var), shift));

                    (pattern, env.clone())
                },
                (None, None) => {
                    let mut env = env.clone();
                    let free_var = env.on_binding(name);
                    let binder = Binder(free_var);
                    let pattern = raw::RcPattern::from(raw::Pattern::Binder(span, binder));

                    (pattern, env)
                },
            },
            concrete::Pattern::Literal(ref literal) => (
                raw::RcPattern::from(raw::Pattern::Literal(literal.desugar(env))),
                env.clone(),
            ),
            concrete::Pattern::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<raw::RcTerm> for concrete::Term {
    fn desugar(&self, env: &DesugarEnv) -> raw::RcTerm {
        let span = self.span();
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(env),
            concrete::Term::Ann(ref expr, ref ty) => {
                raw::RcTerm::from(raw::Term::Ann(expr.desugar(env), ty.desugar(env)))
            },
            concrete::Term::Universe(_, level) => {
                raw::RcTerm::from(raw::Term::Universe(span, Level(level.unwrap_or(0))))
            },
            concrete::Term::Literal(ref literal) => {
                raw::RcTerm::from(raw::Term::Literal(literal.desugar(env)))
            },
            concrete::Term::Array(_, ref elems) => raw::RcTerm::from(raw::Term::Array(
                span,
                elems.iter().map(|elem| elem.desugar(env)).collect(),
            )),
            concrete::Term::Hole(_) => raw::RcTerm::from(raw::Term::Hole(span)),
            concrete::Term::Name(_, ref name, shift) => env.on_name(span, name, shift.unwrap_or(0)),
            concrete::Term::Extern(_, name_span, ref name, ref ty) => raw::RcTerm::from(
                raw::Term::Extern(span, name_span, name.clone(), ty.desugar(env)),
            ),
            concrete::Term::Import(_, _, ref _name) => unimplemented!("imports"),
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(env, params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(env, params, None, body),
            concrete::Term::Arrow(ref ann, ref body) => raw::RcTerm::from(raw::Term::Pi(
                span,
                Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(ann.desugar(env))),
                    body.desugar(env),
                ),
            )),
            concrete::Term::App(ref head, ref args) => {
                args.iter().fold(head.desugar(env), |acc, arg| {
                    raw::RcTerm::from(raw::Term::App(acc, arg.desugar(env)))
                })
            },
            concrete::Term::Let(_, ref items, ref body) => desugar_let(env, items, body),
            concrete::Term::If(start, ref cond, ref if_true, ref if_false) => {
                raw::RcTerm::from(raw::Term::If(
                    start,
                    cond.desugar(env),
                    if_true.desugar(env),
                    if_false.desugar(env),
                ))
            },
            concrete::Term::Case(span, ref head, ref clauses) => {
                raw::RcTerm::from(raw::Term::Case(
                    span,
                    head.desugar(env),
                    clauses
                        .iter()
                        .map(|(pattern, term)| {
                            let (pattern, env) = pattern.desugar(env);
                            Scope::new(pattern, term.desugar(&env))
                        }).collect(),
                ))
            },
            concrete::Term::RecordType(span, ref fields) => desugar_record_ty(env, span, fields),
            concrete::Term::Record(span, ref fields) => desugar_record(env, span, fields),
            concrete::Term::Proj(ref tm, label_start, ref label) => {
                raw::RcTerm::from(raw::Term::Proj(
                    span,
                    tm.desugar(env),
                    ByteSpan::from_offset(label_start, ByteOffset::from_str(label)),
                    Label(label.clone()),
                ))
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}
