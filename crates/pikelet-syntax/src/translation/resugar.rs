use codespan::{ByteIndex, ByteSpan};
use im;
use moniker::{Binder, BoundTerm, Embed, FreeVar, Nest, Scope, Var};

use concrete;
use core;
use {Label, Level, LevelShift};

/// The environment used when resugaring from the core to the concrete syntax
#[derive(Debug, Clone)]
pub struct ResugarEnv {
    usages: im::HashMap<String, u32>,
    renames: im::HashMap<FreeVar<String>, String>,
}

const KEYWORDS: &[&str] = &[
    "as", "case", "else", "if", "import", "in", "let", "record", "Record", "then", "Type",
];

impl ResugarEnv {
    pub fn new() -> ResugarEnv {
        ResugarEnv {
            usages: KEYWORDS.iter().map(|&kw| (kw.to_owned(), 0)).collect(),
            renames: im::HashMap::new(),
        }
    }

    pub fn on_item(&mut self, label: &Label, binder: &Binder<String>) -> String {
        let Label(ref name) = *label;
        let Binder(ref free_var) = *binder;

        self.renames.get(free_var).cloned().unwrap_or_else(|| {
            match self.usages.get(name).cloned() {
                Some(count) => {
                    let count = count + 1;
                    let mapped_name = format!("{}{}", name, count);

                    self.usages.insert(name.clone(), count);
                    self.usages.insert(mapped_name.clone(), count);
                    self.renames.insert(free_var.clone(), mapped_name.clone());

                    mapped_name
                },
                None => {
                    self.usages.insert(name.clone(), 0);
                    self.renames.insert(free_var.clone(), name.clone());

                    name.clone()
                },
            }
        })
    }

    // pub fn on_binder(&mut self, binder: &Binder<String>, free_vars: &HashSet<String>) -> String {
    pub fn on_binder(&mut self, binder: &Binder<String>) -> String {
        let Binder(ref free_var) = *binder;

        self.renames.get(free_var).cloned().unwrap_or_else(|| {
            let pretty_name = match free_var.pretty_name {
                Some(ref name) => name.clone(),
                None => "a".to_owned(),
            };

            match self.usages.get(&pretty_name).cloned() {
                Some(count) => {
                    let count = count + 1;
                    let mapped_name = format!("{}{}", pretty_name, count);

                    self.usages.insert(pretty_name, count);
                    self.usages.insert(mapped_name.clone(), count);
                    self.renames.insert(free_var.clone(), mapped_name.clone());

                    mapped_name
                },
                None => {
                    self.usages.insert(pretty_name.clone(), 0);
                    self.renames.insert(free_var.clone(), pretty_name.clone());

                    pretty_name
                },
            }
        })
    }

    pub fn on_free_var(&self, free_var: &FreeVar<String>) -> String {
        self.renames.get(free_var).cloned().unwrap_or_else(|| {
            panic!(
                "on_free_var: expected {} to be bound in resugar environment",
                free_var,
            );
        })
    }
}

/// Translate something to the corresponding concrete representation
pub trait Resugar<T> {
    fn resugar(&self, env: &ResugarEnv) -> T;
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

fn resugar_pattern(
    env: &mut ResugarEnv,
    pattern: &core::Pattern,
    _prec: Prec,
) -> concrete::Pattern {
    match *pattern {
        core::Pattern::Ann(ref pattern, Embed(ref ty)) => concrete::Pattern::Ann(
            Box::new(resugar_pattern(env, pattern, Prec::NO_WRAP)),
            Box::new(resugar_term(env, ty, Prec::LAM)),
        ),
        core::Pattern::Binder(ref binder) => {
            let name = env.on_binder(binder);
            concrete::Pattern::Name(ByteSpan::default(), name, None)
        },
        core::Pattern::Var(Embed(Var::Free(ref free_var)), shift) => {
            let shift = match shift {
                LevelShift(0) => None,
                LevelShift(shift) => Some(shift),
            };

            let name = env.on_free_var(free_var);
            concrete::Pattern::Name(ByteSpan::default(), name, shift)
        },
        core::Pattern::Var(Embed(Var::Bound(_)), _) => {
            // TODO: Better message
            panic!("Tried to convert a term that was not locally closed");
        },
        core::Pattern::Literal(ref literal) => {
            use concrete::Literal::*;
            use concrete::Pattern;
            use concrete::Pattern::Literal;

            let span = ByteSpan::default();

            match *literal {
                // FIXME: Draw these names from some environment?
                core::Literal::Bool(true) => Pattern::Name(span, "true".to_owned(), None),
                core::Literal::Bool(false) => Pattern::Name(span, "false".to_owned(), None),

                core::Literal::String(ref val) => Literal(String(span, val.clone())),
                core::Literal::Char(val) => Literal(Char(span, val)),

                core::Literal::U8(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U16(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U32(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U64(val, format) => Literal(Int(span, val, format)),

                // FIXME: Underflow for negative numbers
                core::Literal::S8(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S16(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S32(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S64(val, format) => Literal(Int(span, val as u64, format)),

                core::Literal::F32(val, format) => Literal(Float(span, f64::from(val), format)),
                core::Literal::F64(val, format) => Literal(Float(span, val, format)),
            }
        },
    }
}

fn resugar_fun_ty(
    env: &ResugarEnv,
    scope: &Scope<(Binder<String>, Embed<core::RcTerm>), core::RcTerm>,
    prec: Prec,
) -> concrete::Term {
    let mut env = env.clone();

    let ((binder, Embed(mut ann)), mut body) = scope.clone().unbind();
    let body_fvs = body.free_vars();

    // Only use explicit parameter names if the body is dependent on
    // the parameter or there is a human-readable name given.
    //
    // We'll be checking for readable names as we go, because if they've
    // survived until now they're probably desirable to retain!
    if body_fvs.contains(&binder.0) || binder.0.pretty_name.is_some() {
        let name = env.on_binder(&binder);
        let mut params = vec![(
            vec![(ByteIndex::default(), name)],
            resugar_term(&env, &ann, Prec::APP),
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
            let ((next_binder, Embed(next_ann)), next_body) = match *body {
                core::Term::FunType(ref scope) => scope.clone().unbind(),
                _ => break,
            };

            if core::Term::term_eq(&ann, &next_ann) && next_binder.0.pretty_name.is_some() {
                // Combine the parameters if the type annotations are
                // alpha-equivalent. For example:
                //
                // ```
                // (a : Type) (b : Type) -> ...
                // (a b : Type) -> ...
                // ```
                let next_name = env.on_binder(&next_binder);
                let next_param = (ByteIndex::default(), next_name);
                params.last_mut().unwrap().0.push(next_param);
            } else if next_body.free_vars().contains(&next_binder.0)
                || next_binder.0.pretty_name.is_some()
            {
                // Add a new parameter if the body is dependent on the parameter
                // or there is a human-readable name given
                let next_name = env.on_binder(&next_binder);
                params.push((
                    vec![(ByteIndex::default(), next_name)],
                    resugar_term(&env, &next_ann, Prec::APP),
                ));
            } else {
                // Stop collapsing parameters if we encounter a non-dependent pi type.
                return parens_if(
                    Prec::PI < prec,
                    concrete::Term::FunType(
                        ByteIndex::default(),
                        params,
                        Box::new(concrete::Term::FunArrow(
                            Box::new(resugar_term(&env, &next_ann, Prec::APP)),
                            Box::new(resugar_term(&env, &next_body, Prec::LAM)),
                        )),
                    ),
                );
            }

            ann = next_ann;
            body = next_body;
        }

        parens_if(
            Prec::PI < prec,
            concrete::Term::FunType(
                ByteIndex::default(),
                params,
                Box::new(resugar_term(&env, &body, Prec::LAM)),
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
            concrete::Term::FunArrow(
                Box::new(resugar_term(&env, &ann, Prec::APP)),
                Box::new(resugar_term(&env, &body, Prec::LAM)),
            ),
        )
    }
}

fn resugar_fun_intro(
    env: &ResugarEnv,
    scope: &Scope<(Binder<String>, Embed<core::RcTerm>), core::RcTerm>,
    prec: Prec,
) -> concrete::Term {
    let mut env = env.clone();

    let ((binder, Embed(mut ann)), mut body) = scope.clone().unbind();

    let name = env.on_binder(&binder);
    let mut params = vec![(
        vec![(ByteIndex::default(), name)],
        Some(Box::new(resugar_term(&env, &ann, Prec::LAM))),
    )];

    // Argument resugaring
    #[cfg_attr(feature = "cargo-clippy", allow(while_let_loop))]
    loop {
        // Share a parameter list if another lambda is nested directly inside.
        // For example:
        //
        // ```
        // \(a : Type) => \(b : Type -> Type) => ...
        // \(a : Type) (b : Type -> Type) => ...
        // ```
        let ((next_binder, Embed(next_ann)), next_body) = match *body {
            core::Term::FunIntro(ref scope) => scope.clone().unbind(),
            _ => break,
        };

        // Combine the parameters if the type annotations are alpha-equivalent.
        // For example:
        //
        // ```
        // \(a : Type) (b : Type) => ...
        // \(a b : Type) => ...
        // ```
        let next_name = env.on_binder(&next_binder);
        if core::Term::term_eq(&ann, &next_ann) {
            let next_param = (ByteIndex::default(), next_name);
            params.last_mut().unwrap().0.push(next_param);
        } else {
            params.push((
                vec![(ByteIndex::default(), next_name)],
                Some(Box::new(resugar_term(&env, &next_ann, Prec::LAM))),
            ));
        }

        ann = next_ann;
        body = next_body;
    }

    parens_if(
        Prec::LAM < prec,
        concrete::Term::FunIntro(
            ByteIndex::default(),
            params,
            Box::new(resugar_term(&env, &body, Prec::LAM)),
        ),
    )
}

fn resugar_let(
    env: &ResugarEnv,
    scope: &Scope<Nest<(Binder<String>, Embed<(core::RcTerm, core::RcTerm)>)>, core::RcTerm>,
    prec: Prec,
) -> concrete::Term {
    let mut env = env.clone();

    let (bindings, mut body) = scope.clone().unbind();
    let bindings = bindings.unnest();

    let mut items = Vec::with_capacity(bindings.len() * 2);

    for (binder, Embed((ann, term))) in bindings {
        let name = env.on_binder(&binder);
        // pull lambda arguments from the body into the definition
        let (term_params, term_body) = match resugar_term(&env, &term, Prec::NO_WRAP) {
            concrete::Term::FunIntro(_, params, term_body) => (params, *term_body),
            term_body => (vec![], term_body),
        };

        items.push(concrete::Item::Declaration {
            name: (ByteIndex::default(), name.clone()),
            ann: resugar_term(&env, &ann, Prec::ANN),
        });
        items.push(concrete::Item::Definition {
            name: (ByteIndex::default(), name),
            params: term_params,
            return_ann: None,
            body: term_body,
        });
    }

    #[cfg_attr(feature = "cargo-clippy", allow(while_let_loop))]
    loop {
        let (bindings, next_body) = match *body {
            core::Term::Let(ref scope) => scope.clone().unbind(),
            _ => break,
        };

        for (binder, Embed((ann, term))) in bindings.unnest() {
            let next_name = env.on_binder(&binder);
            // pull lambda arguments from the body into the definition
            let (term_params, term_body) = match resugar_term(&env, &term, Prec::NO_WRAP) {
                concrete::Term::FunIntro(_, params, term_body) => (params, *term_body),
                term_body => (vec![], term_body),
            };

            items.push(concrete::Item::Declaration {
                name: (ByteIndex::default(), next_name.clone()),
                ann: resugar_term(&env, &ann, Prec::ANN),
            });
            items.push(concrete::Item::Definition {
                name: (ByteIndex::default(), next_name),
                params: term_params,
                return_ann: None,
                body: term_body,
            });
        }

        body = next_body;
    }

    parens_if(
        Prec::LAM < prec,
        concrete::Term::Let(
            ByteIndex::default(),
            items,
            Box::new(resugar_term(&env, &body, Prec::NO_WRAP)),
        ),
    )
}

fn resugar_term(env: &ResugarEnv, term: &core::Term, prec: Prec) -> concrete::Term {
    match *term {
        core::Term::Ann(ref term, ref ty) => parens_if(
            Prec::ANN < prec,
            concrete::Term::Ann(
                Box::new(resugar_term(env, term, Prec::LAM)),
                Box::new(resugar_term(env, ty, Prec::ANN)),
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
        core::Term::Literal(ref literal) => {
            use concrete::Literal::*;
            use concrete::Term;
            use concrete::Term::Literal;

            let span = ByteSpan::default();

            match *literal {
                // FIXME: Draw these names from some environment?
                core::Literal::Bool(true) => Term::Name(span, "true".to_owned(), None),
                core::Literal::Bool(false) => Term::Name(span, "false".to_owned(), None),

                core::Literal::String(ref val) => Literal(String(span, val.clone())),
                core::Literal::Char(val) => Literal(Char(span, val)),

                core::Literal::U8(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U16(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U32(val, format) => Literal(Int(span, u64::from(val), format)),
                core::Literal::U64(val, format) => Literal(Int(span, val, format)),

                // FIXME: Underflow for negative numbers
                core::Literal::S8(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S16(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S32(val, format) => Literal(Int(span, val as u64, format)),
                core::Literal::S64(val, format) => Literal(Int(span, val as u64, format)),

                core::Literal::F32(val, format) => Literal(Float(span, f64::from(val), format)),
                core::Literal::F64(val, format) => Literal(Float(span, val, format)),
            }
        },
        core::Term::Var(Var::Free(ref free_var), shift) => {
            let shift = match shift {
                LevelShift(0) => None,
                LevelShift(shift) => Some(shift),
            };

            let name = env.on_free_var(free_var);
            concrete::Term::Name(ByteSpan::default(), name, shift)
        },
        core::Term::Var(Var::Bound(_), _) => {
            // TODO: Better message
            panic!("Tried to convert a term that was not locally closed");
        },
        core::Term::Import(ref name) => parens_if(
            Prec::LAM < prec,
            concrete::Term::Import(ByteSpan::default(), ByteSpan::default(), name.clone()),
        ),
        core::Term::FunType(ref scope) => resugar_fun_ty(env, scope, prec),
        core::Term::FunIntro(ref scope) => resugar_fun_intro(env, scope, prec),
        core::Term::FunApp(ref head, ref arg) => parens_if(
            Prec::APP < prec,
            concrete::Term::FunApp(
                Box::new(resugar_term(env, head, Prec::NO_WRAP)),
                vec![resugar_term(env, arg, Prec::NO_WRAP)], // TODO
            ),
        ),
        core::Term::Let(ref scope) => resugar_let(env, scope, prec),
        core::Term::RecordType(ref scope) => {
            let mut env = env.clone();
            let (scope, ()) = scope.clone().unbind();

            let fields = scope
                .unnest()
                .into_iter()
                .map(|(label, binder, Embed(ann))| {
                    let ann = resugar_term(&env, &ann, Prec::NO_WRAP);
                    let name = env.on_item(&label, &binder);

                    concrete::RecordTypeField {
                        label: (ByteIndex::default(), label.0.clone()),
                        binder: match binder.0.pretty_name {
                            Some(ref pretty_name) if *pretty_name == name => None,
                            None | Some(_) => Some((ByteIndex::default(), name)),
                        },
                        ann,
                    }
                })
                .collect();

            concrete::Term::RecordType(ByteSpan::default(), fields)
        },
        core::Term::RecordIntro(ref scope) => {
            let mut env = env.clone();
            let (scope, ()) = scope.clone().unbind();

            let fields = scope
                .unnest()
                .into_iter()
                .map(|(label, binder, Embed(term))| {
                    let (term_params, term_body) = match resugar_term(&env, &term, Prec::NO_WRAP) {
                        concrete::Term::FunIntro(_, params, term_body) => (params, *term_body),
                        term_body => (vec![], term_body),
                    };
                    let name = env.on_item(&label, &binder);

                    // TODO: use a punned label if possible?
                    concrete::RecordField::Explicit {
                        label: (ByteIndex::default(), name),
                        params: term_params,
                        return_ann: None,
                        term: term_body,
                    }
                })
                .collect();

            // TODO: Add let to rename shadowed globals?
            concrete::Term::RecordIntro(ByteSpan::default(), fields)
        },
        core::Term::RecordProj(ref expr, Label(ref label), shift) => {
            let shift = match shift {
                LevelShift(0) => None,
                LevelShift(shift) => Some(shift),
            };

            concrete::Term::RecordProj(
                ByteSpan::default(),
                Box::new(resugar_term(env, expr, Prec::ATOMIC)),
                ByteIndex::default(),
                label.clone(),
                shift,
            )
        },
        // TODO: Resugar boolean patterns into if-then-else expressions?
        core::Term::Case(ref head, ref clauses) => concrete::Term::Case(
            ByteSpan::default(),
            Box::new(resugar_term(env, head, Prec::NO_WRAP)),
            clauses
                .iter()
                .map(|scope| {
                    let (pattern, term) = scope.clone().unbind();
                    let mut env = env.clone();
                    (
                        resugar_pattern(&mut env, &pattern, Prec::NO_WRAP),
                        resugar_term(&env, &term, Prec::NO_WRAP),
                    )
                })
                .collect(),
        ),
        core::Term::ArrayIntro(ref elems) => concrete::Term::ArrayIntro(
            ByteSpan::default(),
            elems
                .iter()
                .map(|elem| resugar_term(env, elem, Prec::NO_WRAP))
                .collect(),
        ),
    }
}

impl Resugar<concrete::Term> for core::Term {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        resugar_term(env, self, Prec::NO_WRAP)
    }
}

impl Resugar<concrete::Term> for core::Value {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        // FIXME: Make this more efficient?
        resugar_term(env, &core::Term::from(self), Prec::NO_WRAP)
    }
}

impl Resugar<concrete::Term> for core::Neutral {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        // FIXME: Make this more efficient?
        resugar_term(env, &core::Term::from(self), Prec::NO_WRAP)
    }
}

impl Resugar<concrete::Term> for core::RcTerm {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        self.inner.resugar(env)
    }
}

impl Resugar<concrete::Term> for core::RcValue {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        self.inner.resugar(env)
    }
}

impl Resugar<concrete::Term> for core::RcNeutral {
    fn resugar(&self, env: &ResugarEnv) -> concrete::Term {
        self.inner.resugar(env)
    }
}
