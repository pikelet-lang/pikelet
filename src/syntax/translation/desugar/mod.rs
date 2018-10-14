use codespan::{ByteIndex, ByteOffset, ByteSpan};
use codespan_reporting::{Diagnostic, Label as DiagnosticLabel};
use im;
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
    locals: im::HashMap<String, FreeVar<String>>,
}

impl DesugarEnv {
    pub fn new(mappings: im::HashMap<String, FreeVar<String>>) -> DesugarEnv {
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

/// An error produced during resugaring
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum DesugarError {
    #[fail(
        display = "Name had more than one declaration associated with it: `{}`",
        name
    )]
    DuplicateDeclarations {
        original_span: ByteSpan,
        duplicate_span: ByteSpan,
        name: String,
    },
    #[fail(display = "Declaration followed definition: `{}`", name)]
    DeclarationFollowedDefinition {
        definition_span: ByteSpan,
        declaration_span: ByteSpan,
        name: String,
    },
    #[fail(
        display = "Name had more than one definition associated with it: `{}`",
        name
    )]
    DuplicateDefinitions {
        original_span: ByteSpan,
        duplicate_span: ByteSpan,
        name: String,
    },
}

impl DesugarError {
    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            DesugarError::DuplicateDeclarations {
                original_span,
                duplicate_span,
                ref name,
            } => Diagnostic::new_error(format!(
                "name had more than one declaration associated with it `{}`",
                name,
            ))
            .with_label(
                DiagnosticLabel::new_primary(duplicate_span)
                    .with_message("the duplicated declaration"),
            )
            .with_label(
                DiagnosticLabel::new_secondary(original_span)
                    .with_message("the original declaration"),
            ),
            DesugarError::DeclarationFollowedDefinition {
                definition_span,
                declaration_span,
                name: _,
            } => Diagnostic::new_error(format!("declarations cannot follow definitions"))
                .with_label(
                    DiagnosticLabel::new_primary(declaration_span).with_message("the declaration"),
                )
                .with_label(
                    DiagnosticLabel::new_secondary(definition_span)
                        .with_message("the original definition"),
                ),
            DesugarError::DuplicateDefinitions {
                original_span,
                duplicate_span,
                ref name,
            } => Diagnostic::new_error(format!(
                "name had more than one definition associated with it `{}`",
                name,
            ))
            .with_label(
                DiagnosticLabel::new_primary(duplicate_span)
                    .with_message("the duplicated definition"),
            )
            .with_label(
                DiagnosticLabel::new_secondary(original_span)
                    .with_message("the original definition"),
            ),
        }
    }
}

/// Translate something to the corresponding core representation
pub trait Desugar<T> {
    fn desugar(&self, env: &DesugarEnv) -> Result<T, DesugarError>;
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
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = raw::RcTerm::from(ann.desugar(&env)?);
        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    Ok(params
        .into_iter()
        .rev()
        .fold(body.desugar(&env)?, |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Pi(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        }))
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
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();

    let mut params = Vec::new();
    for &(ref names, ref ann) in param_groups {
        let ann = match *ann {
            None => raw::RcTerm::from(raw::Term::Hole(ByteSpan::default())),
            Some(ref ann) => ann.desugar(&env)?,
        };

        params.extend(names.iter().map(|&(start, ref name)| {
            let free_var = env.on_binding(name);
            (start, Binder(free_var), ann.clone())
        }));
    }

    let body = match return_ann {
        None => body.desugar(&env)?,
        Some(ann) => raw::RcTerm::from(raw::Term::Ann(body.desugar(&env)?, ann.desugar(&env)?)),
    };

    Ok(params
        .into_iter()
        .rev()
        .fold(body, |acc, (start, binder, ann)| {
            raw::RcTerm::from(raw::Term::Lam(
                ByteSpan::new(start, acc.span().end()),
                Scope::new((binder, Embed(ann.clone())), acc),
            ))
        }))
}

fn desugar_items(
    env: &mut DesugarEnv,
    concrete_items: &[concrete::Item],
) -> Result<Nest<(Binder<String>, Embed<(raw::RcTerm, raw::RcTerm)>)>, DesugarError> {
    use im::HashMap;

    #[derive(Clone)]
    pub enum ForwardDecl {
        Pending(ByteSpan, raw::RcTerm),
        Defined(ByteSpan),
    }

    // Declarations that may be waiting to be defined
    let mut forward_declarations = HashMap::new();
    // The elaborated items, pre-allocated to improve performance
    let mut items = Vec::with_capacity(concrete_items.len());
    let hole = raw::RcTerm::from(raw::Term::Hole(ByteSpan::default()));

    // Iterate through the items in the module, checking each in turn
    for concrete_item in concrete_items {
        match *concrete_item {
            concrete::Item::Declaration {
                name: (start, ref name),
                ref ann,
            } => {
                let binder = env.on_item(name);
                let name_span = ByteSpan::from_offset(start, ByteOffset::from_str(name));

                // Ensure that this declaration has not already been seen
                match forward_declarations.get(&binder) {
                    // There's already a definition associated with this name -
                    // we can't add a new declaration for it!
                    Some(&ForwardDecl::Defined(definition_span)) => {
                        return Err(DesugarError::DeclarationFollowedDefinition {
                            definition_span,
                            declaration_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // There's a declaration  for this name already pending - we
                    // can't add a new one!
                    Some(&ForwardDecl::Pending(original_span, _)) => {
                        return Err(DesugarError::DuplicateDeclarations {
                            original_span,
                            duplicate_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // No previous declaration for this name was seen, so we can
                    // go-ahead and type check, elaborate, and then add it to
                    // the context
                    None => {},
                }

                // Remember the declaration for when we get to a subsequent definition
                let declaration = ForwardDecl::Pending(name_span, ann.desugar(&env)?);
                forward_declarations.insert(binder.clone(), declaration);
            },

            concrete::Item::Definition {
                name: (start, ref name),
                ref params,
                ref return_ann,
                ref body,
            } => {
                let binder = env.on_item(name);
                let name_span = ByteSpan::from_offset(start, ByteOffset::from_str(name));
                let term = desugar_lam(env, params, return_ann.as_ref().map(<_>::as_ref), body)?;
                let ann = match forward_declarations.get(&binder).cloned() {
                    // This declaration was already given a definition, so this
                    // is an error!
                    //
                    // NOTE: Some languages (eg. Haskell, Agda, Idris, and
                    // Erlang) turn duplicate definitions into case matches.
                    // Languages like Elm don't. What should we do here?
                    Some(ForwardDecl::Defined(original_span)) => {
                        return Err(DesugarError::DuplicateDefinitions {
                            original_span,
                            duplicate_span: name_span,
                            name: name.clone(),
                        });
                    },
                    // We found a prior declaration, so we'll use it as a basis
                    // for checking the definition
                    Some(ForwardDecl::Pending(_, ann)) => ann.clone(),
                    // No prior declaration was found, so use a hole instead
                    None => hole.clone(),
                };

                // We must not remove this from the list of pending
                // declarations, lest we encounter another declaration or
                // definition of the same name later on!
                forward_declarations.insert(binder.clone(), ForwardDecl::Defined(name_span));
                // Add the definition to the elaborated items
                items.push((binder, Embed((ann, term))));
            },
            concrete::Item::Error(_) => unimplemented!("error recovery"),
        }
    }

    Ok(Nest::new(items))
}

fn desugar_let(
    env: &DesugarEnv,
    start: ByteIndex,
    concrete_items: &[concrete::Item],
    body: &concrete::Term,
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();
    let items = desugar_items(&mut env, concrete_items)?;

    Ok(raw::RcTerm::from(raw::Term::Let(
        ByteSpan::new(start, body.span().end()),
        Scope::new(items, body.desugar(&env)?),
    )))
}

fn desugar_where(
    env: &DesugarEnv,
    body: &concrete::Term,
    concrete_items: &[concrete::Item],
    end: ByteIndex,
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();
    let items = desugar_items(&mut env, concrete_items)?;

    // TODO: Remember formatting
    Ok(raw::RcTerm::from(raw::Term::Let(
        ByteSpan::new(body.span().start(), end),
        Scope::new(items, body.desugar(&env)?),
    )))
}

fn desugar_record_ty(
    env: &DesugarEnv,
    span: ByteSpan,
    fields: &[concrete::RecordTypeField],
) -> Result<raw::RcTerm, DesugarError> {
    let mut env = env.clone();

    let fields = fields
        .iter()
        .map(|field| {
            let (_, ref label) = field.label;
            let ann = field.ann.desugar(&env)?;
            let free_var = match field.binder {
                Some((_, ref binder)) => env.on_binding(binder),
                None => env.on_binding(label),
            };

            Ok((Label(label.clone()), Binder(free_var), Embed(ann)))
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(raw::RcTerm::from(raw::Term::RecordType(
        span,
        Scope::new(Nest::new(fields), ()),
    )))
}

fn desugar_record(
    env: &DesugarEnv,
    span: ByteSpan,
    fields: &[concrete::RecordField],
) -> Result<raw::RcTerm, DesugarError> {
    use syntax::concrete::RecordField;

    let mut env = env.clone();

    let fields = fields
        .iter()
        .map(|field| match field {
            RecordField::Punned {
                label: (_, ref name),
                shift,
            } => {
                let var = env.on_name(span, name, shift.unwrap_or(0));
                let free_var = env.on_binding(name);
                Ok((Label(name.clone()), Binder(free_var), Embed(var)))
            },
            RecordField::Explicit {
                label: (_, ref name),
                ref params,
                ref return_ann,
                ref term,
            } => {
                let expr = desugar_lam(&env, params, return_ann.as_ref().map(<_>::as_ref), term)?;
                let free_var = env.on_binding(name);
                Ok((Label(name.clone()), Binder(free_var), Embed(expr)))
            },
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(raw::RcTerm::from(raw::Term::Record(
        span,
        Scope::new(Nest::new(fields), ()),
    )))
}

impl Desugar<raw::Literal> for concrete::Literal {
    fn desugar(&self, _: &DesugarEnv) -> Result<raw::Literal, DesugarError> {
        Ok(match *self {
            concrete::Literal::String(span, ref val) => raw::Literal::String(span, val.clone()),
            concrete::Literal::Char(span, val) => raw::Literal::Char(span, val),
            concrete::Literal::Int(span, format, val) => raw::Literal::Int(span, format, val),
            concrete::Literal::Float(span, format, val) => raw::Literal::Float(span, format, val),
        })
    }
}

impl Desugar<(raw::RcPattern, DesugarEnv)> for concrete::Pattern {
    fn desugar(&self, env: &DesugarEnv) -> Result<(raw::RcPattern, DesugarEnv), DesugarError> {
        match *self {
            concrete::Pattern::Parens(_, ref pattern) => pattern.desugar(env),
            concrete::Pattern::Ann(ref pattern, ref ty) => {
                let ty = ty.desugar(env)?;
                let (pattern, env) = pattern.desugar(env)?;
                let ann_pattern = raw::RcPattern::from(raw::Pattern::Ann(pattern, Embed(ty)));

                Ok((ann_pattern, env))
            },
            concrete::Pattern::Name(span, ref name, shift) => match (env.locals.get(name), shift) {
                (Some(free_var), shift) => {
                    let var = Var::Free(free_var.clone());
                    let shift = LevelShift(shift.unwrap_or(0));
                    let pattern = raw::RcPattern::from(raw::Pattern::Var(span, Embed(var), shift));

                    Ok((pattern, env.clone()))
                },
                (None, Some(shift)) => {
                    let var = Var::Free(FreeVar::fresh_named(name.clone()));
                    let shift = LevelShift(shift);
                    let pattern = raw::RcPattern::from(raw::Pattern::Var(span, Embed(var), shift));

                    Ok((pattern, env.clone()))
                },
                (None, None) => {
                    let mut env = env.clone();
                    let free_var = env.on_binding(name);
                    let binder = Binder(free_var);
                    let pattern = raw::RcPattern::from(raw::Pattern::Binder(span, binder));

                    Ok((pattern, env))
                },
            },
            concrete::Pattern::Literal(ref literal) => {
                let literal = raw::RcPattern::from(raw::Pattern::Literal(literal.desugar(env)?));

                Ok((literal, env.clone()))
            },
            concrete::Pattern::Error(_) => unimplemented!("error recovery"),
        }
    }
}

impl Desugar<raw::RcTerm> for concrete::Term {
    fn desugar(&self, env: &DesugarEnv) -> Result<raw::RcTerm, DesugarError> {
        let span = self.span();
        match *self {
            concrete::Term::Parens(_, ref term) => term.desugar(env),
            concrete::Term::Ann(ref expr, ref ty) => Ok(raw::RcTerm::from(raw::Term::Ann(
                expr.desugar(env)?,
                ty.desugar(env)?,
            ))),
            concrete::Term::Universe(_, level) => Ok(raw::RcTerm::from(raw::Term::Universe(
                span,
                Level(level.unwrap_or(0)),
            ))),
            concrete::Term::Literal(ref literal) => {
                Ok(raw::RcTerm::from(raw::Term::Literal(literal.desugar(env)?)))
            },
            concrete::Term::Array(_, ref elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.desugar(env))
                    .collect::<Result<_, _>>()?;

                Ok(raw::RcTerm::from(raw::Term::Array(span, elems)))
            },
            concrete::Term::Hole(_) => Ok(raw::RcTerm::from(raw::Term::Hole(span))),
            concrete::Term::Name(_, ref name, shift) => {
                Ok(env.on_name(span, name, shift.unwrap_or(0)))
            },
            concrete::Term::Import(_, name_span, ref name) => Ok(raw::RcTerm::from(
                raw::Term::Import(span, name_span, name.clone()),
            )),
            concrete::Term::Pi(_, ref params, ref body) => desugar_pi(env, params, body),
            concrete::Term::Lam(_, ref params, ref body) => desugar_lam(env, params, None, body),
            concrete::Term::Arrow(ref ann, ref body) => Ok(raw::RcTerm::from(raw::Term::Pi(
                span,
                Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(ann.desugar(env)?)),
                    body.desugar(env)?,
                ),
            ))),
            concrete::Term::App(ref head, ref args) => {
                args.iter().fold(head.desugar(env), |acc, arg| {
                    Ok(raw::RcTerm::from(raw::Term::App(acc?, arg.desugar(env)?)))
                })
            },
            concrete::Term::Let(start, ref items, ref body) => desugar_let(env, start, items, body),
            concrete::Term::Where(ref expr, ref items, end) => desugar_where(env, expr, items, end),
            concrete::Term::If(_, ref cond, ref if_true, ref if_false) => {
                let bool_pattern = |name: &str| {
                    raw::RcPattern::from(raw::Pattern::Var(
                        ByteSpan::default(),
                        Embed(Var::Free(match env.locals.get(name) {
                            Some(free_var) => free_var.clone(),
                            None => FreeVar::fresh_named("oops"),
                        })),
                        LevelShift(0),
                    ))
                };

                Ok(raw::RcTerm::from(raw::Term::Case(
                    span,
                    cond.desugar(env)?,
                    vec![
                        Scope::new(bool_pattern("true"), if_true.desugar(&env)?),
                        Scope::new(bool_pattern("false"), if_false.desugar(&env)?),
                    ],
                )))
            },
            concrete::Term::Case(span, ref head, ref clauses) => {
                Ok(raw::RcTerm::from(raw::Term::Case(
                    span,
                    head.desugar(env)?,
                    clauses
                        .iter()
                        .map(|(pattern, term)| {
                            let (pattern, env) = pattern.desugar(env)?;
                            Ok(Scope::new(pattern, term.desugar(&env)?))
                        })
                        .collect::<Result<_, _>>()?,
                )))
            },
            concrete::Term::RecordType(span, ref fields) => desugar_record_ty(env, span, fields),
            concrete::Term::Record(span, ref fields) => desugar_record(env, span, fields),
            concrete::Term::Proj(_, ref tm, label_start, ref label, shift) => {
                Ok(raw::RcTerm::from(raw::Term::Proj(
                    span,
                    tm.desugar(env)?,
                    ByteSpan::from_offset(label_start, ByteOffset::from_str(label)),
                    Label(label.clone()),
                    LevelShift(shift.unwrap_or(0)),
                )))
            },
            concrete::Term::Error(_) => unimplemented!("error recovery"),
        }
    }
}
