//! The core syntax of the language

use moniker::{Binder, BoundPattern, BoundTerm, Embed, FreeVar, Nest, Scope, Var};
use pretty::{BoxDoc, Doc};
use std::fmt;
use std::ops;
use std::rc::Rc;

use crate::syntax::domain::{Head, Neutral, Value};
use crate::syntax::{Label, Level, LevelShift, Literal, PRETTY_FALLBACK_WIDTH};

#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub enum Pattern {
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcTerm>),
    /// Patterns that bind variables
    Binder(Binder<String>),
    /// Patterns to be compared structurally with a variable in scope
    Var(Embed<Var<String>>, LevelShift),
    /// Literal patterns
    Literal(Literal),
}

impl Pattern {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => Doc::nil()
                .append(pattern.to_doc())
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ty.to_doc()), // fun-intro?
            ref pattern => pattern.to_doc_atomic(),
        }
    }

    fn to_doc_atomic(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Pattern::Binder(ref binder) => Doc::as_string(binder),
            Pattern::Var(Embed(ref var), shift) => Doc::as_string(format!("{}^{}", var, shift)),
            Pattern::Literal(ref literal) => literal.to_doc(),
            ref pattern => Doc::text("(").append(pattern.to_doc()).append(")"),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

/// Reference counted patterns
#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub struct RcPattern {
    pub inner: Rc<Pattern>,
}

impl From<Pattern> for RcPattern {
    fn from(src: Pattern) -> RcPattern {
        RcPattern {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcPattern {
    type Target = Pattern;

    fn deref(&self) -> &Pattern {
        &self.inner
    }
}

impl fmt::Display for RcPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// The core term syntax
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(Level),
    /// Literals
    Literal(Literal),
    /// A variable
    Var(Var<String>, LevelShift),
    /// An imported definition
    Import(String),
    /// Dependent function types
    FunType(Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Function introductions
    FunIntro(Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Function applications
    FunApp(RcTerm, RcTerm),
    /// Dependent record types
    RecordType(Scope<Nest<(Label, Binder<String>, Embed<RcTerm>)>, ()>),
    /// Record introductions
    RecordIntro(Vec<(Label, RcTerm)>),
    /// Record field projection
    RecordProj(RcTerm, Label, LevelShift),
    /// Case expressions
    Case(RcTerm, Vec<Scope<RcPattern, RcTerm>>),
    /// Array literals
    ArrayIntro(Vec<RcTerm>),
    /// Let bindings
    Let(Scope<Nest<(Binder<String>, Embed<RcTerm>)>, RcTerm>),
}

impl Term {
    pub fn universe(level: impl Into<Level>) -> Term {
        Term::Universe(level.into())
    }

    pub fn var(var: impl Into<Var<String>>, shift: impl Into<LevelShift>) -> Term {
        Term::Var(var.into(), shift.into())
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::Ann(ref term, ref ty) => Doc::nil()
                .append(term.to_doc_expr())
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ty.to_doc_expr()),
            ref term => term.to_doc_expr(),
        }
    }

    fn to_doc_expr(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::Import(ref name) => Doc::nil()
                .append("import")
                .append(Doc::space())
                .append(format!("{:?}", name)),
            Term::FunIntro(ref scope) => Doc::nil()
                .append("\\")
                .append(Doc::as_string(&scope.unsafe_pattern.0))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append((scope.unsafe_pattern.1).0.to_doc_arrow())
                .append(Doc::space())
                .append("=>")
                .append(Doc::space())
                .append(scope.unsafe_body.to_doc_expr()),
            Term::Case(ref head, ref clauses) => Doc::nil()
                .append("case")
                .append(Doc::space())
                .append(head.to_doc_app())
                .append(Doc::space())
                .append("{")
                .append(Doc::space())
                .append(Doc::intersperse(
                    clauses.iter().map(|scope| {
                        Doc::nil()
                            .append(scope.unsafe_pattern.to_doc())
                            .append(Doc::space())
                            .append("=>")
                            .append(Doc::space())
                            .append(scope.unsafe_body.to_doc())
                            .append(";")
                    }),
                    Doc::newline(),
                ))
                .append(Doc::space())
                .append("}"),
            Term::Let(ref scope) => Doc::nil()
                .append("let")
                .append(Doc::space())
                .append(Doc::intersperse(
                    scope.unsafe_pattern.unsafe_patterns.iter().map(
                        |&(ref binder, Embed(ref term))| {
                            Doc::nil()
                                .append(Doc::as_string(binder))
                                .append(Doc::space())
                                .append("=")
                                .append(Doc::space())
                                .append(term.to_doc())
                        },
                    ),
                    Doc::newline(),
                ))
                .append(Doc::space())
                .append("in")
                .append(Doc::space())
                .append(scope.unsafe_body.to_doc_expr()),
            ref term => term.to_doc_arrow(),
        }
    }

    fn to_doc_arrow(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::FunType(ref scope) => Doc::nil()
                .append("(")
                .append(Doc::as_string(&scope.unsafe_pattern.0))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append((scope.unsafe_pattern.1).0.to_doc_arrow())
                .append(")")
                .append(Doc::space())
                .append("->")
                .append(Doc::space())
                .append(scope.unsafe_body.to_doc_expr()),
            ref term => term.to_doc_app(),
        }
    }

    fn to_doc_app(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::FunApp(ref fun, ref arg) => Doc::nil()
                .append(fun.to_doc_atomic())
                .append(Doc::space())
                .append(arg.to_doc_atomic()),
            ref term => term.to_doc_atomic(),
        }
    }

    fn to_doc_atomic(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::Universe(level) => Doc::text(format!("Type^{}", level)),
            Term::ArrayIntro(ref elems) => Doc::nil()
                .append("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            Term::Var(ref var, ref level) => Doc::text(format!("{}^{}", var, level)),
            Term::RecordType(ref scope) => Doc::nil()
                .append("Record {")
                .append(Doc::space())
                .append(Doc::intersperse(
                    scope.unsafe_pattern.unsafe_patterns.iter().map(
                        |&(ref label, ref binder, Embed(ref ann))| {
                            Doc::nil()
                                .append(Doc::as_string(label))
                                .append(Doc::space())
                                .append("as")
                                .append(Doc::space())
                                .append(Doc::as_string(binder))
                                .append(Doc::space())
                                .append(":")
                                .append(Doc::space())
                                .append(ann.to_doc())
                        },
                    ),
                    Doc::text(";").append(Doc::space()),
                ))
                .append(Doc::space())
                .append("}"),
            Term::RecordIntro(ref fields) => Doc::nil()
                .append("record {")
                .append(Doc::space())
                .append(Doc::intersperse(
                    fields.iter().map(|&(ref label, ref value)| {
                        Doc::nil()
                            .append(Doc::as_string(label))
                            .append(Doc::space())
                            .append("=")
                            .append(Doc::space())
                            .append(value.to_doc())
                    }),
                    Doc::text(";").append(Doc::space()),
                ))
                .append(Doc::space())
                .append("}"),
            Term::RecordProj(ref expr, ref label, ref shift) => Doc::nil()
                .append(expr.to_doc_atomic())
                .append(".")
                .append(format!("{}^{}", label, shift)),
            ref term => Doc::text("(").append(term.to_doc()).append(")"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

/// Reference counted terms
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcTerm {
    pub inner: Rc<Term>,
}

impl RcTerm {
    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        match *self.inner {
            Term::Ann(ref term, ref ty) => {
                RcTerm::from(Term::Ann(term.substs(mappings), ty.substs(mappings)))
            },
            Term::Universe(_) | Term::Literal(_) => self.clone(),
            Term::Var(ref var, _) => match mappings.iter().find(|&(ref name, _)| var == name) {
                Some(&(_, ref term)) => term.clone(),
                None => self.clone(),
            },
            Term::Import(ref name) => RcTerm::from(Term::Import(name.clone())),
            Term::FunType(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::FunType(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::FunIntro(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::FunIntro(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::Let(ref scope) => {
                let unsafe_patterns = scope
                    .unsafe_pattern
                    .unsafe_patterns
                    .iter()
                    .map(|&(ref binder, Embed(ref term))| {
                        (binder.clone(), Embed(term.substs(mappings)))
                    })
                    .collect();

                RcTerm::from(Term::Let(Scope {
                    unsafe_pattern: Nest { unsafe_patterns },
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::FunApp(ref head, ref arg) => {
                RcTerm::from(Term::FunApp(head.substs(mappings), arg.substs(mappings)))
            },
            Term::RecordType(ref scope) if scope.unsafe_pattern.unsafe_patterns.is_empty() => {
                self.clone()
            },
            Term::RecordType(ref scope) => {
                let unsafe_patterns = scope
                    .unsafe_pattern
                    .unsafe_patterns
                    .iter()
                    .map(|&(ref label, ref binder, Embed(ref ann))| {
                        (label.clone(), binder.clone(), Embed(ann.substs(mappings)))
                    })
                    .collect();

                RcTerm::from(Term::RecordType(Scope {
                    unsafe_pattern: Nest { unsafe_patterns },
                    unsafe_body: (),
                }))
            },
            Term::RecordIntro(ref fields) if fields.is_empty() => self.clone(),
            Term::RecordIntro(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref expr)| (label.clone(), expr.substs(mappings)))
                    .collect();

                RcTerm::from(Term::RecordIntro(fields))
            },
            Term::RecordProj(ref expr, ref label, shift) => RcTerm::from(Term::RecordProj(
                expr.substs(mappings),
                label.clone(),
                shift,
            )),
            Term::Case(ref head, ref clauses) => RcTerm::from(Term::Case(
                head.substs(mappings),
                clauses
                    .iter()
                    .map(|scope| {
                        Scope {
                            unsafe_pattern: scope.unsafe_pattern.clone(), // subst?
                            unsafe_body: scope.unsafe_body.substs(mappings),
                        }
                    })
                    .collect(),
            )),
            Term::ArrayIntro(ref elems) => RcTerm::from(Term::ArrayIntro(
                elems.iter().map(|elem| elem.substs(mappings)).collect(),
            )),
        }
    }
}

impl From<Term> for RcTerm {
    fn from(src: Term) -> RcTerm {
        RcTerm {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcTerm {
    type Target = Term;

    fn deref(&self) -> &Term {
        &self.inner
    }
}

impl fmt::Display for RcTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl<'a> From<&'a Value> for Term {
    fn from(src: &'a Value) -> Term {
        // Bypassing `Scope::new` and `Scope::unbind` here should be fine
        // because we aren't altering the structure of the scopes during this
        // transformation. This should save on some traversals of the AST!
        match *src {
            Value::Universe(level) => Term::Universe(level),
            Value::Literal(ref lit) => Term::Literal(lit.clone()),
            Value::FunType(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::FunType(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::FunIntro(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::FunIntro(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::RecordType(ref scope) => {
                let unsafe_patterns = scope
                    .unsafe_pattern
                    .unsafe_patterns
                    .iter()
                    .map(|&(ref label, ref binder, Embed(ref ann))| {
                        (label.clone(), binder.clone(), Embed(RcTerm::from(&**ann)))
                    })
                    .collect();

                Term::RecordType(Scope {
                    unsafe_pattern: Nest { unsafe_patterns },
                    unsafe_body: (),
                })
            },
            Value::RecordIntro(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref expr)| (label.clone(), RcTerm::from(&**expr)))
                    .collect();

                Term::RecordIntro(fields)
            },
            Value::ArrayIntro(ref elems) => {
                Term::ArrayIntro(elems.iter().map(|elem| RcTerm::from(&**elem)).collect())
            },
            Value::Neutral(ref neutral, ref spine) => {
                spine.iter().fold(Term::from(&*neutral.inner), |acc, arg| {
                    Term::FunApp(RcTerm::from(acc), RcTerm::from(&**arg))
                })
            },
        }
    }
}

impl<'a> From<&'a Value> for RcTerm {
    fn from(src: &'a Value) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        match *src {
            Neutral::Head(ref head) => Term::from(head),
            Neutral::RecordProj(ref expr, ref name, shift) => {
                Term::RecordProj(RcTerm::from(&**expr), name.clone(), shift)
            },
            Neutral::Case(ref head, ref clauses) => Term::Case(
                RcTerm::from(&**head),
                clauses
                    .iter()
                    .map(|clause| Scope {
                        unsafe_pattern: clause.unsafe_pattern.clone(),
                        unsafe_body: RcTerm::from(&*clause.unsafe_body),
                    })
                    .collect(),
            ),
        }
    }
}

impl<'a> From<&'a Neutral> for RcTerm {
    fn from(src: &'a Neutral) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Head> for Term {
    fn from(src: &'a Head) -> Term {
        match *src {
            Head::Var(ref var, shift) => Term::Var(var.clone(), shift),
            Head::Import(ref name) => Term::Import(name.clone()),
        }
    }
}
