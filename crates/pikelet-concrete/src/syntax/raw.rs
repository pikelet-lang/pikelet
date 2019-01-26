//! The syntax of the language, unchecked and with implicit parts that need to
//! be elaborated in a type-directed way during type checking and inference

use codespan::ByteSpan;
use moniker::{Binder, BoundPattern, BoundTerm, Embed, Nest, Scope, Var};
use pretty::{BoxDoc, Doc};
use std::fmt;
use std::ops;
use std::rc::Rc;

use pikelet_core::syntax::{Label, Level, LevelShift};

use crate::syntax::{FloatFormat, IntFormat, PRETTY_FALLBACK_WIDTH};

/// Literals
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm, BoundPattern)]
pub enum Literal {
    String(ByteSpan, String),
    Char(ByteSpan, char),
    Int(ByteSpan, u64, IntFormat),
    Float(ByteSpan, f64, FloatFormat),
}

impl Literal {
    /// Return the span of source code that the literal originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Literal::String(span, ..)
            | Literal::Char(span, ..)
            | Literal::Int(span, ..)
            | Literal::Float(span, ..) => span,
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Literal::String(_, ref value) => Doc::text(format!("{:?}", value)),
            Literal::Char(_, value) => Doc::text(format!("{:?}", value)),
            Literal::Int(_, value, IntFormat::Bin) => Doc::text(format!("0b{:b}", value)),
            Literal::Int(_, value, IntFormat::Oct) => Doc::text(format!("0o{:o}", value)),
            Literal::Int(_, value, IntFormat::Dec) => Doc::text(format!("{}", value)),
            Literal::Int(_, value, IntFormat::Hex) => Doc::text(format!("0x{:x}", value)),
            Literal::Float(_, value, FloatFormat::Dec) => Doc::text(format!("{}", value)),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub enum Pattern {
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcTerm>),
    /// Patterns that bind variables
    Binder(ByteSpan, Binder<String>),
    /// Patterns to be compared structurally with a variable in scope
    Var(ByteSpan, Embed<Var<String>>, LevelShift),
    /// Literal patterns
    Literal(Literal),
}

impl Pattern {
    /// Return the span of source code that this pattern originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => pattern.span().to(ty.span()),
            Pattern::Var(span, _, _) | Pattern::Binder(span, _) => span,
            Pattern::Literal(ref literal) => literal.span(),
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => Doc::nil()
                .append(pattern.to_doc())
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ty.to_doc_expr()),
            ref pattern => pattern.to_doc_atomic(),
        }
    }

    fn to_doc_atomic(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Pattern::Binder(_, ref binder) => Doc::as_string(binder),
            Pattern::Var(_, Embed(ref var), shift) => Doc::as_string(format!("{}^{}", var, shift)),
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

/// Terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(ByteSpan, Level),
    /// Literals
    Literal(Literal),
    /// A hole
    Hole(ByteSpan),
    /// A variable
    Var(ByteSpan, Var<String>, LevelShift),
    /// An imported definition
    Import(ByteSpan, ByteSpan, String),
    /// Dependent function types
    FunType(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Function introductions
    FunIntro(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Function application
    FunApp(RcTerm, RcTerm),
    /// Dependent record types
    RecordType(
        ByteSpan,
        Scope<Nest<(Label, Binder<String>, Embed<RcTerm>)>, ()>,
    ),
    /// Record introductions
    RecordIntro(ByteSpan, Vec<(Label, RcTerm)>),
    /// Record field projection
    RecordProj(ByteSpan, RcTerm, ByteSpan, Label, LevelShift),
    /// Case expressions
    Case(ByteSpan, RcTerm, Vec<Scope<RcPattern, RcTerm>>),
    /// Array literals
    ArrayIntro(ByteSpan, Vec<RcTerm>),
    /// Let bindings
    Let(
        ByteSpan,
        Scope<Nest<(Binder<String>, Embed<RcTerm>)>, RcTerm>,
    ),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Universe(span, ..)
            | Term::Hole(span)
            | Term::Var(span, ..)
            | Term::Import(span, ..)
            | Term::FunType(span, ..)
            | Term::FunIntro(span, ..)
            | Term::RecordType(span, ..)
            | Term::RecordIntro(span, ..)
            | Term::RecordProj(span, ..)
            | Term::Case(span, ..)
            | Term::ArrayIntro(span, ..)
            | Term::Let(span, ..) => span,
            Term::Literal(ref literal) => literal.span(),
            Term::Ann(ref expr, ref ty) => expr.span().to(ty.span()),
            Term::FunApp(ref head, ref arg) => head.span().to(arg.span()),
        }
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
            Term::Import(_, _, ref name) => Doc::nil()
                .append("import")
                .append(Doc::space())
                .append(format!("{:?}", name)),
            Term::FunType(_, ref scope) => Doc::nil()
                .append("Fun")
                .append(Doc::space())
                .append("(")
                .append(Doc::as_string(&scope.unsafe_pattern.0))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append((scope.unsafe_pattern.1).0.to_doc_expr())
                .append(")")
                .append(Doc::space())
                .append("->")
                .append(Doc::space())
                .append(scope.unsafe_body.to_doc_expr()),
            Term::FunIntro(_, ref scope) => Doc::nil()
                .append("fun")
                .append(Doc::space())
                .append("(")
                .append(Doc::as_string(&scope.unsafe_pattern.0))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append((scope.unsafe_pattern.1).0.to_doc_expr())
                .append(")")
                .append(Doc::space())
                .append("=>")
                .append(Doc::space())
                .append(scope.unsafe_body.to_doc_expr()),
            Term::Case(_, ref head, ref clauses) => Doc::nil()
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
            Term::Let(_, ref scope) => Doc::nil()
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
            Term::Universe(_, level) => Doc::text(format!("Type^{}", level)),
            Term::ArrayIntro(_, ref elems) => Doc::nil()
                .append("[")
                .append(Doc::intersperse(
                    elems.iter().map(|elem| elem.to_doc()),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            Term::Var(_, ref var, ref level) => Doc::text(format!("{}^{}", var, level)),
            Term::Hole(_) => Doc::text("_"),
            Term::RecordType(_, ref scope) => Doc::nil()
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
            Term::RecordIntro(_, ref fields) => Doc::nil()
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
            Term::RecordProj(_, ref expr, _, ref label, ref shift) => Doc::nil()
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
