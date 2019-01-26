//! The concrete syntax of the language

use codespan::{ByteIndex, ByteSpan};
use pretty::{BoxDoc, Doc};
use std::fmt;

use crate::syntax::{FloatFormat, IntFormat, PRETTY_FALLBACK_WIDTH, PRETTY_INDENT_WIDTH};

/// A group of lambda parameters that share an annotation
pub type FunIntroParamGroup = (Vec<(ByteIndex, String)>, Option<Box<Term>>);

/// The parameters to a lambda abstraction
pub type FunIntroParams = Vec<FunIntroParamGroup>;

/// A group of parameters to a dependent function that share an annotation
pub type FunTypeParamGroup = (Vec<(ByteIndex, String)>, Term);

/// The parameters to a dependent function type
pub type FunTypeParams = Vec<FunTypeParamGroup>;

#[derive(Debug, Clone, PartialEq)]
pub struct RecordTypeField {
    pub label: (ByteIndex, String),
    pub binder: Option<(ByteIndex, String)>,
    pub ann: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RecordIntroField {
    Punned {
        label: (ByteIndex, String),
        shift: Option<u32>,
    },
    Explicit {
        label: (ByteIndex, String),
        params: FunIntroParams,
        return_ann: Option<Box<Term>>,
        term: Term,
    },
}

/// Top-level items within a module
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Declares the type associated with a name, prior to its definition
    ///
    /// ```text
    /// foo : some-type
    /// ```
    Declaration {
        name: (ByteIndex, String),
        ann: Term,
    },
    /// Defines the term that should be associated with a name
    ///
    /// ```text
    /// foo = some-body
    /// foo x (y : some-type) = some-body
    /// ```
    Definition {
        name: (ByteIndex, String),
        params: FunIntroParams,
        return_ann: Option<Box<Term>>,
        body: Term,
    },
    /// Items that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Item {
    /// Return the span of source code that this declaration originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Item::Definition {
                name: (start, _),
                body: ref term,
                ..
            }
            | Item::Declaration {
                name: (start, _),
                ann: ref term,
            } => ByteSpan::new(start, term.span().end()),
            Item::Error(span) => span,
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Item::Declaration {
                name: (_, ref name),
                ref ann,
                ..
            } => Doc::as_string(name)
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ann.to_doc()),
            Item::Definition {
                name: (_, ref name),
                ref params,
                ref return_ann,
                ref body,
            } => Doc::as_string(name)
                .append(Doc::space())
                .append(match params[..] {
                    [] => Doc::nil(),
                    _ => pretty_fun_intro_params(params).append(Doc::space()),
                })
                .append(return_ann.as_ref().map_or(Doc::nil(), |return_ann| {
                    Doc::text(":")
                        .append(return_ann.to_doc())
                        .append(Doc::space())
                }))
                .append("=")
                .append(Doc::space())
                .append(body.to_doc().nest(PRETTY_INDENT_WIDTH)),
            Item::Error(_) => Doc::text("<error>"),
        }
        .append(";")
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

/// Literals
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// String literals
    // TODO: Preserve escapes?
    String(ByteSpan, String),
    /// Character literals
    // TODO: Preserve escapes?
    Char(ByteSpan, char),
    /// Integer literals
    // TODO: Preserve digit separators?
    Int(ByteSpan, u64, IntFormat),
    /// Floating point literals
    // TODO: Preserve digit separators?
    Float(ByteSpan, f64, FloatFormat),
}

impl Literal {
    /// Return the span of source code that the literal originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Literal::String(span, _)
            | Literal::Char(span, _)
            | Literal::Int(span, _, _)
            | Literal::Float(span, _, _) => span,
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

/// Patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (p)
    /// ```
    Parens(ByteSpan, Box<Pattern>),
    /// Patterns annotated with types
    ///
    /// ```text
    /// p : t
    /// ```
    Ann(Box<Pattern>, Box<Term>),
    /// Literal patterns
    Literal(Literal),
    /// Patterns that either introduce bound variables, or match by structural
    /// equality with a constant in-scope
    ///
    /// ```text
    /// x
    /// true
    /// false
    /// ```
    Name(ByteSpan, String, Option<u32>),
    /// Terms that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Pattern {
    /// Return the span of source code that this pattern originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Pattern::Parens(span, _) | Pattern::Name(span, _, _) | Pattern::Error(span) => span,
            Pattern::Ann(ref pattern, ref ty) => pattern.span().to(ty.span()),
            Pattern::Literal(ref literal) => literal.span(),
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Pattern::Parens(_, ref term) => Doc::text("(").append(term.to_doc()).append(")"),
            Pattern::Ann(ref term, ref ty) => term
                .to_doc()
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ty.to_doc()),
            Pattern::Name(_, ref name, None) => Doc::text(format!("{}", name)),
            Pattern::Name(_, ref name, Some(shift)) => Doc::text(format!("{}^{}", name, shift)),
            Pattern::Literal(ref literal) => literal.to_doc(),
            Pattern::Error(_) => Doc::text("<error>"),
        }
    }
}

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (e)
    /// ```
    Parens(ByteSpan, Box<Term>),
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(Box<Term>, Box<Term>),
    /// Type of types
    ///
    /// ```text
    /// Type
    /// ```
    Universe(ByteSpan, Option<u32>),
    /// Literals
    Literal(Literal),
    /// Array literals
    ArrayIntro(ByteSpan, Vec<Term>),
    /// Holes
    ///
    /// ```text
    /// _
    /// ```
    Hole(ByteSpan),
    /// Names
    ///
    /// ```text
    /// x
    /// x^1
    /// ```
    Name(ByteSpan, String, Option<u32>),
    /// An imported definition
    ///
    /// ```text
    /// import "prelude"
    /// ```
    Import(ByteSpan, ByteSpan, String),
    /// Dependent function type
    ///
    /// ```text
    /// (x : t1) -> t2
    /// (x y : t1) -> t2
    /// ```
    FunType(ByteIndex, FunTypeParams, Box<Term>),
    /// Non-Dependent function type
    ///
    /// ```text
    /// t1 -> t2
    /// ```
    FunArrow(Box<Term>, Box<Term>),
    /// Function introduction
    ///
    /// ```text
    /// \x => t
    /// \x y => t
    /// \x : t1 => t2
    /// \(x : t1) y (z : t2) => t3
    /// \(x y : t1) => t3
    /// ```
    FunIntro(ByteIndex, FunIntroParams, Box<Term>),
    /// Function application
    ///
    /// ```text
    /// e1 e2
    /// ```
    FunApp(Box<Term>, Vec<Term>),
    /// Let binding
    ///
    /// ```text
    /// let x : S32
    ///     x = 1
    /// in
    ///     x
    /// ```
    Let(ByteIndex, Vec<Item>, Box<Term>),
    /// Where expressions
    ///
    /// ```text
    /// id "hello"
    /// where {
    ///     id : (A : Type) -> A -> A;
    ///     id A x = x;
    /// }
    /// ```
    Where(Box<Term>, Vec<Item>, ByteIndex),
    /// If expression
    ///
    /// ```text
    /// if t1 then t2 else t3
    /// ```
    If(ByteIndex, Box<Term>, Box<Term>, Box<Term>),
    /// Case expression
    ///
    /// ```text
    /// case t1 { pat => t2; .. }
    /// ```
    Case(ByteSpan, Box<Term>, Vec<(Pattern, Term)>),
    /// Record type
    ///
    /// ```text
    /// Record { x : t1, .. }
    /// ```
    RecordType(ByteSpan, Vec<RecordTypeField>),
    /// Record introduction
    ///
    /// ```text
    /// record { x = t1, .. }
    /// record { id (a : Type) (x : a) : a = x, .. }
    /// ```
    RecordIntro(ByteSpan, Vec<RecordIntroField>),
    /// Record field projection
    ///
    /// ```text
    /// e.l
    /// e.l^1
    /// ```
    RecordProj(ByteSpan, Box<Term>, ByteIndex, String, Option<u32>),
    /// Terms that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Term {
    /// Return the span of source code that this term originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Parens(span, ..)
            | Term::Universe(span, ..)
            | Term::Hole(span)
            | Term::Name(span, ..)
            | Term::Import(span, ..)
            | Term::Case(span, ..)
            | Term::RecordType(span, ..)
            | Term::RecordIntro(span, ..)
            | Term::RecordProj(span, ..)
            | Term::ArrayIntro(span, ..)
            | Term::Error(span) => span,
            Term::Literal(ref literal) => literal.span(),
            Term::FunType(start, _, ref body)
            | Term::FunIntro(start, _, ref body)
            | Term::Let(start, _, ref body)
            | Term::If(start, _, _, ref body) => ByteSpan::new(start, body.span().end()),
            Term::Where(ref expr, _, end) => ByteSpan::new(expr.span().start(), end),
            Term::Ann(ref term, ref ty) => term.span().to(ty.span()),
            Term::FunArrow(ref ann, ref body) => ann.span().to(body.span()),
            Term::FunApp(ref head, ref arg) => head.span().to(arg.last().unwrap().span()),
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Term::Parens(_, ref term) => Doc::text("(").append(term.to_doc()).append(")"),
            Term::Ann(ref term, ref ty) => Doc::nil()
                .append(term.to_doc())
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ty.to_doc()),
            Term::Universe(_, None) => Doc::text("Type"),
            Term::Universe(_, Some(level)) => Doc::text(format!("Type^{}", level)),
            Term::Literal(ref literal) => literal.to_doc(),
            Term::ArrayIntro(_, ref elems) => Doc::nil()
                .append("[")
                .append(Doc::intersperse(
                    elems.iter().map(Term::to_doc),
                    Doc::text(";").append(Doc::space()),
                ))
                .append("]"),
            Term::Hole(_) => Doc::text("_"),
            Term::Name(_, ref name, None) => Doc::text(format!("{}", name)),
            Term::Name(_, ref name, Some(shift)) => Doc::text(format!("{}^{}", name, shift)),
            Term::Import(_, _, ref name) => Doc::nil()
                .append("import")
                .append(Doc::space())
                .append(format!("{:?}", name)),
            Term::FunIntro(_, ref params, ref body) => Doc::nil()
                .append("fun")
                .append(Doc::space())
                .append(pretty_fun_intro_params(params))
                .append(Doc::space())
                .append("=>")
                .append(Doc::space())
                .append(body.to_doc()),
            Term::FunType(_, ref params, ref body) => Doc::nil()
                .append("Fun")
                .append(Doc::space())
                .append(pretty_fun_ty_params(params))
                .append(Doc::space())
                .append("->")
                .append(Doc::space())
                .append(body.to_doc()),
            Term::FunArrow(ref ann, ref body) => Doc::nil()
                .append(ann.to_doc())
                .append(Doc::space())
                .append("->")
                .append(Doc::space())
                .append(body.to_doc()),
            Term::FunApp(ref head, ref args) => head.to_doc().append(Doc::space()).append(
                Doc::intersperse(args.iter().map(|arg| arg.to_doc()), Doc::space()),
            ),
            Term::Let(_, ref items, ref body) => {
                Doc::nil()
                    .append("let")
                    .append(Doc::space())
                    .append(Doc::intersperse(
                        // FIXME: Indentation
                        items.iter().map(|item| item.to_doc()),
                        Doc::newline(),
                    ))
                    .append("in")
                    .append(body.to_doc())
            },
            Term::Where(ref expr, ref items, _) => Doc::nil()
                .append(expr.to_doc())
                .append(Doc::newline())
                .append("where {")
                .append(Doc::newline())
                .append(Doc::intersperse(
                    items.iter().map(|item| item.to_doc().group()),
                    Doc::newline(),
                ))
                .append(Doc::newline())
                .nest(PRETTY_INDENT_WIDTH)
                .append("}"),
            Term::If(_, ref cond, ref if_true, ref if_false) => Doc::nil()
                .append("if")
                .append(Doc::space())
                .append(cond.to_doc())
                .append(Doc::space())
                .append("then")
                .append(Doc::space())
                .append(if_true.to_doc())
                .append(Doc::space())
                .append("else")
                .append(Doc::space())
                .append(if_false.to_doc()),
            Term::Case(_, ref head, ref clauses) => Doc::nil()
                .append("case")
                .append(Doc::space())
                .append(head.to_doc())
                .append(Doc::space())
                .append("of")
                .append(Doc::space())
                .append("{")
                .append(Doc::newline())
                .append(Doc::intersperse(
                    clauses.iter().map(|&(ref pattern, ref body)| {
                        Doc::nil()
                            .append(pattern.to_doc())
                            .append(Doc::space())
                            .append("=>")
                            .append(Doc::space())
                            .append(body.to_doc())
                            .append(";")
                    }),
                    Doc::newline(),
                ))
                .append(Doc::newline())
                .nest(PRETTY_INDENT_WIDTH)
                .append("}"),
            Term::RecordType(_, ref fields) if fields.is_empty() => Doc::text("Record {}"),
            Term::RecordIntro(_, ref fields) if fields.is_empty() => Doc::text("record {}"),
            Term::RecordType(_, ref fields) => Doc::nil()
                .append("Record {")
                .append(Doc::space())
                .append(Doc::intersperse(
                    fields.iter().map(|field| {
                        Doc::group(
                            Doc::nil()
                                .append(Doc::as_string(&field.label.1))
                                .append(match field.binder {
                                    Some((_, ref binder)) => Doc::space()
                                        .append("as")
                                        .append(Doc::space())
                                        .append(Doc::as_string(binder)),
                                    None => Doc::nil(),
                                })
                                .append(Doc::space())
                                .append(":")
                                .append(Doc::space())
                                .append(field.ann.to_doc()),
                        )
                    }),
                    Doc::text(";").append(Doc::space()),
                ))
                .nest(PRETTY_INDENT_WIDTH)
                .append(Doc::space())
                .append("}"),
            Term::RecordIntro(_, ref fields) => Doc::nil()
                .append("record {")
                .append(Doc::space())
                .append(Doc::intersperse(
                    fields.iter().map(|field| match field {
                        RecordIntroField::Punned {
                            label: (_, ref label),
                            shift,
                        } => match shift {
                            None => Doc::text(format!("{}", label)),
                            Some(shift) => Doc::text(format!("{}^{}", label, shift)),
                        },
                        RecordIntroField::Explicit {
                            label: (_, ref label),
                            ref params,
                            ref return_ann,
                            ref term,
                        } => Doc::group(
                            Doc::nil()
                                .append(Doc::as_string(label))
                                .append(Doc::space())
                                .append(match params[..] {
                                    [] => Doc::nil(),
                                    _ => pretty_fun_intro_params(params).append(Doc::space()),
                                })
                                .append(return_ann.as_ref().map_or(Doc::nil(), |return_ann| {
                                    Doc::text(":")
                                        .append(return_ann.to_doc())
                                        .append(Doc::space())
                                }))
                                .append("=")
                                .append(Doc::space())
                                .append(term.to_doc()),
                        ),
                    }),
                    Doc::text(";").append(Doc::space()),
                ))
                .nest(PRETTY_INDENT_WIDTH)
                .append(Doc::space())
                .append("}"),
            Term::RecordProj(_, ref expr, _, ref label, None) => {
                expr.to_doc().append(".").append(format!("{}", label))
            },
            Term::RecordProj(_, ref expr, _, ref label, Some(shift)) => Doc::nil()
                .append(expr.to_doc())
                .append(".")
                .append(format!("{}^{}", label, shift)),
            Term::Error(_) => Doc::text("<error>"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(PRETTY_FALLBACK_WIDTH, f)
    }
}

fn pretty_fun_intro_params(params: &[FunIntroParamGroup]) -> Doc<BoxDoc<()>> {
    Doc::intersperse(
        params.iter().map(|&(ref names, ref ann)| match *ann {
            None if names.len() == 1 => Doc::as_string(&names[0].1),
            None => unreachable!(), // FIXME - shouldn't be possible in AST
            Some(ref ann) => Doc::nil()
                .append("(")
                .append(Doc::intersperse(
                    names.iter().map(|name| Doc::as_string(&name.1)),
                    Doc::space(),
                ))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ann.to_doc())
                .append(")"),
        }),
        Doc::space(),
    )
}

fn pretty_fun_ty_params(params: &[FunTypeParamGroup]) -> Doc<BoxDoc<()>> {
    Doc::intersperse(
        params.iter().map(|&(ref names, ref ann)| {
            Doc::nil()
                .append("(")
                .append(Doc::intersperse(
                    names.iter().map(|name| Doc::as_string(&name.1)),
                    Doc::space(),
                ))
                .append(Doc::space())
                .append(":")
                .append(Doc::space())
                .append(ann.to_doc())
                .append(")")
        }),
        Doc::space(),
    )
}
