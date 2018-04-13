//! Errors that might be produced during semantic analysis

use codespan::ByteSpan;
use codespan_reporting::{Diagnostic, Label};
use nameless::{BoundName, Name};
use std::fmt;
use std::rc::Rc;

use syntax::core::{Constant, RawConstant, Term, Type};
use syntax::translation::ToConcrete;

/// An internal error. These are bugs!
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum InternalError {
    #[fail(display = "Unsubstituted debruijn index: `{}{}`.", name, index)]
    UnsubstitutedDebruijnIndex {
        span: ByteSpan,
        name: Name,
        index: BoundName,
    },
    #[fail(display = "Argument applied to non-function.")]
    ArgumentAppliedToNonFunction { span: ByteSpan },
}

impl InternalError {
    pub fn span(&self) -> ByteSpan {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { span, .. } => span,
            InternalError::ArgumentAppliedToNonFunction { span, .. } => span,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex {
                span,
                ref name,
                index,
            } => Diagnostic::new_bug(format!("unsubstituted debruijn index: `{}{}`", name, index))
                .with_label(Label::new_primary(span).with_message("index found here")),
            InternalError::ArgumentAppliedToNonFunction { span } => {
                Diagnostic::new_bug(format!("argument applied to non-function"))
                    .with_label(Label::new_primary(span).with_message("not a function"))
            },
        }
    }
}

/// An error produced during typechecking
#[derive(Debug, Clone, PartialEq)] // FIXME: Derive `Fail` (Rc does not impl `Send + Sync`)
pub enum TypeError {
    ArgAppliedToNonFunction {
        fn_span: ByteSpan,
        arg_span: ByteSpan,
        found: Rc<Type>,
    },
    FunctionParamNeedsAnnotation {
        param_span: ByteSpan,
        var_span: Option<ByteSpan>,
        name: Name,
    },
    LiteralMismatch {
        literal_span: ByteSpan,
        found: RawConstant,
        expected: Constant,
    },
    AmbiguousIntLiteral {
        span: ByteSpan,
    },
    AmbiguousFloatLiteral {
        span: ByteSpan,
    },
    UnableToElaborateHole {
        span: ByteSpan,
        expected: Option<Rc<Type>>,
    },
    Mismatch {
        span: ByteSpan,
        found: Rc<Type>,
        expected: Rc<Type>,
    },
    UnexpectedFunction {
        span: ByteSpan,
        expected: Rc<Type>,
    },
    ExpectedUniverse {
        span: ByteSpan,
        found: Rc<Type>,
    },
    UndefinedName {
        var_span: ByteSpan,
        name: Name,
    },
    Internal(InternalError),
}

impl TypeError {
    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            TypeError::Internal(ref err) => err.to_diagnostic(),
            TypeError::ArgAppliedToNonFunction {
                fn_span,
                arg_span,
                ref found,
            } => Diagnostic::new_error(format!(
                "applied an argument to a term that was not a function - found type `{}`",
                Term::from(&**found).to_concrete(),
            )).with_label(Label::new_primary(fn_span).with_message("the term"))
                .with_label(Label::new_secondary(arg_span).with_message("the applied argument")),
            TypeError::FunctionParamNeedsAnnotation {
                param_span,
                var_span: _, // TODO
                ref name,
            } => Diagnostic::new_error(format!(
                "type annotation needed for the function parameter `{}`",
                name
            )).with_label(
                Label::new_primary(param_span)
                    .with_message("the parameter that requires an annotation"),
            ),
            TypeError::LiteralMismatch {
                literal_span,
                ref found,
                ref expected,
            } => {
                let found_text = match *found {
                    RawConstant::String(_) => "string",
                    RawConstant::Char(_) => "character",
                    RawConstant::Int(_) => "numeric",
                    RawConstant::Float(_) => "floating point",
                };

                Diagnostic::new_error(format!(
                    "found a {} literal, but expected a type `{}`",
                    found_text,
                    expected.to_concrete(),
                )).with_label(Label::new_primary(literal_span).with_message("the literal"))
            },
            TypeError::AmbiguousIntLiteral { span } => {
                Diagnostic::new_error("ambiguous integer literal").with_label(
                    Label::new_primary(span).with_message("type annotations needed here"),
                )
            },
            TypeError::AmbiguousFloatLiteral { span } => {
                Diagnostic::new_error("ambiguous floating point literal").with_label(
                    Label::new_primary(span).with_message("type annotations needed here"),
                )
            },
            TypeError::UnableToElaborateHole {
                span,
                expected: None,
                ..
            } => Diagnostic::new_error("unable to elaborate hole")
                .with_label(Label::new_primary(span).with_message("the hole")),
            TypeError::UnableToElaborateHole {
                span,
                expected: Some(ref expected),
                ..
            } => Diagnostic::new_error(format!(
                "unable to elaborate hole - expected: `{}`",
                Term::from(&**expected).to_concrete(),
            )).with_label(Label::new_primary(span).with_message("the hole")),
            TypeError::UnexpectedFunction {
                span, ref expected, ..
            } => Diagnostic::new_error(format!(
                "found a function but expected a term of type `{}`",
                Term::from(&**expected).to_concrete(),
            )).with_label(Label::new_primary(span).with_message("the function")),
            TypeError::Mismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic::new_error(format!(
                "found a term of type `{}`, but expected a term of type `{}`",
                Term::from(&**found).to_concrete(),
                Term::from(&**expected).to_concrete(),
            )).with_label(Label::new_primary(span).with_message("the term")),
            TypeError::ExpectedUniverse { ref found, span } => {
                Diagnostic::new_error(format!("expected type, found a value of type `{}`", found))
                    .with_label(Label::new_primary(span).with_message("the value"))
            },
            TypeError::UndefinedName { ref name, var_span } => {
                Diagnostic::new_error(format!("cannot find `{}` in scope", name)).with_label(
                    Label::new_primary(var_span).with_message("not found in this scope"),
                )
            },
        }
    }
}

impl From<InternalError> for TypeError {
    fn from(src: InternalError) -> TypeError {
        TypeError::Internal(src)
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeError::ArgAppliedToNonFunction { ref found, .. } => write!(
                f,
                "Applied an argument to a non-function type `{}`",
                Term::from(&**found).to_concrete(),
            ),
            TypeError::FunctionParamNeedsAnnotation { ref name, .. } => write!(
                f,
                "Type annotation needed for the function parameter `{}`",
                name,
            ),
            TypeError::LiteralMismatch {
                ref found,
                ref expected,
                ..
            } => {
                let found_text = match *found {
                    RawConstant::String(_) => "string",
                    RawConstant::Char(_) => "character",
                    RawConstant::Int(_) => "numeric",
                    RawConstant::Float(_) => "floating point",
                };

                write!(
                    f,
                    "found a {} literal, but expected a type `{}`",
                    found_text,
                    expected.to_concrete(),
                )
            },
            TypeError::AmbiguousIntLiteral { .. } => write!(f, "Ambiguous integer literal"),
            TypeError::AmbiguousFloatLiteral { .. } => {
                write!(f, "Ambiguous floating point literal")
            },
            TypeError::UnableToElaborateHole { expected: None, .. } => {
                write!(f, "Unable to elaborate hole")
            },
            TypeError::UnableToElaborateHole {
                expected: Some(ref expected),
                ..
            } => write!(
                f,
                "Unable to elaborate hole, expected: `{}`",
                Term::from(&**expected).to_concrete(),
            ),
            TypeError::Mismatch {
                ref found,
                ref expected,
                ..
            } => write!(
                f,
                "Type mismatch: found `{}` but `{}` was expected",
                Term::from(&**found).to_concrete(),
                Term::from(&**expected).to_concrete(),
            ),
            TypeError::UnexpectedFunction { ref expected, .. } => write!(
                f,
                "Found a function but expected `{}`",
                Term::from(&**expected).to_concrete()
            ),
            TypeError::ExpectedUniverse { ref found, .. } => write!(
                f,
                "Found `{}` but a universe was expected",
                Term::from(&**found).to_concrete(),
            ),
            TypeError::UndefinedName { ref name, .. } => write!(f, "Undefined name `{}`", name),
            TypeError::Internal(ref err) => write!(f, "Internal error - this is a bug! {}", err),
        }
    }
}
