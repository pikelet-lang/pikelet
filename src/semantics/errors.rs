//! Errors that might be produced during semantic analysis

use source::pos::Span;
use source::reporting::{Diagnostic, Severity, SpanLabel, UnderlineStyle};
use std::fmt;

use syntax::core::{Name, RcType};
use syntax::var::{Debruijn, Named};

/// An internal error. These are bugs!
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum InternalError {
    UnsubstitutedDebruijnIndex {
        span: Span,
        index: Named<Name, Debruijn>,
    },
    UndefinedName {
        span: Span,
        name: Name,
    },
}

impl InternalError {
    pub fn span(&self) -> Span {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { span, .. }
            | InternalError::UndefinedName { span, .. } => span,
        }
    }
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { ref index, .. } => {
                write!(f, "Undefined name `{}{}`.", index.name, index.inner)
            },
            InternalError::UndefinedName { ref name, .. } => {
                write!(f, "Undefined name `{}`.", name)
            },
        }
    }
}

/// An error produced during typechecking
#[derive(Debug, Clone, PartialEq)] // FIXME: Derive `Fail`
pub enum TypeError {
    NotAFunctionType {
        span: Span,
        found: RcType,
    },
    TypeAnnotationsNeeded {
        span: Span,
    },
    Mismatch {
        span: Span,
        found: RcType,
        expected: RcType,
    },
    UnexpectedFunction {
        span: Span,
        expected: RcType,
    },
    ExpectedUniverse {
        span: Span,
        found: RcType,
    },
    UndefinedName {
        span: Span,
        name: Name,
    },
    Internal(InternalError),
}

impl TypeError {
    pub fn span(&self) -> Span {
        match *self {
            TypeError::NotAFunctionType { span, .. }
            | TypeError::TypeAnnotationsNeeded { span, .. }
            | TypeError::UnexpectedFunction { span, .. }
            | TypeError::Mismatch { span, .. }
            | TypeError::ExpectedUniverse { span, .. }
            | TypeError::UndefinedName { span, .. } => span,
            TypeError::Internal(ref err) => err.span(),
        }
    }

    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            TypeError::Internal(ref err) => Diagnostic {
                severity: Severity::Error,
                message: format!("{}", err),
                spans: vec![
                    SpanLabel {
                        label: None, // TODO
                        style: UnderlineStyle::Primary,
                        span: err.span(),
                    },
                ],
            },
            TypeError::NotAFunctionType { span, ref found } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "applied an argument to a term that was not a function - found type `{}`",
                    found,
                ),
                spans: vec![
                    SpanLabel {
                        label: None, // TODO
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::TypeAnnotationsNeeded { span } => Diagnostic {
                severity: Severity::Error,
                message: format!("type annotations needed"),
                spans: vec![
                    SpanLabel {
                        label: None, // TODO
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::UnexpectedFunction {
                span, ref expected, ..
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "found a function but expected a term of type `{}`",
                    expected
                ),
                spans: vec![
                    SpanLabel {
                        label: None, // TODO
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::Mismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "found a term of type `{}`, but expected a term of type `{}`",
                    found, expected,
                ),
                spans: vec![
                    SpanLabel {
                        label: None, // TODO
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::ExpectedUniverse { ref found, .. } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected type, found value `{}`", found),
                spans: vec![],
            },
            TypeError::UndefinedName { ref name, span } => Diagnostic {
                severity: Severity::Error,
                message: format!("cannot find `{}` in scope", name),
                spans: vec![
                    SpanLabel {
                        label: Some("not found in this scope".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
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
            TypeError::NotAFunctionType { ref found, .. } => {
                write!(f, "Applied an argument to a non-function type `{}`", found,)
            },
            TypeError::TypeAnnotationsNeeded { .. } => write!(f, "Type annotations needed"),
            TypeError::Mismatch {
                ref found,
                ref expected,
                ..
            } => write!(
                f,
                "Type mismatch: found `{}` but `{}` was expected",
                found, expected,
            ),
            TypeError::UnexpectedFunction { ref expected, .. } => {
                write!(f, "Found a function but expected `{}`", expected,)
            },
            TypeError::ExpectedUniverse { ref found, .. } => {
                write!(f, "Found `{}` but a universe was expected", found,)
            },
            TypeError::UndefinedName { ref name, .. } => write!(f, "Undefined name `{}`", name),
            TypeError::Internal(ref err) => write!(f, "Internal error - this is a bug! {}", err),
        }
    }
}
