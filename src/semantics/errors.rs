//! Errors that might be produced during semantic analysis

use source::pos::Span;
use source::reporting::Diagnostic;
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
            InternalError::UnsubstitutedDebruijnIndex { span, ref index } => write!(
                f,
                "Undefined name `{}{}`, at byte range {}.",
                index.name, index.inner, span
            ),
            InternalError::UndefinedName { span, ref name } => {
                write!(f, "Undefined name `{}`, at byte range {}.", name, span)
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
        use source::reporting::Severity;

        // TODO: add contextual labels to underlines

        let message = match *self {
            TypeError::Internal(ref err) => format!("internal error - this is a bug! {}", err),
            TypeError::NotAFunctionType { ref found, .. } => format!(
                "applied an argument to a term that was not a function - found type `{}`",
                found,
            ),
            TypeError::TypeAnnotationsNeeded { .. } => format!("type annotations needed"),
            TypeError::UnexpectedFunction { ref expected, .. } => format!(
                "found a function but expected a term of type `{}`",
                expected
            ),
            TypeError::Mismatch {
                ref found,
                ref expected,
                ..
            } => format!(
                "found a term of type `{}`, but expected a term of type `{}`",
                found, expected,
            ),
            TypeError::ExpectedUniverse { ref found, .. } => {
                format!("expected type, found value `{}`", found)
            },
            TypeError::UndefinedName { ref name, .. } => format!("cannot find `{}` in scope", name),
        };
        Diagnostic::spanned(self.span(), Severity::Error, message)
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
            TypeError::NotAFunctionType { span, ref found } => write!(
                f,
                "Applied an argument to a non-function type `{}`, at byte range {}.",
                found, span,
            ),
            TypeError::TypeAnnotationsNeeded { span } => {
                write!(f, "Type annotations needed, at byte range {}.", span)
            },
            TypeError::Mismatch {
                span,
                ref found,
                ref expected,
            } => write!(
                f,
                "Type mismatch: found `{}` but `{}` was expected, at byte range {}.",
                found, expected, span,
            ),
            TypeError::UnexpectedFunction { span, ref expected } => write!(
                f,
                "Found a function but expected `{}`, at byte range {}.",
                expected, span,
            ),
            TypeError::ExpectedUniverse { span, ref found } => write!(
                f,
                "Found `{}` but a universe was expected, at byte range {}.",
                found, span,
            ),
            TypeError::UndefinedName { span, ref name } => {
                write!(f, "Undefined name `{}`, at byte range {}.", name, span)
            },
            TypeError::Internal(ref err) => write!(f, "Internal error - this is a bug! {}", err),
        }
    }
}
