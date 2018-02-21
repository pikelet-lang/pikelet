//! Errors that might be produced during semantic analysis

use codespan::ByteSpan;
use codespan_reporting::{Diagnostic, Label, LabelStyle, Severity};
use std::fmt;

use syntax::core::{Name, RcType};
use syntax::var::{Debruijn, Named};

/// An internal error. These are bugs!
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum InternalError {
    UnsubstitutedDebruijnIndex {
        span: ByteSpan,
        index: Named<Name, Debruijn>,
    },
    UndefinedName {
        var_span: ByteSpan,
        name: Name,
    },
}

impl InternalError {
    pub fn span(&self) -> ByteSpan {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { span, .. } => span,
            InternalError::UndefinedName { var_span, .. } => var_span,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { span, ref index } => Diagnostic {
                severity: Severity::Bug,
                message: format!(
                    "unsubstituted debruijn index: `{}{}`",
                    index.name, index.inner,
                ),
                labels: vec![
                    Label {
                        message: Some("index found here".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            InternalError::UndefinedName { ref name, var_span } => Diagnostic {
                severity: Severity::Bug,
                message: format!("cannot find `{}` in scope", name),
                labels: vec![
                    Label {
                        message: Some("not found in this scope".into()),
                        style: LabelStyle::Primary,
                        span: var_span,
                    },
                ],
            },
        }
    }
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex { ref index, .. } => write!(
                f,
                "Unsubstituted debruijn index: `{}{}`.",
                index.name, index.inner,
            ),
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
        fn_span: ByteSpan,
        arg_span: ByteSpan,
        found: RcType,
    },
    FunctionParamNeedsAnnotation {
        param_span: ByteSpan,
        name: Name,
    },
    Mismatch {
        span: ByteSpan,
        found: RcType,
        expected: RcType,
    },
    UnexpectedFunction {
        span: ByteSpan,
        expected: RcType,
    },
    ExpectedUniverse {
        span: ByteSpan,
        found: RcType,
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
            TypeError::NotAFunctionType {
                fn_span,
                arg_span,
                ref found,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "applied an argument to a term that was not a function - found type `{}`",
                    found,
                ),
                labels: vec![
                    Label {
                        message: Some("the term".into()),
                        style: LabelStyle::Primary,
                        span: fn_span,
                    },
                    Label {
                        message: Some("the applied argument".into()),
                        style: LabelStyle::Secondary,
                        span: arg_span,
                    },
                ],
            },
            TypeError::FunctionParamNeedsAnnotation {
                param_span,
                ref name,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "type annotation needed for the function parameter `{}`",
                    name
                ),
                labels: vec![
                    Label {
                        message: Some("the parameter that requires an annotation".into()),
                        style: LabelStyle::Primary,
                        span: param_span,
                    },
                ],
            },
            TypeError::UnexpectedFunction {
                span, ref expected, ..
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "found a function but expected a term of type `{}`",
                    expected,
                ),
                labels: vec![
                    Label {
                        message: None, // TODO
                        style: LabelStyle::Primary,
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
                labels: vec![
                    Label {
                        message: Some("the value".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::ExpectedUniverse { ref found, span } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected type, found value `{}`", found),
                labels: vec![
                    Label {
                        message: Some("the value".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            TypeError::UndefinedName { ref name, var_span } => Diagnostic {
                severity: Severity::Error,
                message: format!("cannot find `{}` in scope", name),
                labels: vec![
                    Label {
                        message: Some("not found in this scope".into()),
                        style: LabelStyle::Primary,
                        span: var_span,
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
            TypeError::FunctionParamNeedsAnnotation { ref name, .. } => write!(
                f,
                "Type annotation needed for the function parameter `{}`",
                name,
            ),
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
