//! Errors that might be produced during semantic analysis

use codespan::ByteSpan;
use codespan_reporting::{Diagnostic, Label};
use nameless::{BoundName, Ident, Name};

use syntax;
use syntax::concrete;
use syntax::raw;

/// An internal error. These are bugs!
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum InternalError {
    #[fail(display = "Unsubstituted debruijn index: `{}`, `{:?}`.", index, hint)]
    UnsubstitutedDebruijnIndex {
        span: Option<ByteSpan>,
        index: BoundName,
        hint: Option<Ident>,
    },
    #[fail(display = "Argument applied to non-function.")]
    ArgumentAppliedToNonFunction,
    #[fail(display = "Expected a boolean expression.")]
    ExpectedBoolExpr,
    #[fail(display = "Projected on non-existent field `{}`.", label)]
    ProjectedOnNonExistentField { label: syntax::Label },
}

impl InternalError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            InternalError::UnsubstitutedDebruijnIndex {
                span,
                index,
                ref hint,
            } => {
                let base = Diagnostic::new_bug(format!(
                    "unsubstituted debruijn index: `{}`, `{:?}`",
                    index, hint
                ));

                match span {
                    None => base,
                    Some(span) => {
                        base.with_label(Label::new_primary(span).with_message("index found here"))
                    },
                }
            },
            InternalError::ArgumentAppliedToNonFunction => {
                Diagnostic::new_bug("argument applied to non-function")
            },
            InternalError::ExpectedBoolExpr => Diagnostic::new_bug("expected a boolean expression"),
            InternalError::ProjectedOnNonExistentField { ref label } => {
                Diagnostic::new_bug(format!("projected on non-existent field `{}`.", label))
            },
        }
    }
}

/// An error produced during type checking
#[derive(Debug, Fail, Clone, PartialEq)]
pub enum TypeError {
    #[fail(display = "Applied an argument to a non-function type `{}`", found)]
    ArgAppliedToNonFunction {
        fn_span: ByteSpan,
        arg_span: ByteSpan,
        found: Box<concrete::Term>,
    },
    #[fail(display = "Type annotation needed for the function parameter `{}`", name)]
    FunctionParamNeedsAnnotation {
        param_span: ByteSpan,
        var_span: Option<ByteSpan>,
        name: Name,
    },
    #[fail(display = "found a `{}`, but expected a type `{}`", found, expected)]
    LiteralMismatch {
        literal_span: ByteSpan,
        found: raw::Literal,
        expected: Box<concrete::Term>,
    },
    #[fail(display = "Ambiguous integer literal")]
    AmbiguousIntLiteral { span: ByteSpan },
    #[fail(display = "Ambiguous floating point literal")]
    AmbiguousFloatLiteral { span: ByteSpan },
    #[fail(display = "Unable to elaborate hole, expected: `{:?}`", expected)]
    UnableToElaborateHole {
        span: ByteSpan,
        expected: Option<Box<concrete::Term>>,
    },
    #[fail(display = "Type mismatch: found `{}` but `{}` was expected", found, expected)]
    Mismatch {
        span: ByteSpan,
        found: Box<concrete::Term>,
        expected: Box<concrete::Term>,
    },
    #[fail(display = "Found a function but expected `{}`", expected)]
    UnexpectedFunction {
        span: ByteSpan,
        expected: Box<concrete::Term>,
    },
    #[fail(display = "Found `{}` but a universe was expected", found)]
    ExpectedUniverse {
        span: ByteSpan,
        found: Box<concrete::Term>,
    },
    #[fail(display = "Undefined name `{}`", name)]
    UndefinedName { var_span: ByteSpan, name: Name },
    #[fail(display = "Label mismatch: found label `{}` but `{}` was expected", found, expected)]
    LabelMismatch {
        span: ByteSpan,
        found: syntax::Label,
        expected: syntax::Label,
    },
    #[fail(display = "Ambiguous record")]
    AmbiguousRecord { span: ByteSpan },
    #[fail(
        display = "Mismatched array length: expected {} elements but found {}",
        expected_len,
        found_len
    )]
    ArrayLengthMismatch {
        span: ByteSpan,
        found_len: u64,
        expected_len: u64,
    },
    #[fail(display = "Ambiguous record")]
    AmbiguousArrayLiteral { span: ByteSpan },
    #[fail(display = "The type `{}` does not contain a field named `{}`.", found, expected_label)]
    NoFieldInType {
        label_span: ByteSpan,
        expected_label: syntax::Label,
        found: Box<concrete::Term>,
    },
    #[fail(display = "Internal error - this is a bug! {}", _0)]
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
                found,
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
                    raw::Literal::String(_) => "string",
                    raw::Literal::Char(_) => "character",
                    raw::Literal::Int(_) => "numeric",
                    raw::Literal::Float(_) => "floating point",
                };

                Diagnostic::new_error(format!(
                    "found a {} literal, but expected a type `{}`",
                    found_text, expected,
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
                expected,
            )).with_label(Label::new_primary(span).with_message("the hole")),
            TypeError::UnexpectedFunction {
                span, ref expected, ..
            } => Diagnostic::new_error(format!(
                "found a function but expected a term of type `{}`",
                expected,
            )).with_label(Label::new_primary(span).with_message("the function")),
            TypeError::Mismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic::new_error(format!(
                "found a term of type `{}`, but expected a term of type `{}`",
                found, expected,
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
            TypeError::LabelMismatch {
                span,
                ref expected,
                ref found,
            } => Diagnostic::new_error(format!(
                "expected field called `{}`, but found a field called `{}",
                expected, found,
            )).with_label(Label::new_primary(span)),
            TypeError::AmbiguousRecord { span } => Diagnostic::new_error("ambiguous record")
                .with_label(Label::new_primary(span).with_message("type annotations needed here")),
            TypeError::ArrayLengthMismatch {
                span,
                found_len,
                expected_len,
            } => Diagnostic::new_error(format!(
                "mismatched array length: expected {} elements but found {}",
                expected_len, found_len
            )).with_label(
                Label::new_primary(span).with_message(format!("array with {} elements", found_len)),
            ),
            TypeError::AmbiguousArrayLiteral { span } => {
                Diagnostic::new_error("ambiguous array literal").with_label(
                    Label::new_primary(span).with_message("type annotations needed here"),
                )
            },
            TypeError::NoFieldInType {
                label_span,
                ref expected_label,
                ref found,
            } => Diagnostic::new_error(format!(
                "the type `{}` does not contain a field called `{}`",
                found, expected_label
            )).with_label(Label::new_primary(label_span).with_message("the field lookup")),
        }
    }
}

impl From<InternalError> for TypeError {
    fn from(src: InternalError) -> TypeError {
        TypeError::Internal(src)
    }
}
