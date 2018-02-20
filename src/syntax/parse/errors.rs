use lalrpop_util::ParseError as LalrpopError;
use source::FileMap;
use source::pos::{BytePos, Span};
use source_reporting::{Diagnostic, Severity, SpanLabel, UnderlineStyle};
use std::fmt;

use syntax::parse::{LexerError, Token};

#[derive(Fail, Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    #[fail(display = "{}", _0)]
    Lexer(#[cause] LexerError),
    #[fail(display = "An identifier was expected when parsing a pi type at byte range {}.", span)]
    IdentifierExpectedInPiType { span: Span },
    #[fail(display = "An integer literal {} was too large for the target type at byte range {}.",
           value, span)]
    IntegerLiteralOverflow { span: Span, value: u64 },
    #[fail(display = "Unknown repl command `:{}` found at byte range {}.", command, span)]
    UnknownReplCommand { span: Span, command: String },
    #[fail(display = "Unexpected EOF at byte pos {}, expected one of: {}.", end, expected)]
    UnexpectedEof {
        end: BytePos,
        expected: ExpectedTokens,
    },
    #[fail(display = "Unexpected token {}, found at byte range {}, expected one of: {}.", token,
           span, expected)]
    UnexpectedToken {
        span: Span,
        token: Token<String>,
        expected: ExpectedTokens,
    },
    #[fail(display = "Extra token {} found at byte range {}", token, span)]
    ExtraToken { span: Span, token: Token<String> },
}

/// Flatten away an LALRPOP error, leaving the inner `ParseError` behind
pub fn from_lalrpop<T>(filemap: &FileMap, err: LalrpopError<BytePos, T, ParseError>) -> ParseError
where
    T: Into<Token<String>>,
{
    match err {
        LalrpopError::User { error } => error,
        LalrpopError::InvalidToken { .. } => unreachable!(),
        LalrpopError::UnrecognizedToken {
            token: None,
            expected,
        } => ParseError::UnexpectedEof {
            end: filemap.span().hi(),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::UnrecognizedToken {
            token: Some((lo, token, hi)),
            expected,
        } => ParseError::UnexpectedToken {
            span: Span::new(lo, hi),
            token: token.into(),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::ExtraToken {
            token: (lo, token, hi),
        } => ParseError::ExtraToken {
            span: Span::new(lo, hi),
            token: token.into(),
        },
    }
}

impl ParseError {
    /// Return the span of source code that this error originated from
    pub fn span(&self) -> Span {
        match *self {
            ParseError::Lexer(ref err) => err.span(),
            ParseError::IdentifierExpectedInPiType { span }
            | ParseError::IntegerLiteralOverflow { span, .. }
            | ParseError::UnknownReplCommand { span, .. }
            | ParseError::UnexpectedToken { span, .. }
            | ParseError::ExtraToken { span, .. } => span,
            ParseError::UnexpectedEof { end, .. } => Span::new(end, end),
        }
    }

    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            ParseError::Lexer(ref err) => err.to_diagnostic(),
            ParseError::IdentifierExpectedInPiType { span } => Diagnostic {
                severity: Severity::Error,
                message: format!("identifier expected when parsing dependent function type"),
                spans: vec![
                    SpanLabel {
                        label: Some("ill-formed dependent function type".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::IntegerLiteralOverflow { span, value } => Diagnostic {
                severity: Severity::Error,
                message: format!("integer literal overflow with value `{}`", value),
                spans: vec![
                    SpanLabel {
                        label: Some("overflowing literal".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::UnknownReplCommand { span, ref command } => Diagnostic {
                severity: Severity::Error,
                message: format!("unknown repl command `:{}`", command),
                spans: vec![
                    SpanLabel {
                        label: Some("unexpected command".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::UnexpectedToken {
                span,
                ref token,
                ref expected,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected one of {}, found `{}`", expected, token),
                spans: vec![
                    SpanLabel {
                        label: Some("unexpected token".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::UnexpectedEof { end, ref expected } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected one of {}, found `EOF`", expected),
                spans: vec![
                    SpanLabel {
                        label: Some("unexpected EOF".into()),
                        style: UnderlineStyle::Primary,
                        span: Span::new(end, end),
                    },
                ],
            },
            ParseError::ExtraToken { span, ref token } => Diagnostic {
                severity: Severity::Error,
                message: format!("extra token `{}`", token),
                spans: vec![
                    SpanLabel {
                        label: Some("extra token".into()),
                        style: UnderlineStyle::Primary,
                        span,
                    },
                ],
            },
        }
    }
}

impl From<LexerError> for ParseError {
    fn from(src: LexerError) -> ParseError {
        ParseError::Lexer(src)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedTokens(pub Vec<String>);

impl fmt::Display for ExpectedTokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            match i {
                0 => write!(f, "{}", token)?,
                i if i < self.0.len() - 1 => write!(f, ", {}", token)?,
                _ => write!(f, ", or {}", token)?,
            }
        }
        Ok(())
    }
}
