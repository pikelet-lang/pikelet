use lalrpop_util::ParseError as LalrpopError;
use codespan::FileMap;
use codespan::{ByteIndex, ByteSpan};
use codespan_reporting::Diagnostic;
use std::fmt;

use syntax::parse::{LexerError, Token};

#[derive(Fail, Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    #[fail(display = "{}", _0)]
    Lexer(#[cause] LexerError),
    #[fail(display = "An identifier was expected when parsing a pi type.")]
    IdentifierExpectedInPiType { span: ByteSpan },
    #[fail(display = "An integer literal {} was too large for the target type.", value)]
    IntegerLiteralOverflow { span: ByteSpan, value: u64 },
    #[fail(display = "Unknown repl command `:{}` found.", command)]
    UnknownReplCommand { span: ByteSpan, command: String },
    #[fail(display = "Unexpected EOF, expected one of: {}.", expected)]
    UnexpectedEof {
        end: ByteIndex,
        expected: ExpectedTokens,
    },
    #[fail(display = "Unexpected token {}, found, expected one of: {}.", token, expected)]
    UnexpectedToken {
        span: ByteSpan,
        token: Token<String>,
        expected: ExpectedTokens,
    },
    #[fail(display = "Extra token {} found", token)]
    ExtraToken {
        span: ByteSpan,
        token: Token<String>,
    },
}

/// Flatten away an LALRPOP error, leaving the inner `ParseError` behind
pub fn from_lalrpop<T>(filemap: &FileMap, err: LalrpopError<ByteIndex, T, ParseError>) -> ParseError
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
            end: filemap.span().end(),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::UnrecognizedToken {
            token: Some((start, token, end)),
            expected,
        } => ParseError::UnexpectedToken {
            span: ByteSpan::new(start, end),
            token: token.into(),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::ExtraToken {
            token: (start, token, end),
        } => ParseError::ExtraToken {
            span: ByteSpan::new(start, end),
            token: token.into(),
        },
    }
}

impl ParseError {
    /// Return the span of source code that this error originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            ParseError::Lexer(ref err) => err.span(),
            ParseError::IdentifierExpectedInPiType { span }
            | ParseError::IntegerLiteralOverflow { span, .. }
            | ParseError::UnknownReplCommand { span, .. }
            | ParseError::UnexpectedToken { span, .. }
            | ParseError::ExtraToken { span, .. } => span,
            ParseError::UnexpectedEof { end, .. } => ByteSpan::new(end, end),
        }
    }

    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            ParseError::Lexer(ref err) => err.to_diagnostic(),
            ParseError::IdentifierExpectedInPiType { span } => {
                Diagnostic::new_error("identifier expected when parsing dependent function type")
                    .with_primary_label(span, "ill-formed dependent function type")
            },
            ParseError::IntegerLiteralOverflow { span, value } => {
                Diagnostic::new_error(format!("integer literal overflow with value `{}`", value))
                    .with_primary_label(span, "overflowing literal")
            },
            ParseError::UnknownReplCommand { span, ref command } => {
                Diagnostic::new_error(format!("unknown repl command `:{}`", command))
                    .with_primary_label(span, "unexpected command")
            },
            ParseError::UnexpectedToken {
                span,
                ref token,
                ref expected,
            } => Diagnostic::new_error(format!("expected one of {}, found `{}`", expected, token))
                .with_primary_label(span, "unexpected token"),
            ParseError::UnexpectedEof { end, ref expected } => {
                Diagnostic::new_error(format!("expected one of {}, found `EOF`", expected))
                    .with_primary_label(ByteSpan::new(end, end), "unexpected EOF")
            },
            ParseError::ExtraToken { span, ref token } => {
                Diagnostic::new_error(format!("extra token `{}`", token))
                    .with_primary_label(span, "extra token")
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
