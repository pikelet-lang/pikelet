use codespan_reporting::diagnostic::{Diagnostic, Label};
use pretty::DocAllocator;
use std::ops::Range;

use crate::lang::core;
use crate::pass::surface_to_core;

/// Global diagnostic messages
#[derive(Clone, Debug)]
pub enum Message {
    /// Errors produced during lexing
    Lexer(LexerError),
    /// Errors produced during parsing
    Parse(ParseError),
    /// Messages produced from [lang::core::typing]
    CoreTyping(core::typing::Message),
    /// Messages produced from [pass::surface_to_core]
    SurfaceToCore(surface_to_core::Message),
}

impl From<LexerError> for Message {
    fn from(error: LexerError) -> Self {
        Message::Lexer(error)
    }
}

impl From<ParseError> for Message {
    fn from(error: ParseError) -> Self {
        Message::Parse(error)
    }
}

impl From<core::typing::Message> for Message {
    fn from(message: core::typing::Message) -> Self {
        Message::CoreTyping(message)
    }
}

impl From<surface_to_core::Message> for Message {
    fn from(message: surface_to_core::Message) -> Self {
        Message::SurfaceToCore(message)
    }
}

impl Message {
    pub fn from_lalrpop<T: std::fmt::Display>(
        error: lalrpop_util::ParseError<usize, T, LexerError>,
    ) -> Message {
        use lalrpop_util::ParseError::*;

        match error {
            InvalidToken { location } => Message::from(LexerError::InvalidToken {
                range: location..location,
            }),
            UnrecognizedEOF { location, expected } => Message::from(ParseError::UnrecognizedEOF {
                range: location..location,
                expected,
            }),
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Message::from(ParseError::UnrecognizedToken {
                range: start..end,
                token: token.to_string(),
                expected,
            }),
            ExtraToken {
                token: (start, token, end),
            } => Message::from(ParseError::ExtraToken {
                range: start..end,
                token: token.to_string(),
            }),
            User { error } => Message::from(error),
        }
    }

    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<()>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::Lexer(error) => error.to_diagnostic(),
            Message::Parse(error) => error.to_diagnostic(),
            Message::CoreTyping(message) => message.to_diagnostic(pretty_alloc),
            Message::SurfaceToCore(message) => message.to_diagnostic(pretty_alloc),
        }
    }
}

/// Lexer errors
#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidToken { range: Range<usize> },
}

impl LexerError {
    pub fn to_diagnostic(&self) -> Diagnostic<()> {
        match self {
            LexerError::InvalidToken { range } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), range.clone())]),
        }
    }
}

/// Parse errors
#[derive(Clone, Debug)]
pub enum ParseError {
    UnrecognizedEOF {
        range: Range<usize>,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        range: Range<usize>,
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        range: Range<usize>,
        token: String,
    },
}

impl ParseError {
    pub fn to_diagnostic(&self) -> Diagnostic<()> {
        match self {
            ParseError::UnrecognizedEOF { range, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary((), range.clone()).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseError::UnrecognizedToken {
                range,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(vec![
                    Label::primary((), range.clone()).with_message("unexpected token")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseError::ExtraToken { range, token } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(vec![
                    Label::primary((), range.clone()).with_message("extra token")
                ]),
        }
    }
}

fn format_expected(expected: &[String]) -> Option<String> {
    use itertools::Itertools;

    expected.split_last().map(|items| match items {
        // TODO: Improve token formatting
        (last, []) => format!("expected {}", last),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
}
