use codespan_reporting::diagnostic::{Diagnostic, Label};
use pretty::DocAllocator;
use std::ops::Range;

use crate::lang::core;
use crate::pass::surface_to_core;

#[derive(Clone, Debug)]
pub enum Message {
    Lexer(LexerError),
    Parse(ParseError),
    CoreTyping(core::typing::Message),
    SurfaceToCore(surface_to_core::Message),
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
            InvalidToken { location } => Message::Lexer(LexerError::InvalidToken {
                range: location..location,
            }),
            UnrecognizedEOF { location, expected } => Message::Parse(ParseError::UnrecognizedEOF {
                range: location..location,
                expected,
            }),
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Message::Parse(ParseError::UnrecognizedToken {
                range: start..end,
                token: token.to_string(),
                expected,
            }),
            ExtraToken {
                token: (start, token, end),
            } => Message::Parse(ParseError::ExtraToken {
                range: start..end,
                token: token.to_string(),
            }),
            User { error } => Message::Lexer(error),
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
