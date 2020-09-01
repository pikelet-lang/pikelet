use codespan_reporting::diagnostic::{Diagnostic, Label};
use pretty::DocAllocator;
use std::ops::Range;

use crate::lang::core;
use crate::pass::surface_to_core;

#[derive(Clone, Debug)]
pub enum Message {
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
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<()>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::Parse(error) => error.to_diagnostic(),
            Message::CoreTyping(message) => message.to_diagnostic(pretty_alloc),
            Message::SurfaceToCore(message) => message.to_diagnostic(pretty_alloc),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    InvalidToken {
        location: usize,
    },
    UnrecognizedEOF {
        location: usize,
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
    User {
        error: &'static str,
    },
}

impl ParseError {
    pub fn from_lalrpop<T: std::fmt::Display>(
        error: lalrpop_util::ParseError<usize, T, &'static str>,
    ) -> ParseError {
        use lalrpop_util::ParseError::*;

        match error {
            InvalidToken { location } => ParseError::InvalidToken { location },
            UnrecognizedEOF { location, expected } => {
                ParseError::UnrecognizedEOF { location, expected }
            }
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => ParseError::UnrecognizedToken {
                range: start..end,
                token: token.to_string(),
                expected,
            },
            ExtraToken {
                token: (start, token, end),
            } => ParseError::ExtraToken {
                range: start..end,
                token: token.to_string(),
            },
            User { error } => ParseError::User { error },
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic<()> {
        match self {
            ParseError::InvalidToken { location } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), *location..*location)]),
            ParseError::UnrecognizedEOF { location, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary((), *location..*location).with_message("unexpected end of file")
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
            ParseError::User { error } => Diagnostic::error().with_message(*error),
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
