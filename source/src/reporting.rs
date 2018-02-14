//! Reporting of diagnostic messages

use std::fmt;
use termcolor::Color;

use pos::Span;

/// A severity level for diagnostic messages
#[derive(Copy, PartialEq, Clone, Hash, Debug)]
pub enum Severity {
    /// Internal errors - these are bugs!
    Bug,
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A help message.
    Help,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_str().fmt(f)
    }
}

impl Severity {
    /// Return the termcolor to use when rendering messages of this diagnostic severity
    pub fn color(self) -> Color {
        match self {
            Severity::Bug | Severity::Error => Color::Red,
            Severity::Warning => Color::Yellow,
            Severity::Note => Color::Green,
            Severity::Help => Color::Cyan,
        }
    }

    /// A string that explains this diagnostic severity
    pub fn to_str(self) -> &'static str {
        match self {
            Severity::Bug => "error: internal compiler error",
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
            Severity::Help => "help",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

impl LabelStyle {
    pub fn underline_char(self) -> char {
        match self {
            LabelStyle::Primary => '^',
            LabelStyle::Secondary => '-',
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpanLabel {
    /// The span we are going to include in the final snippet.
    pub span: Span,
    /// The style of label
    pub style: LabelStyle,
    /// What label should we attach to this span (if any)?
    pub label: Option<String>,
}

/// Represents a diagnostic message and associated child messages.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    severity: Severity,
    message: String,
    span: Option<Span>,
    children: Vec<Diagnostic>,
}

impl Diagnostic {
    /// Create a new diagnostic with the given `severity` and `message`.
    pub fn new<T: Into<String>>(severity: Severity, message: T) -> Diagnostic {
        Diagnostic {
            severity: severity,
            message: message.into(),
            span: None,
            children: vec![],
        }
    }

    /// Create a new diagnostic with the given `severity` and `message` pointing to
    /// the given `span`.
    pub fn spanned<T: Into<String>>(span: Span, severity: Severity, message: T) -> Diagnostic {
        Diagnostic {
            severity: severity,
            message: message.into(),
            span: Some(span),
            children: vec![],
        }
    }

    /// Returns the diagnostic `severity` for `self`.
    pub fn severity(&self) -> Severity {
        self.severity
    }

    /// Returns the diagnostic `message` for `self`.
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Returns the diagnostic `span` for `self`.
    pub fn span(&self) -> Option<Span> {
        self.span
    }

    /// Returns the child diagnostics for `self`.
    pub fn children(&self) -> &[Diagnostic] {
        &self.children
    }
}
