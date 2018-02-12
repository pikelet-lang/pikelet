//! Reporting of diagnostic messages

use std::fmt;
use termcolor::Color;

use pos::Span;

/// A severity level for diagnostic messages
#[derive(Copy, PartialEq, Clone, Hash, Debug)]
pub enum Level {
    /// Internal compiler errors - these are bugs!
    Bug,
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_str().fmt(f)
    }
}

impl Level {
    /// Return the termcolor to use when rendering messages of this diagnostic level
    pub fn color(self) -> Color {
        match self {
            Level::Bug | Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Note => Color::Green,
            Level::Help => Color::Cyan,
        }
    }

    /// A string that explains this diagnostic level
    pub fn to_str(self) -> &'static str {
        match self {
            Level::Bug => "error: internal compiler error",
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
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
