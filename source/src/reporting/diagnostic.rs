use pos::Span;

use reporting::Severity;

/// A style for the underline
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnderlineStyle {
    Primary,
    Secondary,
}

impl UnderlineStyle {
    pub fn underline_char(self) -> char {
        match self {
            UnderlineStyle::Primary => '^',
            UnderlineStyle::Secondary => '-',
        }
    }
}

/// Describes an underlined region of code associated with a diagnostic
#[derive(Clone, Debug)]
pub struct SpanLabel {
    /// The span we are going to include in the final snippet.
    pub span: Span,
    /// A label to provide some additional information for the underlined code.
    pub label: Option<String>,
    /// The style to use for the underline.
    pub style: UnderlineStyle,
}

/// Represents a diagnostic message and associated child messages.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The overall severity of the diagnostic
    pub severity: Severity,
    /// The main message associated with this diagnostic
    pub message: String,
    /// The labelled spans marking the regions of code that cause this
    /// diagnostic to be raised
    pub spans: Vec<SpanLabel>,
}
