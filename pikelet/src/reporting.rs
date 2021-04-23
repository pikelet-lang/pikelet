//! Reporting diagnostic messages.

use codespan_reporting::diagnostic::{Diagnostic, Label};
use pretty::DocAllocator;
use std::sync::Arc;

use crate::lang::{core, surface, FileId, Location};
use crate::literal;

/// Global diagnostic messages
#[derive(Clone, Debug)]
pub enum Message {
    /// Errors produced during lexing.
    Lexer(LexerError),
    /// Errors produced during parsing.
    Parse(ParseError),
    /// Messages produced when parsing literals.
    LiteralParse(LiteralParseMessage),
    /// Messages produced from [`lang::core::typing`].
    ///
    /// [`lang::core::typing`]: crate::lang::core::typing
    CoreTyping(CoreTypingMessage),
    /// Messages produced from [`pass::surface_to_core`].
    ///
    /// [`pass::surface_to_core`]: crate::pass::surface_to_core
    SurfaceToCore(SurfaceToCoreMessage),
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

impl From<LiteralParseMessage> for Message {
    fn from(message: LiteralParseMessage) -> Self {
        Message::LiteralParse(message)
    }
}

impl From<CoreTypingMessage> for Message {
    fn from(message: CoreTypingMessage) -> Self {
        Message::CoreTyping(message)
    }
}

impl From<SurfaceToCoreMessage> for Message {
    fn from(message: SurfaceToCoreMessage) -> Self {
        Message::SurfaceToCore(message)
    }
}

impl Message {
    pub fn from_lalrpop<T: std::fmt::Display>(
        file_id: FileId,
        error: lalrpop_util::ParseError<usize, T, LexerError>,
    ) -> Message {
        use lalrpop_util::ParseError::*;

        match error {
            InvalidToken { location } => Message::from(LexerError::InvalidToken {
                location: Location::file_range(file_id, location..location),
            }),
            UnrecognizedEOF { location, expected } => Message::from(ParseError::UnrecognizedEof {
                location: Location::file_range(file_id, location..location),
                expected,
            }),
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Message::from(ParseError::UnrecognizedToken {
                location: Location::file_range(file_id, start..end),
                token: token.to_string(),
                expected,
            }),
            ExtraToken {
                token: (start, token, end),
            } => Message::from(ParseError::ExtraToken {
                location: Location::file_range(file_id, start..end),
                token: token.to_string(),
            }),
            User { error } => Message::from(error),
        }
    }

    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::Lexer(error) => error.to_diagnostic(),
            Message::Parse(error) => error.to_diagnostic(),
            Message::LiteralParse(message) => message.to_diagnostic(),
            Message::CoreTyping(message) => message.to_diagnostic(pretty_alloc),
            Message::SurfaceToCore(message) => message.to_diagnostic(pretty_alloc),
        }
    }
}

/// Lexer errors
#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidToken { location: Location },
}

impl LexerError {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            LexerError::InvalidToken { location } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(option_to_vec(primary(location))),
        }
    }
}

/// Parse errors
#[derive(Clone, Debug)]
pub enum ParseError {
    UnrecognizedEof {
        location: Location,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        location: Location,
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        location: Location,
        token: String,
    },
}

impl ParseError {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            ParseError::UnrecognizedEof { location, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(option_to_vec(
                    primary(location).map(|label| label.with_message("unexpected end of file")),
                ))
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseError::UnrecognizedToken {
                location,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(option_to_vec(
                    primary(location).map(|label| label.with_message("unexpected token")),
                ))
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseError::ExtraToken { location, token } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(option_to_vec(
                    primary(location).map(|label| label.with_message("extra token")),
                )),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LiteralParseMessage {
    ExpectedRadixOrDecimalDigit(Location),
    ExpectedStartOfNumericLiteral(Location),
    NegativeUnsignedInteger(Location),
    ExpectedDigit(Location, literal::Base),
    ExpectedDigitOrSeparator(Location, literal::Base),
    ExpectedDigitSeparatorOrExp(Location, literal::Base),
    ExpectedDigitSeparatorFracOrExp(Location, literal::Base),
    FloatLiteralExponentNotSupported(Location),
    UnsupportedFloatLiteralBase(Location, literal::Base),
    LiteralOutOfRange(Location),
    OverlongCharLiteral(Location),
    EmptyCharLiteral(Location),
    OversizedUnicodeEscapeCode(Location),
    EmptyUnicodeEscapeCode(Location),
    OverlongUnicodeEscapeCode(Location),
    InvalidUnicodeEscapeCode(Location),
    InvalidUnicodeEscape(Location),
    OversizedAsciiEscapeCode(Location),
    InvalidAsciiEscape(Location),
    UnknownEscapeSequence(Location),
    InvalidToken(Location),
    ExpectedEndOfLiteral(Location),
    UnexpectedEndOfLiteral(Location),
}

impl LiteralParseMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            LiteralParseMessage::ExpectedRadixOrDecimalDigit(location) => Diagnostic::error()
                .with_message("expected a radix or decimal digit")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedStartOfNumericLiteral(location) => Diagnostic::error()
                .with_message("expected the start of a numeric literal")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::NegativeUnsignedInteger(location) => Diagnostic::error()
                .with_message("unsigned integer literals cannot be negative")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedDigit(location, base) => Diagnostic::error()
                .with_message(format!("expected a base {} digit", base.to_u8()))
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedDigitOrSeparator(location, base) => Diagnostic::error()
                .with_message(format!(
                    "expected a base {} digit or digit separator",
                    base.to_u8(),
                ))
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedDigitSeparatorOrExp(location, base) => Diagnostic::error()
                .with_message(format!(
                    "expected a base {} digit, digit separator, or exponent",
                    base.to_u8(),
                ))
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedDigitSeparatorFracOrExp(location, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "expected a base {} digit, digit separator, period, or exponent",
                        base.to_u8(),
                    ))
                    .with_labels(option_to_vec(primary(location)))
            }
            LiteralParseMessage::FloatLiteralExponentNotSupported(location) => Diagnostic::error()
                .with_message("exponents are not yet supported for float literals")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::UnsupportedFloatLiteralBase(location, base) => Diagnostic::error()
                .with_message(format!(
                    "base {} float literals are not yet supported",
                    base.to_u8(),
                ))
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec![
                    "only base 10 float literals are currently supported".to_owned()
                ]),
            LiteralParseMessage::LiteralOutOfRange(location) => Diagnostic::error()
                .with_message("literal out of range")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::OverlongCharLiteral(location) => Diagnostic::error()
                .with_message("too many codepoints in character literal")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec![
                    "character literals may only contain one codepoint".to_owned()
                ]),
            LiteralParseMessage::EmptyCharLiteral(location) => Diagnostic::error()
                .with_message("empty character literal")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec!["character literals must not be empty".to_owned()]),
            LiteralParseMessage::OversizedUnicodeEscapeCode(location) => Diagnostic::error()
                .with_message("unicode escape code exceeds maximum allowed range")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec![format!("must be at most {:X} ", literal::MAX_UNICODE)]),
            LiteralParseMessage::EmptyUnicodeEscapeCode(location) => Diagnostic::error()
                .with_message("empty unicode character code")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec!["must contain at least one hex digit".to_owned()]),
            LiteralParseMessage::OverlongUnicodeEscapeCode(location) => Diagnostic::error()
                .with_message("too many digits in unicode character code")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec!["must contain at most six hex digits".to_owned()]),
            LiteralParseMessage::InvalidUnicodeEscapeCode(location) => Diagnostic::error()
                .with_message("invalid unicode escape code")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec!["must contain only hex digits".to_owned()]),
            LiteralParseMessage::InvalidUnicodeEscape(location) => Diagnostic::error()
                .with_message("invalid unicode escape sequence")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec![
                    "must be followed with a braced sequence of hex digits".to_owned(),
                    "for example: `\\u{..}`".to_owned(),
                ]),
            LiteralParseMessage::OversizedAsciiEscapeCode(location) => Diagnostic::error()
                .with_message("ACII escape code exceeds maximum allowed range")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec![format!("must be at most {:X} ", literal::MAX_ASCII)]),
            LiteralParseMessage::InvalidAsciiEscape(location) => Diagnostic::error()
                .with_message("invalid ASCII escape")
                .with_labels(option_to_vec(primary(location)))
                .with_notes(vec!["must contain exactly two hex digits ".to_owned()]),
            LiteralParseMessage::UnknownEscapeSequence(location) => Diagnostic::error()
                .with_message("unknown escape sequence")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::InvalidToken(location) => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::ExpectedEndOfLiteral(location) => Diagnostic::error()
                .with_message("expected end of literal")
                .with_labels(option_to_vec(primary(location))),
            LiteralParseMessage::UnexpectedEndOfLiteral(location) => Diagnostic::error()
                .with_message("unexpected end of literal")
                .with_labels(option_to_vec(primary(location))),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AmbiguousTerm {
    NumberLiteral,
    Sequence,
    FunctionTerm,
    RecordTerm,
}

impl AmbiguousTerm {
    fn description(&self) -> &'static str {
        match self {
            AmbiguousTerm::NumberLiteral => "numeric literal",
            AmbiguousTerm::Sequence => "sequence",
            AmbiguousTerm::FunctionTerm => "function term",
            AmbiguousTerm::RecordTerm => "record term",
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExpectedType<T> {
    Universe,
    Type(T),
}

/// Message produced from [lang::core::typing]
#[derive(Clone, Debug)]
pub enum CoreTypingMessage {
    UnboundGlobal {
        name: String,
    },
    UnboundVar,
    InvalidRecordType {
        duplicate_labels: Vec<String>,
    },
    UnexpectedRecordTermLabels {
        found_labels: Arc<[String]>,
        expected_labels: Arc<[String]>,
    },
    InvalidRecordTypeLabelCount,
    InvalidRecordTermLabelCount,
    LabelNotFound {
        expected_label: String,
        head_type: core::Term,
    },
    TooManyInputsInFunctionTerm,
    TooManyInputsInFunctionElim {
        head_type: core::Term,
    },
    UnexpectedArrayTerm {
        expected_type: core::Term,
    },
    UnexpectedListTerm {
        expected_type: core::Term,
    },
    AmbiguousTerm {
        term: AmbiguousTerm,
    },
    MismatchedTypes {
        found_type: core::Term,
        expected_type: ExpectedType<core::Term>,
    },
}

impl CoreTypingMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        use itertools::Itertools;

        use crate::pass::core_to_pretty;

        let to_doc = |term| core_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            CoreTypingMessage::UnboundGlobal { name } => {
                Diagnostic::bug().with_message(format!("unbound global variable `{}`", name))
            }
            CoreTypingMessage::UnboundVar => Diagnostic::bug().with_message("unbound variable"),
            CoreTypingMessage::InvalidRecordType { duplicate_labels } => Diagnostic::bug()
                .with_message("invalid record type")
                .with_notes(
                    duplicate_labels
                        .iter()
                        .map(|name| format!("label `{}` was used more than once", name))
                        .collect(),
                ),
            CoreTypingMessage::UnexpectedRecordTermLabels {
                expected_labels,
                found_labels,
            } => Diagnostic::bug()
                .with_message("unexpected record term labels")
                .with_notes(vec![
                    format!(
                        "expected labels: {}",
                        expected_labels
                            .iter()
                            .map(|label| format!("`{}`", label))
                            .format(", "),
                    ),
                    format!(
                        "expected labels: {}",
                        found_labels
                            .iter()
                            .map(|label| format!("`{}`", label))
                            .format(", "),
                    ),
                ]),
            CoreTypingMessage::InvalidRecordTypeLabelCount => Diagnostic::bug()
                .with_message("invalid record type")
                .with_notes(vec![
                    "number of record labels does not match the number of types".to_owned(),
                ]),
            CoreTypingMessage::InvalidRecordTermLabelCount => Diagnostic::bug()
                .with_message("invalid record term")
                .with_notes(vec![
                    "number of record labels does not match the number of terms".to_owned(),
                ]),
            CoreTypingMessage::LabelNotFound {
                expected_label,
                head_type,
            } => Diagnostic::bug()
                .with_message(format!("label `{}` not found", expected_label))
                .with_notes(vec![format!(
                    "eliminating a term of type `{}`",
                    to_doc(head_type).pretty(std::usize::MAX),
                )]),
            CoreTypingMessage::TooManyInputsInFunctionTerm => {
                Diagnostic::bug().with_message("too many inputs in function term")
            }
            CoreTypingMessage::TooManyInputsInFunctionElim { head_type } => Diagnostic::bug()
                .with_message("too many inputs in function elimination")
                .with_notes(vec![format!(
                    "eliminating a term of type `{}`",
                    to_doc(head_type).pretty(std::usize::MAX),
                )]),
            CoreTypingMessage::UnexpectedArrayTerm { expected_type } => Diagnostic::bug()
                .with_message("unexpected array term")
                .with_notes(vec![format!(
                    "expected `{}`, found an array",
                    to_doc(&expected_type).pretty(std::usize::MAX),
                )]),
            CoreTypingMessage::UnexpectedListTerm { expected_type } => Diagnostic::bug()
                .with_message("unexpected list term")
                .with_notes(vec![format!(
                    "expected `{}`, found a list",
                    to_doc(&expected_type).pretty(std::usize::MAX),
                )]),
            CoreTypingMessage::AmbiguousTerm { term } => {
                Diagnostic::bug().with_message(format!("ambiguous {}", term.description(),))
            }
            CoreTypingMessage::MismatchedTypes {
                found_type,
                expected_type,
            } => Diagnostic::bug()
                .with_message("mismatched types")
                .with_notes(vec![match expected_type {
                    ExpectedType::Universe => format!(
                        "expected a type, found `{}`",
                        to_doc(&found_type).pretty(std::usize::MAX),
                    ),
                    ExpectedType::Type(expected_type) => format!(
                        "expected `{}`, found `{}`",
                        to_doc(&expected_type).pretty(std::usize::MAX),
                        to_doc(&found_type).pretty(std::usize::MAX),
                    ),
                }]),
        }
    }
}

/// Message produced from [pass::surface_to_core]
#[derive(Clone, Debug)]
pub enum SurfaceToCoreMessage {
    UnboundName {
        location: Location,
        name: String,
    },
    InvalidRecordType {
        duplicate_labels: Vec<(String, Location, Location)>,
    },
    InvalidRecordTerm {
        location: Location,
        missing_labels: Vec<String>,
        unexpected_labels: Vec<Location>,
    },
    LabelNotFound {
        head_location: Location,
        label_location: Location,
        expected_label: String,
        head_type: surface::Term,
    },
    TooManyInputsInFunctionTerm {
        unexpected_inputs: Vec<Location>,
    },
    TooManyInputsInFunctionElim {
        head_location: Location,
        head_type: surface::Term,
        unexpected_input_terms: Vec<Location>,
    },
    NoLiteralConversion {
        location: Location,
        expected_type: surface::Term,
    },
    MismatchedSequenceLength {
        location: Location,
        found_len: usize,
        expected_len: surface::Term,
    },
    NoSequenceConversion {
        location: Location,
        expected_type: surface::Term,
    },
    AmbiguousTerm {
        location: Location,
        term: AmbiguousTerm,
    },
    MismatchedTypes {
        location: Location,
        found_type: surface::Term,
        expected_type: ExpectedType<surface::Term>,
    },
}

impl SurfaceToCoreMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        use itertools::Itertools;

        use crate::pass::surface_to_pretty;

        let to_doc = |term| surface_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            SurfaceToCoreMessage::UnboundName { location, name } => Diagnostic::error()
                .with_message(format!("cannot find `{}` in this scope", name))
                // TODO: name suggestions?
                .with_labels(option_to_vec(
                    primary(location).map(|label| label.with_message("not found in this scope")),
                )),

            SurfaceToCoreMessage::InvalidRecordType { duplicate_labels } => Diagnostic::error()
                .with_message("invalid record type")
                .with_labels({
                    let mut labels = Vec::with_capacity(duplicate_labels.len() * 2);

                    for (label_name, label_location1, label_location2) in duplicate_labels {
                        labels.extend(secondary(label_location1).map(|label| {
                            label.with_message(format!("first use of `{}`", label_name))
                        }));
                        labels
                            .extend(primary(label_location2).map(|label| {
                                label.with_message("entry label used more than once")
                            }));
                    }

                    labels
                }),

            SurfaceToCoreMessage::InvalidRecordTerm {
                location,
                missing_labels,
                unexpected_labels,
            } => Diagnostic::error()
                .with_message("invalid record term")
                .with_labels({
                    let mut labels = Vec::with_capacity(
                        unexpected_labels.len() + if missing_labels.is_empty() { 0 } else { 1 },
                    );

                    for label_location in unexpected_labels {
                        labels.extend(
                            primary(label_location)
                                .map(|label| label.with_message("unexpected entry label")),
                        );
                    }

                    if !missing_labels.is_empty() {
                        labels.extend(primary(location).map(|label| {
                            label.with_message(format!(
                                "missing the labels {} in this record term",
                                missing_labels
                                    .iter()
                                    // TODO: reduce string allocations
                                    .map(|label| format!("`{}`", label))
                                    .format(", "),
                            ))
                        }));
                    }

                    labels
                }),

            SurfaceToCoreMessage::LabelNotFound {
                head_location,
                label_location,
                expected_label,
                head_type,
            } => Diagnostic::error()
                .with_message(format!(
                    "no entry with label `{}` in type `{}`",
                    expected_label,
                    to_doc(&head_type).pretty(std::usize::MAX),
                ))
                .with_labels(
                    primary(label_location)
                        .map(|label| label.with_message("unknown entry label"))
                        .into_iter()
                        .chain(secondary(head_location).map(|label| {
                            label.with_message(format!(
                                "the type here is `{}`",
                                to_doc(&head_type).pretty(std::usize::MAX),
                            ))
                        }))
                        .collect(),
                ),

            SurfaceToCoreMessage::TooManyInputsInFunctionTerm { unexpected_inputs } => {
                Diagnostic::error()
                    .with_message("too many inputs given for function term")
                    .with_labels(
                        unexpected_inputs
                            .iter()
                            .flat_map(|input_location| {
                                primary(input_location)
                                    .map(|label| label.with_message("unexpected input"))
                            })
                            .collect(),
                    )
            }

            SurfaceToCoreMessage::TooManyInputsInFunctionElim {
                head_location,
                head_type,
                unexpected_input_terms,
            } => Diagnostic::error()
                .with_message("term was applied to too many inputs")
                .with_labels(
                    primary(head_location)
                        .map(|label| {
                            label.with_message(format!(
                                // TODO: multi-line?
                                "expected a function, found `{}`",
                                to_doc(&head_type).pretty(std::usize::MAX),
                            ))
                        })
                        .into_iter()
                        .chain(unexpected_input_terms.iter().flat_map(|input_location| {
                            primary(input_location)
                                .map(|label| label.with_message("unexpected input".to_owned()))
                        }))
                        .collect(),
                ),

            SurfaceToCoreMessage::NoLiteralConversion {
                location,
                expected_type,
            } => Diagnostic::error()
                .with_message("no known literal conversion")
                .with_labels(option_to_vec(primary(location).map(|label| {
                    label.with_message(format!(
                        // TODO: multi-line?
                        "expected `{}`, found a literal",
                        to_doc(&expected_type).pretty(std::usize::MAX),
                    ))
                }))),

            SurfaceToCoreMessage::MismatchedSequenceLength {
                location,
                found_len,
                expected_len,
            } => Diagnostic::error()
                .with_message("mismatched sequence length")
                .with_labels(option_to_vec(primary(location).map(|label| {
                    label.with_message(format!(
                        // TODO: multi-line?
                        "expected `{}` entries, found `{}` entries",
                        to_doc(&expected_len).pretty(std::usize::MAX),
                        found_len,
                    ))
                }))),

            SurfaceToCoreMessage::NoSequenceConversion {
                location,
                expected_type,
            } => Diagnostic::error()
                .with_message("no known sequence conversion")
                .with_labels(option_to_vec(primary(location).map(|label| {
                    label.with_message(format!(
                        // TODO: multi-line?
                        "expected `{}`, found a sequence",
                        to_doc(&expected_type).pretty(std::usize::MAX),
                    ))
                }))),

            SurfaceToCoreMessage::AmbiguousTerm { location, term } => Diagnostic::error()
                .with_message(format!("ambiguous {}", term.description()))
                .with_labels(option_to_vec(
                    primary(location).map(|label| label.with_message("type annotations needed")),
                )),

            SurfaceToCoreMessage::MismatchedTypes {
                location,
                found_type,
                expected_type,
            } => Diagnostic::error()
                .with_message("mismatched types")
                .with_labels(option_to_vec(primary(location).map(|label| {
                    label.with_message(match expected_type {
                        ExpectedType::Universe => format!(
                            // TODO: multi-line?
                            "expected a type, found `{}`",
                            to_doc(&found_type).pretty(std::usize::MAX),
                        ),
                        ExpectedType::Type(expected_type) => format!(
                            // TODO: multi-line?
                            "expected `{}`, found `{}`",
                            to_doc(&expected_type).pretty(std::usize::MAX),
                            to_doc(&found_type).pretty(std::usize::MAX),
                        ),
                    })
                }))),
        }
    }
}

/// Create a new label with a style of [`LabelStyle::Primary`].
///
/// [`LabelStyle::Primary`]: LabelStyle::Primary
fn primary(location: &Location) -> Option<Label<FileId>> {
    match location {
        Location::Generated => None,
        Location::FileRange(file_id, range) => Some(Label::primary(*file_id, *range)),
    }
}

/// Create a new label with a style of [`LabelStyle::Secondary`].
///
/// [`LabelStyle::Secondary`]: LabelStyle::Secondary
fn secondary(location: &Location) -> Option<Label<FileId>> {
    match location {
        Location::Generated => None,
        Location::FileRange(file_id, range) => Some(Label::secondary(*file_id, *range)),
    }
}

fn option_to_vec<T>(option: Option<T>) -> Vec<T> {
    match option {
        None => Vec::new(),
        Some(elem) => vec![elem],
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
