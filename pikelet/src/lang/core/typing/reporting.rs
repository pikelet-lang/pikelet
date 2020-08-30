use codespan_reporting::diagnostic::Diagnostic;
use pretty::DocAllocator;

use crate::lang::core::Term;

#[derive(Clone, Debug)]
pub enum Message {
    MaximumUniverseLevelReached,
    UnboundGlobal {
        name: String,
    },
    UnboundLocal,
    InvalidRecordType {
        duplicate_labels: Vec<String>,
    },
    InvalidRecordTerm {
        missing_labels: Vec<String>,
        unexpected_labels: Vec<String>,
    },
    LabelNotFound {
        expected_label: String,
        head_type: Term,
    },
    TooManyInputsInFunctionTerm,
    TooManyInputsInFunctionElim {
        head_type: Term,
    },
    MismatchedSequenceLength {
        found_len: usize,
        expected_len: Term,
    },
    NoSequenceConversion {
        expected_type: Term,
    },
    AmbiguousTerm {
        term: AmbiguousTerm,
    },
    MismatchedTypes {
        found_type: Term,
        expected_type: ExpectedType,
    },
}

#[derive(Clone, Debug)]
pub enum AmbiguousTerm {
    Sequence,
    FunctionTerm,
    RecordTerm,
}

#[derive(Clone, Debug)]
pub enum ExpectedType {
    Universe,
    Type(Term),
}

impl Message {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<()>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        use itertools::Itertools;

        use crate::pass::core_to_pretty;

        let to_doc = |term| core_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            Message::MaximumUniverseLevelReached => {
                Diagnostic::bug().with_message("maximum universe level reached")
            }
            Message::UnboundGlobal { name } => {
                Diagnostic::bug().with_message(format!("unbound global variable `{}`", name))
            }
            Message::UnboundLocal => Diagnostic::bug().with_message("unbound local variable"),
            Message::InvalidRecordType { duplicate_labels } => Diagnostic::bug()
                .with_message("invalid record type")
                .with_notes(
                    duplicate_labels
                        .iter()
                        .map(|name| format!("label `{}` was used more than once", name))
                        .collect(),
                ),
            Message::InvalidRecordTerm {
                missing_labels,
                unexpected_labels,
            } => Diagnostic::bug()
                .with_message("invalid record term")
                .with_notes({
                    let mut notes = Vec::with_capacity(
                        unexpected_labels.len() + if missing_labels.is_empty() { 0 } else { 1 },
                    );

                    for label in unexpected_labels {
                        notes.push(format!("unexpected label `{}`", label));
                    }

                    if !missing_labels.is_empty() {
                        notes.push(format!(
                            "missing the labels {} in this record term",
                            missing_labels
                                .iter()
                                // TODO: reduce string allocations
                                .map(|label| format!("`{}`", label))
                                .format(", "),
                        ));
                    }

                    notes
                }),
            Message::LabelNotFound {
                expected_label,
                head_type,
            } => Diagnostic::bug()
                .with_message(format!("label `{}` not found", expected_label))
                .with_notes(vec![format!(
                    "eliminating a term of type `{}`",
                    to_doc(head_type).pretty(std::usize::MAX),
                )]),
            Message::TooManyInputsInFunctionTerm => {
                Diagnostic::bug().with_message("too many inputs in function term")
            }
            Message::TooManyInputsInFunctionElim { head_type } => Diagnostic::bug()
                .with_message("too many inputs in function elimination")
                .with_notes(vec![format!(
                    "eliminating a term of type `{}`",
                    to_doc(head_type).pretty(std::usize::MAX),
                )]),
            Message::MismatchedSequenceLength {
                found_len,
                expected_len,
            } => Diagnostic::bug()
                .with_message("mismatched sequence length")
                .with_notes(vec![format!(
                    "expected `{}` entries, found `{}` entries",
                    to_doc(&expected_len).pretty(std::usize::MAX),
                    found_len,
                )]),
            Message::NoSequenceConversion { expected_type } => Diagnostic::bug()
                .with_message("no known sequence conversion")
                .with_notes(vec![format!(
                    "expected `{}`, found a sequence",
                    to_doc(&expected_type).pretty(std::usize::MAX),
                )]),
            Message::AmbiguousTerm { term } => Diagnostic::bug().with_message(format!(
                "ambiguous {}",
                match term {
                    AmbiguousTerm::Sequence => "sequence",
                    AmbiguousTerm::FunctionTerm => "function term",
                    AmbiguousTerm::RecordTerm => "record term",
                },
            )),
            Message::MismatchedTypes {
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
