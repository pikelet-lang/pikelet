use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

fn snippet(title: Annotation, slices: Vec<Slice>) -> Snippet {
    Snippet {
        title: Some(title),
        slices,
        footer: vec![],
    }
}

fn annotation(label: impl Into<String>, annotation_type: AnnotationType) -> Annotation {
    Annotation {
        label: Some(label.into()),
        id: None,
        annotation_type,
    }
}

fn slice(source: &str, annotations: Vec<SourceAnnotation>) -> Slice {
    Slice {
        source: source.to_owned(),
        line_start: 0,
        origin: None,
        fold: false,
        annotations,
    }
}

pub trait ToSnippet {
    fn to_snippet(&self, source: &str) -> Snippet;
}

impl ToSnippet for pikelet::surface::projections::core::Message {
    fn to_snippet(&self, source: &str) -> Snippet {
        use itertools::Itertools;
        use pikelet::surface;
        use pikelet::surface::projections::core::{
            AmbiguousTerm, ExpectedType, InvalidLiteral, Message,
        };

        let pretty_alloc = pretty::BoxAllocator;
        let to_doc = |term| surface::projections::pretty::pretty_term(&pretty_alloc, term).1;

        match self {
            Message::MaximumUniverseLevelReached { range } => snippet(
                annotation("maximum universe level reached", AnnotationType::Error),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        label: "overflowing universe level".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::UnboundName { range, name } => snippet(
                annotation(
                    format!("cannot find `{}` in this scope", name),
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        // TODO: name suggestions?
                        label: "not found in this scope".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::InvalidRecordType { duplicate_names } => snippet(
                annotation("invalid record type", AnnotationType::Error),
                vec![slice(source, {
                    let mut annotations = Vec::with_capacity(duplicate_names.len() * 2);

                    for (name, name_range1, name_range2) in duplicate_names {
                        annotations.push(SourceAnnotation {
                            label: format!("first use of `{}`", name),
                            annotation_type: AnnotationType::Note,
                            range: (name_range1.start, name_range1.end),
                        });
                        annotations.push(SourceAnnotation {
                            label: "entry name used more than once".to_owned(),
                            annotation_type: AnnotationType::Error,
                            range: (name_range2.start, name_range2.end),
                        });
                    }

                    annotations
                })],
            ),

            Message::InvalidRecordTerm {
                range,
                duplicate_names,
                missing_names,
                unexpected_names,
            } => snippet(
                annotation("invalid record term", AnnotationType::Error),
                vec![slice(source, {
                    let mut annotations = Vec::with_capacity(
                        duplicate_names.len() * 2
                            + unexpected_names.len()
                            + if missing_names.is_empty() { 0 } else { 1 },
                    );

                    for (name, name_range1, name_range2) in duplicate_names {
                        annotations.push(SourceAnnotation {
                            label: format!("first use of `{}`", name),
                            annotation_type: AnnotationType::Note,
                            range: (name_range1.start, name_range1.end),
                        });
                        annotations.push(SourceAnnotation {
                            label: "entry name used more than once".to_owned(),
                            annotation_type: AnnotationType::Error,
                            range: (name_range2.start, name_range2.end),
                        });
                    }

                    for (_, name_range) in unexpected_names {
                        annotations.push(SourceAnnotation {
                            label: "unexpected entry name".to_owned(),
                            annotation_type: AnnotationType::Error,
                            range: (name_range.start, name_range.end),
                        });
                    }

                    if !missing_names.is_empty() {
                        annotations.push(SourceAnnotation {
                            label: format!(
                                "missing the names {} in this record term",
                                missing_names
                                    .iter()
                                    // TODO: reduce string allocations
                                    .map(|name| format!("`{}`", name))
                                    .format(", "),
                            ),
                            annotation_type: AnnotationType::Error,
                            range: (range.start, range.end),
                        });
                    }

                    annotations
                })],
            ),

            Message::EntryNameNotFound {
                head_range,
                name_range,
                expected_field_name,
                head_type,
            } => snippet(
                annotation(
                    format!(
                        "no entry named `{}` in type `{}`",
                        expected_field_name,
                        to_doc(&head_type).pretty(std::usize::MAX),
                    ),
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    vec![
                        SourceAnnotation {
                            label: "unknown entry name".to_owned(),
                            annotation_type: AnnotationType::Error,
                            range: (name_range.start, name_range.end),
                        },
                        SourceAnnotation {
                            label: format!(
                                "the type here is `{}`",
                                to_doc(&head_type).pretty(std::usize::MAX),
                            ),
                            annotation_type: AnnotationType::Help,
                            range: (head_range.start, head_range.end),
                        },
                    ],
                )],
            ),

            Message::TooManyParameters {
                unexpected_parameters,
            } => snippet(
                annotation(
                    "too many parameters given for function term",
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    unexpected_parameters
                        .iter()
                        .map(|parameter_range| SourceAnnotation {
                            label: "unexpected parameter".to_owned(),
                            annotation_type: AnnotationType::Note,
                            range: (parameter_range.start, parameter_range.end),
                        })
                        .collect(),
                )],
            ),

            Message::TooManyArguments {
                head_range,
                head_type,
                unexpected_arguments,
            } => snippet(
                annotation(
                    "term was applied to too many arguments",
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    std::iter::once(SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected a function, found `{}`",
                            to_doc(&head_type).pretty(std::usize::MAX),
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (head_range.start, head_range.end),
                    })
                    .chain(
                        unexpected_arguments
                            .iter()
                            .map(|argument_range| SourceAnnotation {
                                label: "unexpected argument".to_owned(),
                                annotation_type: AnnotationType::Note,
                                range: (argument_range.start, argument_range.end),
                            }),
                    )
                    .collect(),
                )],
            ),

            Message::InvalidLiteral { range, literal } => snippet(
                annotation(
                    // TODO: supply expected type information
                    format!(
                        "invalid {} literal",
                        match literal {
                            InvalidLiteral::Char => "character",
                            InvalidLiteral::String => "string",
                            InvalidLiteral::Number => "numeric",
                        },
                    ),
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        label: "failed to parse literal".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::NoLiteralConversion {
                range,
                expected_type,
            } => snippet(
                annotation("no known literal conversion", AnnotationType::Error),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}`, found a literal",
                            to_doc(&expected_type).pretty(std::usize::MAX),
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::MismatchedSequenceLength {
                range,
                found_len,
                expected_len,
            } => snippet(
                annotation("mismatched sequence length", AnnotationType::Error),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}` entries, found `{}` entries",
                            to_doc(&expected_len).pretty(std::usize::MAX),
                            found_len,
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::NoSequenceConversion {
                range,
                expected_type,
            } => snippet(
                annotation("no known sequence conversion", AnnotationType::Error),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}`, found a sequence",
                            to_doc(&expected_type).pretty(std::usize::MAX),
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::AmbiguousTerm { range, term } => snippet(
                annotation(
                    format!(
                        "ambiguous {}",
                        match term {
                            AmbiguousTerm::NumberLiteral => "numeric literal",
                            AmbiguousTerm::Sequence => "sequence",
                            AmbiguousTerm::FunctionTerm => "function term",
                            AmbiguousTerm::RecordTerm => "record term",
                        },
                    ),
                    AnnotationType::Error,
                ),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        label: "type annotations needed".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),

            Message::MismatchedTypes {
                range,
                found_type,
                expected_type,
            } => snippet(
                annotation("mismatched types", AnnotationType::Error),
                vec![slice(
                    source,
                    vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: match expected_type {
                            ExpectedType::Universe => format!(
                                "expected a type, found `{}`",
                                to_doc(&found_type).pretty(std::usize::MAX),
                            ),
                            ExpectedType::Type(expected_type) => format!(
                                "expected `{}`, found `{}`",
                                to_doc(&expected_type).pretty(std::usize::MAX),
                                to_doc(&found_type).pretty(std::usize::MAX),
                            ),
                        },
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                )],
            ),
        }
    }
}
