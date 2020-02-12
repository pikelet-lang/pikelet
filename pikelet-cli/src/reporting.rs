use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

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
            Message::MaximumUniverseLevelReached { range } => Snippet {
                title: Some(Annotation {
                    label: Some("maximum universe level reached".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        label: "overflowing universe level".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::UnboundName { range, name } => Snippet {
                title: Some(Annotation {
                    label: Some(format!("cannot find `{}` in this scope", name)),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        // TODO: name suggestions?
                        label: "not found in this scope".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::InvalidRecordType { duplicate_names } => Snippet {
                title: Some(Annotation {
                    label: Some("invalid record type".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: {
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
                    },
                }],
                footer: vec![],
            },

            Message::InvalidRecordTerm {
                range,
                duplicate_names,
                missing_names,
                unexpected_names,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("invalid record term".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: {
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
                    },
                }],
                footer: vec![],
            },

            Message::EntryNameNotFound {
                head_range,
                name_range,
                expected_field_name,
                head_type,
            } => Snippet {
                title: Some(Annotation {
                    label: Some(format!(
                        "no entry named `{}` in type `{}`",
                        expected_field_name,
                        to_doc(&head_type).pretty(std::usize::MAX),
                    )),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![
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
                }],
                footer: vec![],
            },

            Message::TooManyParameters {
                unexpected_parameters,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("too many parameters given for function term".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: unexpected_parameters
                        .iter()
                        .map(|parameter_range| SourceAnnotation {
                            label: "unexpected parameter".to_owned(),
                            annotation_type: AnnotationType::Note,
                            range: (parameter_range.start, parameter_range.end),
                        })
                        .collect(),
                }],
                footer: vec![],
            },

            Message::TooManyArguments {
                head_range,
                head_type,
                unexpected_arguments,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("term was applied to too many arguments".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: std::iter::once(SourceAnnotation {
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
                }],
                footer: vec![],
            },

            Message::InvalidLiteral { range, literal } => Snippet {
                title: Some(Annotation {
                    // TODO: supply expected type information
                    label: Some(format!(
                        "invalid {} literal",
                        match literal {
                            InvalidLiteral::Char => "character",
                            InvalidLiteral::String => "string",
                            InvalidLiteral::Number => "numeric",
                        },
                    )),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        label: "failed to parse literal".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::NoLiteralConversion {
                range,
                expected_type,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("no known literal conversion".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}`, found a literal",
                            to_doc(&expected_type).pretty(std::usize::MAX),
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::MismatchedSequenceLength {
                range,
                found_len,
                expected_len,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("mismatched sequence length".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}` entries, found `{}` entries",
                            to_doc(&expected_len).pretty(std::usize::MAX),
                            found_len,
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::NoSequenceConversion {
                range,
                expected_type,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("no known sequence conversion".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        // TODO: multi-line?
                        label: format!(
                            "expected `{}`, found a sequence",
                            to_doc(&expected_type).pretty(std::usize::MAX),
                        ),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::AmbiguousTerm { range, term } => Snippet {
                title: Some(Annotation {
                    label: Some(format!(
                        "ambiguous {}",
                        match term {
                            AmbiguousTerm::NumberLiteral => "numeric literal",
                            AmbiguousTerm::Sequence => "sequence",
                            AmbiguousTerm::FunctionTerm => "function term",
                            AmbiguousTerm::RecordTerm => "record term",
                        },
                    )),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        label: "type annotations needed".to_owned(),
                        annotation_type: AnnotationType::Error,
                        range: (range.start, range.end),
                    }],
                }],
                footer: vec![],
            },

            Message::MismatchedTypes {
                range,
                found_type,
                expected_type,
            } => Snippet {
                title: Some(Annotation {
                    label: Some("mismatched types".to_owned()),
                    id: None,
                    annotation_type: AnnotationType::Error,
                }),
                slices: vec![Slice {
                    source: source.to_owned(),
                    line_start: 0,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
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
                }],
                footer: vec![],
            },
        }
    }
}
