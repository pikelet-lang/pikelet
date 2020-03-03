---
id: roadmap
title: Roadmap
sidebar_label: Roadmap
keywords:
  - docs
  - pikelet
---

Our main aim is to start off with a simple configuration language, like
[Dhall][dhall]. From there we will progressively add features to gain more
flexibility. We want to provide a textual syntax up-front, but we should aim to
keep the core language reasonably decoupled from this, allowing us to provide
support for [projectional editing][structure-editor-wikipedia] in the future.

[dhall]: https://dhall-lang.org/
[structure-editor-wikipedia]: https://en.wikipedia.org/wiki/Structure_editor

You can read more about what we hope to achieve in [_Pondering the next version of Pikelet_][next-pikelet].

[next-pikelet]: https://gist.github.com/brendanzab/eba7015e6345abe79a57a704091820bb/.

### Language

- Basic config language
  - [x] Comments
  - [x] Boolean literals/constants
  - [x] Integer literals/constants
  - [x] Float literals/constants
  - [x] Character literals/constants
  - [x] String literals/constants
  - [x] Record terms
  - [x] Non-dependent record types
  - [x] Dynamically sized arrays
  - [x] Fixed sized arrays
- Basic programming language
  - [ ] Improved literal parsing
  - [x] Annotated terms
  - [ ] Let expressions
  - [x] Record field lookups
  - [ ] Import expressions
  - [x] Function terms
  - [x] Non-dependent function types
  - [ ] Pattern matching
  - [ ] Recursive terms
  - [ ] Opaque (abstract) terms
  - [ ] Builtin terms
- Dependently typed language
  - [x] Dependent record types
  - [ ] Dependent function types
  - [ ] Equality types
  - [x] Universe levels
    - [x] Stratified
    - [x] Cumulative
    - [x] Lifting operator
    - [ ] Large types (`Typeω`)
  - [ ] Multi-stage programming
  - [ ] Quantitative type theory

### Projections

- [x] Surface &rarr; Pretty
- [x] Surface &rarr; Core
- [x] Core &rarr; Pretty
- [x] Core &rarr; Value
- [ ] Core &rarr; Binary
- [ ] Core &rarr; Documentation
- [ ] Core &rarr; Cranelift
- [ ] Core &rarr; LLVM
- [ ] Value &rarr; JSON/YAML/TOML

### Tooling

- [x] REPL
- [ ] Package manager
- [ ] Auto-formatter for surface language
- [ ] Structured editor

### Testing

- [x] Language samples
- [ ] Feature tests
- [ ] Property based tests

### Diagnostics

- [x] Basic error enum
- [x] Error recovery
- [x] Pretty diagnostic reporting

### Rust marshalling

- [x] Mashalling traits
- [ ] Improved error messages
- [ ] Nicer marshalling API
- [ ] Derive macro for generating marshalling trait implementations
- [ ] More efficient, visitor based marshalling
