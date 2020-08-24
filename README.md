# Pikelet

A simple language.

This is planned to meet the demands of the 'next version of Pikelet', as
described in [_Pondering the next version of Pikelet_][next-pikelet].

[next-pikelet]: https://gist.github.com/brendanzab/eba7015e6345abe79a57a704091820bb/.

## Roadmap

Our main aim is to start off with a simple configuration language, like
[Dhall][dhall]. From there we will progressively add features to gain more
flexibility. We want to provide a textual syntax up-front, but we should aim to
keep the core language reasonably decoupled from this, allowing us to provide
support for [projectional editing][structure-editor-wikipedia] in the future.

[dhall]: https://dhall-lang.org/
[structure-editor-wikipedia]: https://en.wikipedia.org/wiki/Structure_editor

### Language

- Basic config language
  - [ ] Comments
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
  - [ ] Record field lookups
  - [ ] Import expressions
  - [ ] Function terms
  - [ ] Non-dependent function types
  - [ ] Pattern matching
  - [ ] Recursive terms
  - [ ] Opaque (abstract) terms
  - [ ] Builtin terms
- Dependently typed language
  - [ ] Dependent record types
  - [ ] Dependent function types
  - [ ] Equality types
  - [ ] Fixed-size arrays
  - [x] Universe levels
    - [x] Stratified
    - [x] Cumulative
    - [x] Lifting operator
    - [ ] Large types (Typeω)
  - [ ] Multi-stage programming
  - [ ] Quantitative type theory

### Projections

- [x] Surface -> Pretty
- [x] Surface -> Core
- [x] Core -> Pretty
- [x] Core -> Value
- [ ] Core -> Binary
- [ ] Core -> Documentation
- [ ] Core -> Cranelift
- [ ] Core -> LLVM
- [ ] Value -> JSON/YAML/TOML

### Tooling

- [ ] REPL
- [ ] Package manager
- [ ] Auto-formatter for surface language

### Testing

- [x] Language samples
- [ ] Feature tests
- [ ] Property based tests

### Diagnostics

- [x] Basic error enum
- [x] Error recovery
- [ ] Pretty diagnostic reporting

### Rust marshalling

- [x] Mashalling traits
- [ ] Improved error messages
- [ ] Nicer marshalling API
- [ ] Derive macro for generating marshalling trait implementations
- [ ] More efficient, visitor based marshalling

### Documentation

- [ ] Guide
- [ ] Specification
  - [ ] Surface
    - [ ] Syntax
    - [ ] Elaboration
  - [ ] Core
    - [ ] Syntax
    - [ ] Typing
    - [ ] Operational semantics
- [ ] References
  - [ ] Papers
  - [ ] Languages
