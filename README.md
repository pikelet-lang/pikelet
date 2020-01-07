# Pikelet!

![Pikelet Mascot][pikelet-mascot]

[pikelet-mascot]: assets/pikelet.png

[![Build Status][travis-badge]][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/pikelet-lang/pikelet.svg?branch=master
[travis-url]: https://travis-ci.org/pikelet-lang/pikelet
[gitter-badge]: https://badges.gitter.im/pikelet-lang/pikelet.svg
[gitter-lobby]: https://gitter.im/pikelet-lang/Lobby

Pikelet is a small, functional, dependently typed programming language.

Dependent types allow us to do a bunch of really interesting things, like using
records for modules, declaring the length of arrays at the type level, and
much more — many of great utility for low-level and high-level code alike!

We hope to one day grow Pikelet into fully-fledged systems programming language,
with support for unboxed data types, control over memory layout and allocation
strategy, linear types, and a flexible phase distinction and support for calling
other languages. At the moment however we've only implemented a type checker and
very slow  interpreter, so don't get your hopes up too much yet! There's still a
whole lot to do before it is even remotely useful to anyone! 😅

## Roadmap

Our main aim is to start off with a simple configuration language, like
[Dhall][dhall]. From there we will progressively add features to gain more
flexibility. We want to provide a textual syntax up-front, but we should aim to
keep the core language reasonably decoupled from this, allowing us to provide
support for [projectional editing][structure-editor-wikipedia] in the future.

[dhall]: https://dhall-lang.org/
[structure-editor-wikipedia]: https://en.wikipedia.org/wiki/Structure_editor

You can read more about what we hope to achieve in [_Pondering the next version of Pikelet_][next-pikelet].

[next-pikelet]: https://gist.github.com/brendanzab/eba7015e6345abe79a57a704091820bb/.

<details>
  <summary>Full Roadmap</summary>

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
  - [x] Record field lookups
  - [ ] Import expressions
  - [ ] Function terms
  - [x] Non-dependent function types
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
- [ ] Pretty diagnostic reporting

### Rust marshalling

- [x] Mashalling traits
- [ ] Improved error messages
- [ ] Nicer marshalling API
- [ ] Derive macro for generating marshalling trait implementations
- [ ] More efficient, visitor based marshalling

</details>
