# Design

## Design goals

Pikelet should feel:

- Friendly
- Liberating
- Sympathetic

This is a rough list things that might be interesting to explore with Pikelet:

- Sympathetic to humans
  - Programs should look pretty, and read clearly
  - Friendly community
  - Helpful interactive tooling
  - Tight feedback loops
- Sympathetic to machines
  - Predictable performance
  - Predictable memory layout
  - Predictable optimizations
  - Zero (or close to zero) cost abstractions
  - Low level control
  - Minimal runtime
- Sympathetic to real-world realities
  - Gradual correctness (eg. examples -> generative tests -> solvers -> proofs)
  - Provide clearly marked escape hatches (learn from the lessons of [RustBelt](rust-belt)?)
  - Automatic upgrades of code - between language and library versions
  - Work for large scale systems
- Sympathetic to mathematical foundations
  - Simple core language that can be formalized and proven sound
  - Should allow for mathematically inspired patterns of code reuse
  - Allow newcomers to learn mathematical thinking gradually
  - Adaptable to future developments in type theory

It's unclear how many of these will be able to be met, and what priorities these
should have, so this list might change over time. Come and [chat with us][gitter-channel]
if you'd like to give your input and get involved!

[rust-belt]: https://plv.mpi-sws.org/rustbelt/
[gitter-channel]: https://gitter.im/pikelet-lang/Lobby

## Some key features of interest

- [Dependent types](https://en.wikipedia.org/wiki/Dependent_type)
- [Purely functional](https://en.wikipedia.org/wiki/Pure_function)
- [Strict evaluation](https://en.wikipedia.org/wiki/Eager_evaluation)
- Totality checking
- Dependent records as first class modules/type classes
- Non-uniform memory layout
- [Quantitative type theory](https://bentnib.org/quantitative-type-theory.pdf)
  for [erasure](https://en.wikipedia.org/wiki/Type_erasure) and
  [linear types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems)
- Interactive program development using holes

Some other features that may be trickier to integrate given the previous
features and design goals:

- [Effect systems/Algebraic Effects](https://en.wikipedia.org/wiki/Effect_system)
  - could make it easier to integrate async-io without needing to build it in
  - how do cubical type theory and observational type theory play into this?
  - how do we makes this *fast* for systems programming?
    - should compile down in a similar way to the equivalent procedural code in Rust or C
    - most systems cause lots of intemediate allocations or stack switching
- [Combined Effects/Coeffects](https://www.cs.kent.ac.uk/people/staff/dao7/publ/combining-effects-and-coeffects-icfp16.pdf)
  - allow for statically checked compilation configurations
  - could subsume quantitative type theory, implicit arguments, etc
  - not yet integrated into dependent types in the research literature
- Row polymorphism
  - no research on integrating these with dependent records and inductive datatypes
- Program composition via category theory
  - Challenging to integrate in the presence of strict evaluation
  - Similar problems to effect systems: we don't want to allocate intermediate
    data structures, instead we want to build up stack allocated state machines
    (like in Rust's future and iterator traits) to be executed later
- Optional garbage collection
- Alternatives to currying for function application
- First-class declarations (Levitation or Elaborator Reflection could be useful here)
- Explicit variable capture as a coeffect

## Inspiration

Some languages have been inspiring when building Pikelet. These include:

- [1ML](https://people.mpi-sws.org/~rossberg/1ml/)
- [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php)
- [ATS](http://www.ats-lang.org/)
- [Dhall](https://github.com/dhall-lang/)
- [Discus (formerly DDC)](http://www.discus-lang.org/)
- [Elm](http://elm-lang.org/)
- [F*](https://www.fstar-lang.org/)
- [Gluon](https://github.com/gluon-lang/gluon)
- [Granule](https://github.com/dorchard/granule/)
- [Idris](https://www.idris-lang.org/)
- [Ivory](https://ivorylang.org/ivory-introduction.html)
- [Koka](https://www.microsoft.com/en-us/research/project/koka/)
- [Lean](http://leanprover.github.io)
- [OCaml (Multicore)](https://github.com/ocamllabs/ocaml-multicore)
- [OCaml (Modular implicits)](https://github.com/ocamllabs/ocaml-modular-implicits)
- [Rust](http://rust-lang.org/)
- [Sixten](https://github.com/ollef/sixten)
- [Ur](http://www.impredicative.com/ur/)
