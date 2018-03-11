# Pikelet 🥞

[![Build Status](travis-badge)][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/brendanzab/pikelet.svg?branch=master
[travis-url]: https://travis-ci.org/brendanzab/pikelet
[gitter-badge]: https://badges.gitter.im/gluon-lang/gluon.svg
[gitter-lobby]: https://gitter.im/pikelet-lang/Lobby

This is an implementation of a small [dependently typed][dependent-type-wikipedia]
lambda calculus in Rust. This project is intended as an explaratory exercise,
and to serve as a basis for more fully featured dependently typed languages.

Thanks to the hard work of our academics, it has become much easier for the
average developer to try their hand at building dependent type systems. Alas the
materials required to tackle this are scattered throughout the literature, and
can be time consuming to track down. Hopefully this project makes it easier to
see how this can be done in Rust!

We take a [bidirectional approach][bidirectional-typing-paper] to building the
type system, similar to the one descibed in the excellent
[LambdaPi paper][lambdapi-site]. This allows us to provide a good amount of
local type inference while still maintaining a simple, easy to understand type
checking algorithm.

[dependent-type-wikipedia]: https://en.wikipedia.org/wiki/Dependent_type

## Roadmap

There's still a decent amount of work I want to do to Pikelet before I share it
around more widely:

- syntax
  - [ ] layout based lexer
  - [ ] `let` and `where` bindings
- type system
  - [ ] dependent records
  - [ ] universe hierarchy
    - [x] stratified - _this is actually very annoying to use_
    - [ ] cumulative?
      - [twitter thread][universes-twitter-ramble]
      - [blog post by Conor McBride][universes-pigworker-blog]
- more fully featured elaborator
  - [x] holes, ie. `_`
  - [ ] metavariables, ie. `?metavar`
  - [ ] implicit arguments, ie. `{a : Type}`
  - [ ] instance arguments, ie. `[eq : EQ a]`
- UX and polish
  - [ ] actually implement module system
  - [ ] pretty error messages
    - [x] diagnostic reporting crate - see [brendanzab/codespan][codespan]
    - [ ] underlined source code snippets
    - [ ] type diffs
  - [ ] prettier pretty printing
    - [ ] rename ugly genvars!
    - [ ] use non-dependant arrows when possible
- back-end
  - [ ] JIT/VM
  - [ ] compiler - to LLVM perhaps - see “Implementing and Optimizing a Simple,
        Dependently-Typed Language” [[PAPER][compiling-lambdapi-paper]]
- clean up and document internals
  - [ ] document challenges with name binding
  - [x] ensure substitution points match the typing judgements
  - [ ] fix typechecking at the module level
  - [ ] settle on a logging strategy for judgements
  - [x] automate name binding boilerplate to get it out of `syntax::core`
  - [ ] review `core::{Term, Value}`
    - [x] What about `CheckedTerm`s? `ElaboratedTerm`s?
    - [x] Why is there an `Option<Value>` on `Value::Lam`?
    - [ ] neutral/weak head normal forms
  - [ ] performance tuning, and profiling
  - [ ] property based testing - see “Effect-Driven QuickChecking of Compilers”
        [[VIDEO][quickchecking-compilers-video]]
        [[PAPER][quickchecking-compilers-paper]]

Future experiments into integrating effects, coeffects, worldly types, linear
types, graded types, cubical type theory, and observational type theory are
(sadly) most likely beyond the scope of Pikelet. Getting them all to play nicely
under one roof is still an open research problem. That said, Pikelet could serve
as a basis for experimenting with those ideas, perhaps with the goal of building
a new systems-oriented dependently typed language!

[codespan]: https://github.com/brendanzab/codespan
[universes-twitter-ramble]: https://twitter.com/brendanzab/status/962681577120587776
[universes-pigworker-blog]: https://pigworker.wordpress.com/2015/01/09/universe-hierarchies/
[compiling-lambdapi-paper]: http://publications.lib.chalmers.se/records/fulltext/124826.pdf
[quickchecking-compilers-video]: https://www.youtube.com/watch?v=_KrZzaShDew
[quickchecking-compilers-paper]: http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

## References

- Charguéraud, Arthur (2011). “The Locally Nameless Representation”.
  In _Journal of Automated Reasoning (JAR)_.
  [[SITE][ln-site]]
  [[PAPER][ln-paper]]
- Christiansen, David Raymond (2013). “Bidirectional Typing Rules: A Tutorial”.
  [[PAPER][bidirectional-typing-paper]]
- Löh, Andres, McBride, Conor and Swierstra, Wouter (2009). “A tutorial
  implementation of a dependently typed lambda calculus”.
  [[SITE][lambdapi-site]]
  [[PAPER][lambdapi-paper]]
- Norell, Ulf (2007). “Towards a practical programming language based on
  dependent type theory”.
  [[PAPER][agda-paper]]

[ln-site]: http://www.chargueraud.org/softs/ln/
[ln-paper]: http://www.chargueraud.org/research/2009/ln/main.pdf
[bidirectional-typing-paper]: http://www.davidchristiansen.dk/tutorials/bidirectional.pdf
[lambdapi-site]: https://www.andres-loeh.de/LambdaPi/
[lambdapi-paper]: https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
[agda-paper]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf

## What is a Pikelet?

A pikelet is an odd sort of small (often pre-made) pancake found in Australia
and New Zealand. Commonly sent in school lunches spread with jam and butter.
Handily it also has a name that includes 'pi' and 'let' as substrings! 😅

## Acknowledgments

[![YesLogic Logo][yeslogic-logo]][yeslogic]

This work was done with the generous support of [YesLogic][yeslogic].

[yeslogic]: http://yeslogic.com/
[yeslogic-logo]: assets/yeslogic-logo.png
