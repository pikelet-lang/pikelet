# Pikelet 🥞

An implementation of a small dependently typed lambda calculus in Rust. This
project is intended as an explaratory exercise, and as a basis for more fully
featured dependently typed languages in Rust.

The type system was originally inspired by the one presented in [_A Tutorial
Implementation of a Dependently Typed Lambda Calculus_](lambdapi), but it has
since diverged from it in a number of ways, so perhaps a rename is in order!

[lambdapi]: https://www.andres-loeh.de/LambdaPi/

## Why dependent types?

You may have noticed in languages like Haskell that there seems to be an uncanny
similarity between the way one works with terms (ie. expressions), and the way
one works with types:

```hs
id :: forall a. a -> a
id x = x

test = id 1

type Id (a :: Type) = a
type Test = Id Int
```

Dependent type systems elevate types to being _first class_ entities in a
programming language by eliminating the distinction between terms and types,
putting them all in the same syntax. This means that we can reuse the same
programming techniques both at the type and value level:

```idris
id : {a : Type} -> a -> a
id x = x

test1 = id 1 : Int
test2 = id Int : Type
```

Notice that type aliases just become regular old value definitions!

## What is a Pikelet?

An odd sort of small (often pre-made) crumpet or pancake found in Australia and
New Zealand. Often sent in school lunches spread with jam and butter. Also has
a name that includes 'pi' and 'let' as substrings! 😅

## Roadmap

- [ ] dependent record types
- [ ] implement module imports
- [ ] `let` and `where` bindings
- [ ] layout based syntax
- [ ] improve UX
  - [ ] pretty error messages
    - [ ] underlined source code snippets
    - [ ] type diffs
  - [ ] prettier pretty printing
    - [ ] rename ugly genvars!
    - [ ] use non-dependant arrows when possible
- [ ] universe hierarchy
  - [x] stratified - _this is actually very annoying to use_
  - [ ] cumulative?
    - [twitter thread](https://twitter.com/brendanzab/status/962681577120587776)
    - [blog post by Conor McBride](https://pigworker.wordpress.com/2015/01/09/universe-hierarchies/)
- [ ] more fully featured elaborator
  - [ ] holes, ie. `_` and `?hole`
  - [ ] implicit arguments, ie. `{a : Type}`
  - [ ] instance arguments, ie. `[eq : EQ a]`
- [ ] clean up and document internals
  - [ ] document challenges with name binding
  - [ ] ensure substitution points match the typing judgements
  - [ ] fix typechecking at the module level
  - [ ] settle on a logging strategy for judgements
  - [ ] automate name binding boilerplate to get it out of `syntax::core`
  - [ ] review `core::{Term, Value}`
    - What about `CheckedTerm`s? `ElaboratedTerm`s?
    - Why is there an `Option<Value>` on `Value::Lam`?
  - [ ] performance tuning, and profiling
  - [ ] property based testing - see “Effect-Driven QuickChecking of Compilers”
        [[VIDEO](https://www.youtube.com/watch?v=_KrZzaShDew)]
        [[PAPER](http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf)]

## References

- Charguéraud, Arthur (2011). “The Locally Nameless Representation”.
  In _Journal of Automated Reasoning (JAR)_.
  [[SITE]](http://www.chargueraud.org/softs/ln/)
  [[PAPER]](http://www.chargueraud.org/research/2009/ln/main.pdf)
- Christiansen, David Raymond (2013). “Bidirectional Typing Rules: A Tutorial”.
  [[PAPER]](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
- Löh, Andres, McBride, Conor and Swierstra, Wouter (2009). “A tutorial
  implementation of a dependently typed lambda calculus”.
  [[SITE]](https://www.andres-loeh.de/LambdaPi/)
  [[PAPER]](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
- Norell, Ulf (2007). “Towards a practical programming language based on
  dependent type theory”.
  [[PAPER]](http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf)

## Acknowledgments

[![YesLogic Logo][yeslogic-logo]][yeslogic]

This work was done with the generous support of [YesLogic][yeslogic].

[yeslogic]: http://yeslogic.com/
[yeslogic-logo]: assets/yeslogic-logo.png
