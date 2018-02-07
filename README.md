# LambdaPi

A Rust implementation of [_A Tutorial Implementation of a Dependently Typed
Lambda Calculus_](https://www.andres-loeh.de/LambdaPi/).

## Todo

- [ ] fix typechecking at the module level
- [ ] ensure substitution points match the typing judgements
- [ ] document challenges with name binding
- [ ] should elaboration normalize as well?
- [ ] performance tuning, and profiling
- [ ] settle on a logging strategy for judgements
- [ ] Agda-style pi type suger: `(A : Type) (C : A -> Type) (x y : A) -> Eq A x y -> C x -> C y`
- [ ] automate name binding boilerplate to get it out of `syntax::core`
- [ ] have prettier, more useful errors than relying on `fmt::Debug`
- [ ] implement universe levels
- [ ] implement holes
- [ ] review `core::{Term, Value}`
  - What about `CheckedTerm`s? `ElaboratedTerm`s?
  - Why is there an `Option<Value>` on `Value::Lam`?

## References

- Charguéraud, Arthur (2011). “The Locally Nameless Representation”.
  In _Journal of Automated Reasoning (JAR)_.
  [[URL]](http://www.chargueraud.org/softs/ln/)
  [[PDF]](http://www.chargueraud.org/research/2009/ln/main.pdf)
- Christiansen, David Raymond (2013). “Bidirectional Typing Rules: A Tutorial”.
  [[PDF]](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
- Löh, Andres, McBride, Conor and Swierstra, Wouter (2009). “A tutorial
  implementation of a dependently typed lambda calculus”.
  [[URL]](https://www.andres-loeh.de/LambdaPi/)
  [[PDF]](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
- Norell, Ulf (2007). “Towards a practical programming language based on
  dependent type theory”.
  [[PDF]](http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf)
