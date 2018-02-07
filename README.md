# LambdaPi

A Rust implementation of [_A Tutorial Implementation of a Dependently Typed
Lambda Calculus_](lambdapi).

[lambdapi]: https://www.andres-loeh.de/LambdaPi/

To run the REPL:

```
$ cargo run --features=repl
λΠ> (\(a : Type) (x : a) => x) Type (Type -> Type)
($9 : Type) -> Type : Type
λΠ>
```

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
