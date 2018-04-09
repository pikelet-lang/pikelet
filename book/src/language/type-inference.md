# Type inference

Many statically typed languages perform type inference, to varying degrees, and
Pikelet is no different! The goal is to reduce the burden of writing type
annotations everywhere. Some languages like [OCaml](https://ocaml.org/) and
[Elm](http://elm-lang.org/) can even infer the types of a whole program without
any annotations at all!

Pikelet's type inference follows some very simple rules that you can probably
pick up on your own, but we thought it might help to give a deeper explanation
of how it works, without getting to bogged down in the theoretical details.

## Contents

- [Bidirectional type checking](#bidirectional-typechecking)
    - [Inferrable terms](#inferrable-terms)
    - [Checkable terms](#checkable-terms)
- [Further reading](#further-reading)

## Bidirectional type checking

Pikelet has a rather flexible type system that can have expressions embedded in
them, so we've opted to use an algorithm known as 'bidirectional type checking'
as a way to get a decent amount of inference while still remaining relatively
predictable to you, the programmer. This means that you may sometimes have to
write annotations on top-level definitions, but the types should propagate
downwards and inner definitions should not require much annotation at all.

To do this we break the terms of the language into two groups. We call these
[_inferrable terms_](#inferrable-terms) and [_checkable terms_](#checkable-terms).

### Inferrable terms

Inferrable terms can be checked on their own or based on previous definitions.

> TODO: Explain examples

```pikelet-repl
Pikelet> "1"
Pikelet> 'a'
Pikelet> \x : Int => x
Pikelet> (a : Type) -> a
```

### Checkable terms

Checkable terms need extra annotations, or be used in a position where extra
information can be supplied.

> TODO: Explain examples

```pikelet-repl
Pikelet> 1
Pikelet> 2.0
Pikelet> \x => x
```

```pikelet-repl
Pikelet> 1 : I32
Pikelet> 2.0 : F32
Pikelet> \x => x : I32 -> I32
```


## Further reading

We describe Pikelet's type checking algorithm more formally
[in the appendix](../appendix/theory.md). If you have a background in progamming
languages and type theory this might be of interest to you. If not, that's
ok - understanding the formal notation is not neccessary for developing a high
level intuition of type inference in Pikelet.
