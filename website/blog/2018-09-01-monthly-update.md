---
title: Monthly Update, September 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

Ugh, so I missed last month's update, but not for lack of work!

[Pikelet](https://github.com/pikelet-lang/pikelet), my dependently typed systems language now has a [goofy macot](https://github.com/pikelet-lang/pikelet#readme)!

This month I had some new contributors submit some PRs, which was really cool:

<!--truncate-->

- [boomshroom](https://github.com/boomshroom) added support for let bindings and fixed a panic on mismatched record fields. They've been also playing around with doing some category theory in Pikelet too!
- [heyrutvik](https://github.com/heyrutvik) renamed the signed integer types from `I8`, `I16`, `I32`, `I64` to `S8`, `S16`, `S32`, `S64`, and has been learning about dependent type systems by [porting David Christiansen's NBE tutorial to scala](https://github.com/heyrutvik/nbe-a-tutorial).

On my end, I've finally [fixed the pretty printer](https://github.com/pikelet-lang/pikelet/pull/132) do I don't output ugly internal ids. This makes the repl much nicer to use! I'd also removed the top-level module items - now you [just use records](https://github.com/pikelet-lang/pikelet/pull/149)! This is more in the spirit of 1ML. So this:

```pikelet
id : (A : Type) -> A -> A;
id A x = x;
```

Becomes:

```pikelet
record {
    id = id;
} where {
    id : (A : Type) -> A -> A;
    id A x = x;
}
```

I also added universe shifting/hoisting/lifting (still haven't decided [what to call it](https://github.com/pikelet-lang/pikelet/issues/140)). The idea is to allow you to reuse functions and types at different levels in the universe hierarchy. Eg.

```pikelet
id String "hello"
id^1 Type String
id^2 Type^1 Type
```

I have [some docs here](https://pikelet-lang.github.io/pikelet/language/universes.html), but my description could do with some improvement. The approach I took was very simple, compared to other dependently typed languages. Conor McBride talks about it in [a blog post](https://pigworker.wordpress.com/2015/01/09/universe-hierarchies/), but it lacked a reference implementation, and took a ton of puzzling over it before I [figured out](https://github.com/pikelet-lang/pikelet/pull/125) what he was actually describing. Thanks goes to Jon Sterling for [helping me out](https://github.com/pikelet-lang/pikelet/issues/10)! The algorithm is [described in the theory appendix](https://pikelet-lang.github.io/pikelet/appendix/theory.html#universe-shifting) of the Pikelet docs, if you are interested.

I continue to [mull over implicit arguments](https://github.com/pikelet-lang/pikelet/issues/76). There are two interesting approaches that I could use (referenced in the linked issue), _Let Arguments Go First_, and _Spine Local Type Inference_. The paper that describes the latter uses notation that is _really_ tricky to get my head around, but it seems promising.

