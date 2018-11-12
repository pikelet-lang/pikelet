---
title: Monthly Update, April 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

News in [Pikelet](https://github.com/brendanzab/pikelet) land:

Last month I worked on records. Had been trying to add them all in one go, but that proved a bit much for my little mind to handle, so instead I started by [adding non-dependent records](https://github.com/brendanzab/pikelet/pull/45), which weren't too tricky. This meant I could finally write:

```pikelet
||| Module for defining equality between two terms
Eq (a : Type) = Record {
    ||| Compare two terms for equality
    eq : a -> a -> Bool,
};
```

<!--truncate-->

After I was done with the simpler form of the problem I then [finally got dependent records working](https://github.com/brendanzab/pikelet/pull/47)! This took a bit of figuring out - I was having issues with projections, substitutions would escape weirdly. I thought it would be a really subtle, easily overlooked gap in my understanding of normalization and closures, but it turned out that I was just applying the substitution to the wrong variable in my Rust code (ie. `ty` vs `field_ty`)! I did learn a ton about weak head normal forms, normal forms, and explicit substitutions along the way though, and this is something I might able to [make use of](https://github.com/brendanzab/pikelet/issues/62) in the future.

But yes, I can now write:

```pikelet
Set : Type = Record {
    t : Type,
    elem : Type,
    insert : elem -> t -> t,
    count : t -> U64,
};
```

And so I am happy! I achieved the goal in [my last post](https://www.reddit.com/r/ProgrammingLanguages/comments/88stz8/monthly_what_are_you_working_on_how_is_it_coming/dwnqagp/)! :D

I have [many more things I want to do with records](https://github.com/brendanzab/pikelet/issues/2), but they will have to wait till the future. There are also limitations and problems - like I don't check for duplicate fields, and field order matters when checking if two record types are equivalent.

Here's a grab bag of other stuff I did in the background:

- added syntax highlighting to the book
- added [a page of language influences](https://brendanzab.github.io/pikelet/appendix/influences.html) to the book
- added [a page with some design notes and an implementation plan](https://brendanzab.github.io/pikelet/appendix/design.html) to the book
- switched the pretty printing of core terms to use s-expressions (for simplicity)
- added a `:let <ident> = <term>` command to the REPL
- added booleans
- added [if-then-else expressions](https://brendanzab.github.io/pikelet/language/conditionals.html#if-then-else-expressions)
- a bunch of assorted cleanups along the way to keep the code reading nicely

I've also been [working on case expressions on a separate branch](https://github.com/brendanzab/pikelet/pull/52) in the background. These will be simple at first - just matching on literals for now.

Next month I want to add some more builtin functions for working with strings and integers, and perhaps some inbuilt support for arrays. I also want to finish implementing let bindings, and finally fix the issue of the pretty printer outputting ugly variable numbers all the time. As always, come [chat with us on the Gitter channel](https://gitter.im/pikelet-lang/Lobby) - we are friendly!
