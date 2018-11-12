---
title: Monthly Update, October 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

Pikelet is a dependently type language that I hope to grow into a more powerful, yet simple systems language with unboxed data types.

This month I added record field puns. For example:

```pikelet
let
    data = "hello"
in
    record { data }
```

desugars to:

<!--truncate-->

```pikelet
let
    data = "hello"
in
    record { data = data }
```

I also split up my project into a bunch of sub-crates. This will make embedding pikelet in other applications easier, because you only need to take what you need! In addition I also started a VS Code extension, trying to figure out what that will look like. I'm hoping that I can use [salsa](https://github.com/salsa-rs/salsa/) or [adapton](http://adapton.org/) for incremental analysis/evaluation/compilation.

I'm looking at how I can embed Pikelet in applications, including games. While it will be a while before Pikelet can be used to build entire applications (you need a full library ecosystem for that), being able to embed it as a scripting language will provide me with more immediate feedback on how well it feels to use as an actual language.
