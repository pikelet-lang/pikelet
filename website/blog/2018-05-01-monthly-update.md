---
title: Monthly Update, May 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

This month was more of a quiet one for [Pikelet](https://github.com/brendanzab/pikelet), mainly due to me being off at a LambdaJam (a functional programming conference in Sydney).

- [added primitive functions](https://github.com/brendanzab/pikelet/pull/64), which were a little tricky due to currying.
- [added array syntax](https://github.com/brendanzab/pikelet/commit/d5cf893518d1d396aa55a2b22971970ce85473bf) and [hooked them up to the type checker and normalizer](https://github.com/brendanzab/pikelet/pull/72).
- [added a`:core <term>` command](https://github.com/brendanzab/pikelet/pull/68) that prints out the core representation after elaboration and type checking

<!--truncate-->

There were also [some contributions](https://github.com/brendanzab/pikelet/pull/70) that [fixed up](https://github.com/brendanzab/pikelet/pull/65) a bunch of grammar and spelling errors that were most appreciated!

As always, lots of interesting discussions [on the Gitter channel](https://gitter.im/pikelet-lang/Lobby), including:

- [effects and handlers and using partial evaluation to remove the runtime overhead](https://gitter.im/pikelet-lang/Lobby?at=5afd821e52194a4a67f17b44)
- [compiling to combinatory logic](https://gitter.im/pikelet-lang/Lobby?at=5b07842b160c567d16d6a6d3)
- [questioning whether I should use visitors internally](https://gitter.im/pikelet-lang/Lobby?at=5b0b52bac712f56125393c82)
- [putting composable external iterators in category theory](https://gitter.im/pikelet-lang/Lobby?at=5b0ccceec712f561253dcb56)
- [the lexical syntax of comments](https://gitter.im/pikelet-lang/Lobby?at=5b0fc682c712f56125485b3d)
- ...and more!
