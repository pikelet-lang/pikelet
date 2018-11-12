---
title: Monthly Update, February 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

My little dependently typed experiment is now called [Pikelet](https://github.com/brendanzab/pikelet)! Come say hello on [Gitter](https://gitter.im/pikelet-lang/Lobby) if you like! Fixed an annoying and hard to figure out variable scoping bug that turned out to have been because I was being lazy and not generating fresh variable names when I moved things into the context while type checking.

<!--truncate-->

I've made a library called [codespan](https://github.com/brendanzab/codespan) to share some of the annoying source location tracking and error diagnostic code between languages. Although the pretty error messages are not yet implemented yet. Want to see what I can salvage from Rust's own diagnostics. Potentially hook it up to the language server protocol too! That would be neat.

Next for Pikelet will probably be dependent record types, as that shouldn't be too hard and I need them for my DDL as well. Also [been experimenting](https://play.rust-lang.org/?gist=a563a8e617a379d8cee632a618bc8b9f&version=nightly) with “[Trees that Grow](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf)” in Rust. Also want to see if I can make an auto-binding library thing (like Bound or Unbound for Haskell) to get rid of some of my icky nameplate. Also still have a bug in how I bind my type signatures in modules that means I can't load the prelude, so I should fix that -_-#.
