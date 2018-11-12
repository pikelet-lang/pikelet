---
title: Monthly Update, June 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

My language, [Pikelet](https://github.com/pikelet-lang/pikelet), hopes to one day be a dependently typed systems programming language!

This month I moved Pikelet to [its very own Github organisation](https://github.com/pikelet-lang). I also pulled out some of the span information from the core syntax. There were some nice updates contributed from my mate Adrian who added some support for goldenfile tests in the compiler, which was lovely! A number of folks were having trouble getting their heads around how the front end of the compiler worked, I began [a dedicated page in the book for this](https://pikelet-lang.github.io/pikelet/appendix/implementation.html).

<!--truncate-->

Some work has been done on [my autobinding library](https://github.com/brendanzab/moniker), renaming it (hah) from 'nameless' to 'moniker'. I added [support for recursion](https://github.com/brendanzab/moniker/blob/e325e701aed8fd5a6dfbcf3eae4a13d6a1255ca5/moniker/examples/lc_letrec.rs#L19-L20), and have been learning about how to implement isorecursive and equirecursive types from TaPL, as [can be seen from my twitter posts](https://twitter.com/brendanzab/status/1013641820645945344). I found [this part](https://twitter.com/brendanzab/status/1012677041425039360) more amusing than I probably should have!

I've been looking at how I might make more steps towards actually compiling Pikelet code, namely looking into [A-Normal Form](https://en.wikipedia.org/wiki/A-normal_form) as my next intermediate representation. To that end I've updated [the issue on compilation](https://github.com/pikelet-lang/pikelet/issues/9) and [created a new issue](https://github.com/pikelet-lang/pikelet/issues/91). There are so many other things to do though - implicit arguments, instance arguments, imports, working on an editor plugin, reworking the compiler to be more query-driven... it can be quite challenging to focus!

In theoretical news, Robert Atkey and his student James Wood have [posted an abstract on 'Context Constrained Computing'](https://twitter.com/bentnib/status/1007238241488162817). This combines Atkey and McBride's work on [quantitative type theory](https://bentnib.org/quantitative-type-theory.html) (which includes linearity and erasure) with other styles of coeffect tracking. This excites me, because it might also allow me to do separate compilation and compile-time partial evaluation in a more principled way! I'm looking on with interest. :)
