---
title: Monthly Update, January 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

Still poodling along on my toy dependent type system: https://github.com/brendanzab/lambdapi. Please don't repost - it's still not ready to show more widely! Will have to rename it soon, seeing as it has drifted far enough away from the original LambdaPi paper that it could be its own thing!

<!--truncate-->

Made significant progress this week, fixing some long-stanging misunderstanding I had when implementing locally nameless variable binding. I still have one outlying variable binding bug that means I have a test failure when typechecking the prelude, but that should be an easy fix. Next I'm going to be adding a simple module system, and better error messages. Then it will be onto backporting the stuff I've learned to the DDL I've been working on!
