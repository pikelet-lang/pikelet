---
title: Monthly Update, July 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

Continuing to plug away at [Pikelet](https://github.com/pikelet-lang/pikelet), my dependently typed systems language, and my binary data description language for work.

Last month I finally published my variable binding library, [Moniker](https://github.com/brendanzab/moniker), to [crates.io](https://crates.io/crates/moniker). It provides a nice way to describe binding forms in an AST. For example:

<!--truncate-->

```rust
#[macro_use]
extern crate moniker;

use moniker::{Embed, Binder, Scope, Var};
use std::rc::Rc;

#[derive(BoundTerm)]
pub enum Expr {
    Var(Var<String>),
    Lam(Scope<Binder<String>, RcExpr>),
    App(RcExpr, RcExpr),
}

pub type RcExpr = Rc<Expr>;
```

I tried to work hard on the README, to make it easier to understand what the library is for, and what problems it solves, but it still needs some more work! The breakthough that led to my confidence in publishing it was finally getting pattern matching to work nicely. You can see an example of it at work in the [`stlc_data`](https://github.com/brendanzab/moniker/blob/master/moniker/examples/stlc_data.rs) example. I still need to implement derives for a `Subst` trait though - currently this still needs to be implemented manually.

After this I used the updates on Moniker to add some limited support for pattern matching to Pikelet and the DDL, and did some internal cleanups in preparation for adding mutually recursive definitions.

I also did some looking at the [Web Assembly Spec](https://webassembly.github.io/spec/core/index.html) over the weekend. It's really well done, with [built-in typing rules](https://webassembly.github.io/spec/core/appendix/index-rules.html#index-rules) and everything. I think I'm going to redo Pikelet's spec in the spirit of this!
