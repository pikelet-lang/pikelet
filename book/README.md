# Pikelet Language Book

To build the book, you will first need to [install mdBook via cargo][install-mdbook]:

```sh
cargo install mdbook
```

You can then serve the documentation locally by calling the [`serve` command][mdbook-serve]
from the `book` directory:

```sh
mdbook serve
```

[install-mdbook]: https://rust-lang-nursery.github.io/mdBook/cli/cli-tool.html#install-cratesio-version
[mdbook-serve]: https://rust-lang-nursery.github.io/mdBook/cli/serve.html

## Building custom syntax highlighting

Highlight.js can be [hard to extend][mdbook-custom-highlighting-issue], so we've
had to make a [custom fork][highlightjs-fork] that supports Pikelet syntax. For
better or worse, we've included this as a submodule as a temporary solution.
To build this, we've included a handy script:

```sh
tools/build-highlight-js
```

This should update/initialize the submodule, update the npm dependencies,
and copy the minified highlighting source to the proper directory. Note that
this is an optional step for developing locally - the CI pipeline will take care
of building and deploying this automatically.

[mdbook-custom-highlighting-issue]: https://github.com/rust-lang-nursery/mdBook/issues/657
[highlightjs-fork]: https://github.com/pikelet-lang/highlight.js/tree/add-pikelet
