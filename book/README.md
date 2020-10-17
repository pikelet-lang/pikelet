# Pikelet Book

To build the book, you will first need to [install mdBook][install-mdbook] and [mdbook-linkcheck]:

```sh
cargo install mdbook mdbook-linkcheck
```

Note that for consistency we use specific versions of these tools on CI,
so the one you install might be newer than the one used to build and deploy the book.
To check the versions we currently assume, look at the [workflows directory](../.github/workflows).

## Building additional JavaScript

In order to highlight the Fathom code examples in the book we override mdBook's built-in [highlight.js] with our own.
To build the highlighting code, run the following commands using [Yarn]:

```sh
yarn workspace book install
yarn workspace book build
```

You will need to rebuild the book or restart the mdBook server for changes to take effect.

[highlight.js]: https://highlightjs.org/
[Yarn]: (https://yarnpkg.com/)

## Running the mdBook server

You can then serve the documentation locally by calling the [`serve` command][mdbook-serve]
from the `book` directory:

```sh
mdbook serve
```

Alternatively it can be called from the root of the repository:

```sh
mdbook serve book
```

[install-mdbook]: https://rust-lang.github.io/mdBook/cli/index.html#install-cratesio-version
[mdbook-serve]: https://rust-lang.github.io/mdBook/cli/serve.html
[mdbook-linkcheck]: https://github.com/Michael-F-Bryan/mdbook-linkcheck#getting-started
