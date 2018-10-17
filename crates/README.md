# The Pikelet Compiler

Welcome to the core implementation of the Pikelet compiler. We hope you enjoy
your stay!

## Crates

### Tools

| Name                        | Description                                     |
|-----------------------------|-------------------------------------------------|
| [`pikelet`]                 | Top-level command line interface                |
| [`pikelet-repl`]            | Interactive mode                                |
| [`pikelet-language-server`] | LSP conforming language server                  |

[`pikelet`]: /crates/pikelet
[`pikelet-repl`]: /crates/pikelet-repl
[`pikelet-language-server`]: /crates/pikelet-language-server

### Compiler

| Name                        | Description                                             |
|-----------------------------|---------------------------------------------------------|
| [`pikelet-driver`]          | Main entry-point for the compiler pipeline              |
| [`pikelet-library`]         | Builtin libraries                                       |
| [`pikelet-syntax`]          | Parsing, ASTs, pretty printing, and desugaring          |
| [`pikelet-elaborate`]       | Type checking, normalization, and elaboration of terms  |

[`pikelet-driver`]: /crates/pikelet-driver
[`pikelet-library`]: /crates/pikelet-library
[`pikelet-syntax`]: /crates/pikelet-syntax
[`pikelet-elaborate`]: /crates/pikelet-elaborate
