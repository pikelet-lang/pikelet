---
id: names
title: Names (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

### Keywords

The following keywords are reserved by Pikelet:

- `Record`
- `record`

:::note
See [Surface language - Lexical syntax - Keywords and identifiers](../specification/surface/lexical-structure#keywords-and-identifiers): <a href="../specification/surface/lexical-structure#var:keyword"><var>keyword</var></a>
:::

### Identifiers

Names refer to declarations that are currently in scope.

```pikelet
make-string
Foo-23
Unicode-String
```

:::note
See [Surface language - Lexical syntax - Keywords and identifiers](../specification/surface/lexical-structure#keywords-and-identifiers): <a href="../specification/surface/lexical-structure#var:ident"><var>ident</var></a>
:::

### Conventions

#### Term names

Term names are in `lower-kebab-case`, for example:

```pikelet
binary-data
```

#### Type Names

Type names are in `Title-Kebab-Case`, for example:

```pikelet
Unicode-String
```
