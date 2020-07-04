---
id: names
title: Names (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

### Identifiers

Names refer to declarations that are currently in scope.

These could either be _global_, or _local_.

```pikelet
make-string
Foo-23
Unicode-String
```

:::note
See [Surface language - Lexical syntax - Keywords and identifiers](../specification/surface/lexical-structure#keywords-and-identifiers): <a href="../specification/surface/lexical-structure#var:ident"><var>ident</var></a>
:::

### Keywords

Keywords use the same syntax as [identifiers](#identifiers), but are reserved by Pikelet.

The following keywords are reserved by Pikelet:

| Keyword | Purpose |
| ------- | ------- |
| `as` | [Explicit binding names](./records#Explicit-binding-names) |
| `fun` | [Function terms](./functions#Terms) |
| `Record` | [Record types](./records#Types) |
| `record` | [Record terms](./records#Terms) |

:::note
See [Surface language - Lexical syntax - Keywords and identifiers](../specification/surface/lexical-structure#keywords-and-identifiers): <a href="../specification/surface/lexical-structure#var:keyword"><var>keyword</var></a>
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
